//! Comprehensive type checker tests covering edge cases, error paths,
//! and all major language constructs.
//!
//! Kraken syntax notes:
//! - Struct fields use semicolons: `name: string;`
//! - Match arms use `->` not `=>`
//! - Variables are reassignable by default (no `let mut`)
//! - For loops: `for (let i = 0; i < n; i = i + 1) { }`
//! - While loops: `while (cond) { }`

#[cfg(test)]
mod tests {
    use crate::analyzer::type_checker::TypeChecker;
    use crate::error::CompilerResult;
    use crate::lexer::tokenizer::Tokenizer;
    use crate::parser::parser::Parser;
    use std::path::PathBuf;

    fn type_check(source: &str) -> CompilerResult<()> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.kr"));
        let program = parser.parse()?;
        let mut checker = TypeChecker::new(PathBuf::from("test.kr"));
        checker.check_program(&program)
    }

    fn assert_ok(source: &str) {
        let result = type_check(source);
        assert!(
            result.is_ok(),
            "Expected OK for: {source}\nGot error: {}",
            result.unwrap_err()
        );
    }

    fn assert_err(source: &str) {
        assert!(type_check(source).is_err(), "Expected error for: {source}");
    }

    // ─── Variable Declarations ─────────────────────────────────────

    #[test]
    fn test_var_int_literal() {
        assert_ok("let x: int = 42;");
    }

    #[test]
    fn test_var_float_literal() {
        assert_ok("let x: float = 1.5;");
    }

    #[test]
    fn test_var_bool_literal() {
        assert_ok("let x: bool = true;");
    }

    #[test]
    fn test_var_string_literal() {
        assert_ok(r#"let x: string = "hello";"#);
    }

    #[test]
    fn test_var_type_inference_int() {
        assert_ok("let x = 42;");
    }

    #[test]
    fn test_var_type_inference_float() {
        assert_ok("let x = 3.5;");
    }

    #[test]
    fn test_var_type_inference_bool() {
        assert_ok("let x = false;");
    }

    #[test]
    fn test_var_type_inference_string() {
        assert_ok(r#"let x = "world";"#);
    }

    #[test]
    fn test_var_type_mismatch_int_float() {
        assert_err("let x: int = 3.14;");
    }

    #[test]
    fn test_var_type_mismatch_int_string() {
        assert_err(r#"let x: int = "hello";"#);
    }

    #[test]
    fn test_var_type_mismatch_bool_int() {
        assert_err("let x: bool = 42;");
    }

    #[test]
    fn test_var_type_mismatch_string_int() {
        assert_err("let x: string = 42;");
    }

    #[test]
    fn test_var_shadowing() {
        assert_ok(r#"
            let x = 42;
            let x = "hello";
        "#);
    }

    #[test]
    fn test_var_reassignment() {
        assert_ok(r#"
            let x = 42;
            x = 100;
        "#);
    }

    #[test]
    fn test_var_uninitialized_with_type() {
        assert_ok("let x: int;");
    }

    // ─── Constants ─────────────────────────────────────────────────

    #[test]
    fn test_const_int() {
        assert_ok("const MAX = 100;");
    }

    #[test]
    fn test_const_string() {
        assert_ok(r#"const NAME = "kraken";"#);
    }

    // ─── Function Declarations ─────────────────────────────────────

    #[test]
    fn test_fn_no_params_no_return() {
        assert_ok("fn greet() { }");
    }

    #[test]
    fn test_fn_with_params() {
        assert_ok("fn add(a: int, b: int) -> int { return a + b; }");
    }

    #[test]
    fn test_fn_return_type_mismatch() {
        assert_err(r#"fn bad() -> int { return "hello"; }"#);
    }

    #[test]
    fn test_fn_call_correct_args() {
        assert_ok(r#"
            fn add(a: int, b: int) -> int { return a + b; }
            let result = add(1, 2);
        "#);
    }

    #[test]
    fn test_fn_call_wrong_arg_count() {
        assert_err(r#"
            fn add(a: int, b: int) -> int { return a + b; }
            let result = add(1);
        "#);
    }

    #[test]
    fn test_fn_call_wrong_arg_type() {
        assert_err(r#"
            fn add(a: int, b: int) -> int { return a + b; }
            let result = add(1, "two");
        "#);
    }

    #[test]
    fn test_fn_recursive() {
        assert_ok(r#"
            fn factorial(n: int) -> int {
                if (n <= 1) {
                    return 1;
                }
                return n * factorial(n - 1);
            }
        "#);
    }

    #[test]
    fn test_fn_forward_reference() {
        assert_ok(r#"
            fn caller() -> int { return callee(5); }
            fn callee(x: int) -> int { return x * 2; }
        "#);
    }

    #[test]
    fn test_fn_multiple_returns() {
        assert_ok(r#"
            fn my_abs(x: int) -> int {
                if (x < 0) {
                    return 0 - x;
                }
                return x;
            }
        "#);
    }

    // ─── Control Flow ──────────────────────────────────────────────

    #[test]
    fn test_if_bool_condition() {
        assert_ok(r#"
            let x = true;
            if (x) { let y = 1; }
        "#);
    }

    #[test]
    fn test_if_comparison_condition() {
        assert_ok(r#"
            let x = 5;
            if (x > 0) { let y = 1; }
        "#);
    }

    #[test]
    fn test_if_else() {
        assert_ok(r#"
            let x = 5;
            if (x > 0) {
                let y = 1;
            } else {
                let y = 0;
            }
        "#);
    }

    #[test]
    fn test_while_loop() {
        assert_ok(r#"
            let i = 0;
            while (i < 10) {
                i = i + 1;
            }
        "#);
    }

    #[test]
    fn test_for_loop() {
        assert_ok(r#"
            for (let i = 0; i < 10; i = i + 1) {
                let x = i;
            }
        "#);
    }

    #[test]
    fn test_break_in_loop() {
        assert_ok(r#"
            let i = 0;
            while (true) {
                if (i > 5) { break; }
                i = i + 1;
            }
        "#);
    }

    #[test]
    fn test_continue_in_loop() {
        assert_ok(r#"
            for (let i = 0; i < 10; i = i + 1) {
                if (i == 5) { continue; }
                let x = i;
            }
        "#);
    }

    // ─── Binary Operations ─────────────────────────────────────────

    #[test]
    fn test_arithmetic_int() {
        assert_ok("let x = 1 + 2 * 3 - 4 / 2;");
    }

    #[test]
    fn test_arithmetic_float() {
        assert_ok("let x = 1.0 + 2.5;");
    }

    #[test]
    fn test_comparison_int() {
        assert_ok("let x = 5 > 3;");
    }

    #[test]
    fn test_comparison_equality() {
        assert_ok("let x = 5 == 5;");
    }

    #[test]
    fn test_logical_and() {
        assert_ok("let x = true && false;");
    }

    #[test]
    fn test_logical_or() {
        assert_ok("let x = true || false;");
    }

    #[test]
    fn test_mixed_type_arithmetic_error() {
        assert_err(r#"let x = 42 + "hello";"#);
    }

    // ─── Unary Operations ──────────────────────────────────────────

    #[test]
    fn test_unary_negate_int() {
        assert_ok("let x = -42;");
    }

    #[test]
    fn test_unary_not_bool() {
        assert_ok("let x = !true;");
    }

    // ─── Struct Declarations ───────────────────────────────────────

    #[test]
    fn test_struct_declaration() {
        assert_ok(r#"
            struct Point {
                x: int;
                y: int;
            }
        "#);
    }

    #[test]
    fn test_struct_literal() {
        assert_ok(r#"
            struct Point { x: int; y: int; }
            let p = Point { x: 1, y: 2 };
        "#);
    }

    #[test]
    fn test_struct_field_access() {
        assert_ok(r#"
            struct Point { x: int; y: int; }
            let p = Point { x: 1, y: 2 };
            let x = p.x;
        "#);
    }

    #[test]
    fn test_struct_wrong_field_type() {
        assert_err(r#"
            struct Point { x: int; y: int; }
            let p = Point { x: "hello", y: 2 };
        "#);
    }

    #[test]
    fn test_struct_forward_reference() {
        assert_ok(r#"
            fn make_point() -> Point {
                return Point { x: 0, y: 0 };
            }
            struct Point { x: int; y: int; }
        "#);
    }

    // ─── Enum Declarations ─────────────────────────────────────────

    #[test]
    fn test_enum_declaration() {
        assert_ok(r#"
            enum Color {
                Red,
                Green,
                Blue
            }
        "#);
    }

    #[test]
    fn test_enum_variant_construction() {
        assert_ok(r#"
            enum Color { Red, Green, Blue }
            let c = Color::Red;
        "#);
    }

    #[test]
    fn test_enum_with_payload() {
        assert_ok(r#"
            enum Shape {
                Circle(float),
                Rectangle(float, float)
            }
            let s = Shape::Circle(5.0);
        "#);
    }

    #[test]
    fn test_enum_forward_reference() {
        assert_ok(r#"
            fn get_color() -> Color { return Color::Red; }
            enum Color { Red, Green, Blue }
        "#);
    }

    // ─── Match Expressions ─────────────────────────────────────────

    #[test]
    fn test_match_int() {
        assert_ok(r#"
            let x = 5;
            match (x) {
                1 -> { let y = 10; }
                5 -> { let y = 50; }
                _ -> { let y = 0; }
            }
        "#);
    }

    #[test]
    fn test_match_enum() {
        assert_ok(r#"
            enum Color { Red, Green, Blue }
            let c = Color::Red;
            match (c) {
                Color::Red -> { let x = 1; }
                Color::Green -> { let x = 2; }
                Color::Blue -> { let x = 3; }
            }
        "#);
    }

    // ─── Arrays ────────────────────────────────────────────────────

    #[test]
    fn test_array_literal() {
        assert_ok("let arr = [1, 2, 3];");
    }

    #[test]
    fn test_array_index() {
        assert_ok(r#"
            let arr = [1, 2, 3];
            let x = arr[0];
        "#);
    }

    #[test]
    fn test_array_mixed_types_error() {
        assert_err(r#"let arr = [1, "two", 3];"#);
    }

    // ─── Tuples ────────────────────────────────────────────────────

    #[test]
    fn test_tuple_literal() {
        assert_ok(r#"let t = (1, "hello", true);"#);
    }

    #[test]
    fn test_tuple_index() {
        assert_ok(r#"
            let t = (1, 2, 3);
            let x = t.0;
        "#);
    }

    // ─── Trait Declarations ────────────────────────────────────────

    #[test]
    fn test_trait_declaration_basic() {
        assert_ok(r#"
            trait Printable {
                fn print(name: string);
            }
        "#);
    }

    #[test]
    fn test_trait_with_return_type() {
        assert_ok(r#"
            trait Describable {
                fn describe() -> string;
            }
        "#);
    }

    #[test]
    fn test_trait_with_provided_method() {
        assert_ok(r#"
            trait Greetable {
                fn greet() -> string {
                    return "hello";
                }
            }
        "#);
    }

    #[test]
    fn test_trait_impl_basic() {
        assert_ok(r#"
            trait Printable {
                fn print();
            }
            struct Dog { name: string; }
            impl Printable for Dog {
                fn print() { }
            }
        "#);
    }

    #[test]
    fn test_trait_impl_missing_method() {
        assert_err(r#"
            trait Printable {
                fn print();
                fn display();
            }
            struct Dog { name: string; }
            impl Printable for Dog {
                fn print() { }
            }
        "#);
    }

    #[test]
    fn test_trait_impl_wrong_signature() {
        assert_err(r#"
            trait Printable {
                fn print() -> string;
            }
            struct Dog { name: string; }
            impl Printable for Dog {
                fn print() -> int { return 0; }
            }
        "#);
    }

    #[test]
    fn test_trait_impl_for_enum() {
        assert_ok(r#"
            trait Describable {
                fn describe() -> string;
            }
            enum Color { Red, Green, Blue }
            impl Describable for Color {
                fn describe() -> string {
                    return "a color";
                }
            }
        "#);
    }

    #[test]
    fn test_trait_duplicate_impl() {
        assert_err(r#"
            trait Printable {
                fn print();
            }
            struct Dog { name: string; }
            impl Printable for Dog {
                fn print() { }
            }
            impl Printable for Dog {
                fn print() { }
            }
        "#);
    }

    #[test]
    fn test_trait_nonexistent_trait() {
        assert_err(r#"
            struct Dog { name: string; }
            impl NonExistent for Dog {
                fn print() { }
            }
        "#);
    }

    // ─── Async Functions ───────────────────────────────────────────

    #[test]
    fn test_async_fn_declaration() {
        assert_ok(r#"
            async fn fetch() -> int {
                return 42;
            }
        "#);
    }

    // ─── Unsafe Blocks ─────────────────────────────────────────────

    #[test]
    fn test_unsafe_block() {
        assert_ok(r#"
            unsafe {
                let x = 42;
            }
        "#);
    }

    // ─── Defer Statement ───────────────────────────────────────────

    #[test]
    fn test_defer_statement() {
        assert_ok(r#"
            fn cleanup() { }
            fn main() {
                defer cleanup();
                let x = 42;
            }
        "#);
    }

    // ─── Undefined Variable/Function ───────────────────────────────

    #[test]
    fn test_undefined_variable() {
        assert_err("let x = y;");
    }

    #[test]
    fn test_undefined_function() {
        assert_err("let x = nonexistent(42);");
    }

    // ─── Nested Scoping ────────────────────────────────────────────

    #[test]
    fn test_nested_scope_access() {
        assert_ok(r#"
            let x = 42;
            if (true) {
                let y = x + 1;
            }
        "#);
    }

    #[test]
    fn test_scope_isolation() {
        assert_ok(r#"
            if (true) {
                let x = 42;
            }
        "#);
    }

    // ─── Complex Programs ──────────────────────────────────────────

    #[test]
    fn test_fibonacci() {
        assert_ok(r#"
            fn fib(n: int) -> int {
                if (n <= 1) {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
            let result = fib(10);
        "#);
    }

    #[test]
    fn test_struct_methods_via_impl() {
        assert_ok(r#"
            struct Counter { value: int; }
            impl Counter {
                fn new() -> Counter {
                    return Counter { value: 0 };
                }
                fn get_value() -> int {
                    return 42;
                }
            }
        "#);
    }

    #[test]
    fn test_multiple_structs() {
        assert_ok(r#"
            struct Point { x: int; y: int; }
            struct Size { w: int; h: int; }
            let p = Point { x: 0, y: 0 };
            let s = Size { w: 10, h: 10 };
        "#);
    }

    #[test]
    fn test_enum_match_with_payload() {
        assert_ok(r#"
            enum Option {
                Some(int),
                None
            }
            let val = Option::Some(42);
            match (val) {
                Option::Some(x) -> { let y = 1; }
                Option::None -> { let y = 0; }
            }
        "#);
    }

    #[test]
    fn test_deeply_nested_control_flow() {
        assert_ok(r#"
            fn deep(x: int) -> int {
                if (x > 0) {
                    if (x > 5) {
                        if (x > 10) {
                            return 3;
                        }
                        return 2;
                    }
                    return 1;
                }
                return 0;
            }
        "#);
    }

    #[test]
    fn test_for_in_range() {
        assert_ok(r#"
            let sum = 0;
            for (x in 0..10) {
                sum = sum + x;
            }
        "#);
    }

    #[test]
    fn test_type_alias() {
        assert_ok(r#"
            type MyInt = int;
        "#);
    }

    #[test]
    fn test_multiple_trait_impls_different_types() {
        assert_ok(r#"
            trait Printable {
                fn print();
            }
            struct Dog { name: string; }
            struct Cat { name: string; }
            impl Printable for Dog {
                fn print() { }
            }
            impl Printable for Cat {
                fn print() { }
            }
        "#);
    }

    #[test]
    fn test_multiple_traits_same_type() {
        assert_ok(r#"
            trait Printable {
                fn print();
            }
            trait Describable {
                fn describe() -> string;
            }
            struct Dog { name: string; }
            impl Printable for Dog {
                fn print() { }
            }
            impl Describable for Dog {
                fn describe() -> string {
                    return "a dog";
                }
            }
        "#);
    }

    #[test]
    fn test_empty_struct() {
        assert_ok("struct Empty { }");
    }

    #[test]
    fn test_empty_enum() {
        assert_ok("enum Nothing { }");
    }

    #[test]
    fn test_empty_function_body() {
        assert_ok("fn noop() { }");
    }

    #[test]
    fn test_chained_comparisons() {
        assert_ok(r#"
            let a = 1;
            let b = 2;
            let c = 3;
            let x = a < b && b < c;
        "#);
    }

    #[test]
    fn test_modulo_operator() {
        assert_ok("let x = 10 % 3;");
    }

    #[test]
    fn test_bitwise_and() {
        assert_ok("let x = 255 & 15;");
    }

    #[test]
    fn test_nested_arithmetic() {
        assert_ok("let x = (1 + 2) * (3 - 4);");
    }

    #[test]
    fn test_bitwise_xor() {
        assert_ok("let x = 255 ^ 255;");
    }
}
