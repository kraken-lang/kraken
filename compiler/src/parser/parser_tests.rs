//! Comprehensive parser tests covering all statement and expression types,
//! error recovery, malformed input, and edge cases.

#[cfg(test)]
mod tests {
    use crate::error::CompilerResult;
    use crate::lexer::token::Operator;
    use crate::lexer::tokenizer::Tokenizer;
    use crate::parser::ast::*;
    use crate::parser::parser::Parser;
    use std::path::PathBuf;

    fn parse(source: &str) -> CompilerResult<Program> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.kr"));
        parser.parse()
    }

    fn assert_parses(source: &str) {
        let result = parse(source);
        assert!(
            result.is_ok(),
            "Expected parse OK for: {source}\nGot error: {}",
            result.unwrap_err()
        );
    }

    fn assert_parse_err(source: &str) {
        assert!(
            parse(source).is_err(),
            "Expected parse error for: {source}"
        );
    }

    // ─── Variable Declarations ─────────────────────────────────────

    #[test]
    fn test_parse_let_simple() {
        assert_parses("let x = 42;");
    }

    #[test]
    fn test_parse_let_typed() {
        assert_parses("let x: int = 42;");
    }

    #[test]
    fn test_parse_let_uninitialized() {
        assert_parses("let x: int;");
    }

    #[test]
    fn test_parse_let_missing_semicolon() {
        assert_parse_err("let x = 42");
    }

    #[test]
    fn test_parse_let_missing_value() {
        // let x = ; should fail
        assert_parse_err("let x = ;");
    }

    // ─── Constant Declarations ─────────────────────────────────────

    #[test]
    fn test_parse_const() {
        assert_parses("const MAX = 100;");
    }

    #[test]
    fn test_parse_const_typed() {
        assert_parses("const MAX: int = 100;");
    }

    // ─── Function Declarations ─────────────────────────────────────

    #[test]
    fn test_parse_fn_empty() {
        assert_parses("fn noop() { }");
    }

    #[test]
    fn test_parse_fn_with_params() {
        assert_parses("fn add(a: int, b: int) -> int { return a + b; }");
    }

    #[test]
    fn test_parse_fn_no_return_type() {
        assert_parses("fn greet(name: string) { }");
    }

    #[test]
    fn test_parse_fn_async() {
        assert_parses("async fn fetch() -> int { return 0; }");
    }

    #[test]
    fn test_parse_fn_unsafe() {
        assert_parses("unsafe fn danger() { }");
    }

    #[test]
    fn test_parse_fn_public() {
        assert_parses("pub fn api() -> int { return 1; }");
    }

    #[test]
    fn test_parse_fn_generic() {
        assert_parses("fn identity<T>(x: T) -> T { return x; }");
    }

    #[test]
    fn test_parse_fn_missing_body() {
        assert_parse_err("fn broken()");
    }

    #[test]
    fn test_parse_fn_missing_paren() {
        assert_parse_err("fn broken { }");
    }

    // ─── Struct Declarations ───────────────────────────────────────

    #[test]
    fn test_parse_struct_empty() {
        assert_parses("struct Empty { }");
    }

    #[test]
    fn test_parse_struct_fields() {
        assert_parses("struct Point { x: int; y: int; }");
    }

    #[test]
    fn test_parse_struct_public() {
        assert_parses("pub struct Visible { data: int; }");
    }

    #[test]
    fn test_parse_struct_generic() {
        assert_parses("struct Pair<T> { first: T; second: T; }");
    }

    #[test]
    fn test_parse_struct_missing_brace() {
        assert_parse_err("struct Bad { x: int;");
    }

    // ─── Enum Declarations ─────────────────────────────────────────

    #[test]
    fn test_parse_enum_simple() {
        assert_parses("enum Color { Red, Green, Blue }");
    }

    #[test]
    fn test_parse_enum_trailing_comma() {
        assert_parses("enum Color { Red, Green, Blue, }");
    }

    #[test]
    fn test_parse_enum_with_payload() {
        assert_parses("enum Shape { Circle(float), Rect(float, float) }");
    }

    #[test]
    fn test_parse_enum_empty() {
        assert_parses("enum Nothing { }");
    }

    #[test]
    fn test_parse_enum_generic() {
        assert_parses("enum Option<T> { Some(T), None }");
    }

    // ─── Impl Blocks ──────────────────────────────────────────────

    #[test]
    fn test_parse_impl_block() {
        assert_parses(r#"
            struct Foo { x: int; }
            impl Foo {
                fn get_x() -> int { return 0; }
            }
        "#);
    }

    #[test]
    fn test_parse_trait_impl() {
        assert_parses(r#"
            trait Bar { fn bar(); }
            struct Foo { x: int; }
            impl Bar for Foo {
                fn bar() { }
            }
        "#);
    }

    // ─── Trait Declarations ────────────────────────────────────────

    #[test]
    fn test_parse_trait_empty() {
        assert_parses("trait Empty { }");
    }

    #[test]
    fn test_parse_trait_required_method() {
        assert_parses("trait Greet { fn greet() -> string; }");
    }

    #[test]
    fn test_parse_trait_provided_method() {
        assert_parses(r#"
            trait Greet {
                fn greet() -> string {
                    return "hello";
                }
            }
        "#);
    }

    #[test]
    fn test_parse_trait_with_super() {
        assert_parses("trait Child: Parent { fn child_method(); }");
    }

    #[test]
    fn test_parse_trait_associated_type() {
        assert_parses("trait Iterator { type Item; fn next() -> int; }");
    }

    #[test]
    fn test_parse_trait_async_method() {
        assert_parses("trait Fetcher { async fn fetch() -> int; }");
    }

    // ─── Control Flow ──────────────────────────────────────────────

    #[test]
    fn test_parse_if() {
        assert_parses("if (true) { let x = 1; }");
    }

    #[test]
    fn test_parse_if_else() {
        assert_parses("if (true) { let x = 1; } else { let x = 2; }");
    }

    #[test]
    fn test_parse_while() {
        assert_parses("while (true) { break; }");
    }

    #[test]
    fn test_parse_for_c_style() {
        assert_parses("for (let i = 0; i < 10; i = i + 1) { }");
    }

    #[test]
    fn test_parse_for_in() {
        assert_parses("for (x in 0..10) { }");
    }

    #[test]
    fn test_parse_match() {
        assert_parses(r#"
            match (x) {
                1 -> { }
                2 -> { }
                _ -> { }
            }
        "#);
    }

    #[test]
    fn test_parse_match_enum_pattern() {
        assert_parses(r#"
            match (val) {
                Color::Red -> { }
                Color::Green -> { }
                _ -> { }
            }
        "#);
    }

    #[test]
    fn test_parse_break() {
        assert_parses("while (true) { break; }");
    }

    #[test]
    fn test_parse_continue() {
        assert_parses("while (true) { continue; }");
    }

    #[test]
    fn test_parse_return() {
        assert_parses("fn f() -> int { return 42; }");
    }

    #[test]
    fn test_parse_return_void() {
        assert_parses("fn f() { return; }");
    }

    // ─── Expressions ───────────────────────────────────────────────

    #[test]
    fn test_parse_int_literal() {
        assert_parses("let x = 42;");
    }

    #[test]
    fn test_parse_float_literal() {
        assert_parses("let x = 3.14;");
    }

    #[test]
    fn test_parse_bool_literal() {
        assert_parses("let x = true;");
        assert_parses("let y = false;");
    }

    #[test]
    fn test_parse_string_literal() {
        assert_parses(r#"let x = "hello";"#);
    }

    #[test]
    fn test_parse_binary_arithmetic() {
        assert_parses("let x = 1 + 2 * 3 - 4 / 2;");
    }

    #[test]
    fn test_parse_binary_comparison() {
        assert_parses("let x = 1 < 2;");
        assert_parses("let x = 1 <= 2;");
        assert_parses("let x = 1 > 2;");
        assert_parses("let x = 1 >= 2;");
        assert_parses("let x = 1 == 2;");
        assert_parses("let x = 1 != 2;");
    }

    #[test]
    fn test_parse_binary_logical() {
        assert_parses("let x = true && false;");
        assert_parses("let x = true || false;");
    }

    #[test]
    fn test_parse_unary_negate() {
        assert_parses("let x = -42;");
    }

    #[test]
    fn test_parse_unary_not() {
        assert_parses("let x = !true;");
    }

    #[test]
    fn test_parse_parenthesized() {
        assert_parses("let x = (1 + 2) * 3;");
    }

    #[test]
    fn test_parse_nested_parens() {
        assert_parses("let x = ((1 + 2) * (3 - 4));");
    }

    #[test]
    fn test_parse_function_call() {
        assert_parses("let x = foo(1, 2, 3);");
    }

    #[test]
    fn test_parse_function_call_no_args() {
        assert_parses("let x = foo();");
    }

    #[test]
    fn test_parse_array_literal() {
        assert_parses("let arr = [1, 2, 3];");
    }

    #[test]
    fn test_parse_array_empty() {
        assert_parses("let arr = [];");
    }

    #[test]
    fn test_parse_array_index() {
        assert_parses("let x = arr[0];");
    }

    #[test]
    fn test_parse_member_access() {
        assert_parses("let x = obj.field;");
    }

    #[test]
    fn test_parse_chained_member_access() {
        assert_parses("let x = a.b.c;");
    }

    #[test]
    fn test_parse_tuple_literal() {
        assert_parses(r#"let t = (1, "hello", true);"#);
    }

    #[test]
    fn test_parse_tuple_index() {
        assert_parses("let x = t.0;");
    }

    #[test]
    fn test_parse_struct_literal() {
        assert_parses("struct P { x: int; y: int; } let p = P { x: 1, y: 2 };");
    }

    #[test]
    fn test_parse_enum_variant() {
        assert_parses("enum Color { Red, Green } let c = Color::Red;");
    }

    #[test]
    fn test_parse_enum_variant_with_payload() {
        assert_parses("enum Opt { Some(int), None } let v = Opt::Some(42);");
    }

    #[test]
    fn test_parse_assignment() {
        assert_parses("let x = 0; x = 42;");
    }

    #[test]
    fn test_parse_range() {
        assert_parses("for (i in 0..10) { }");
    }

    #[test]
    fn test_parse_range_inclusive() {
        assert_parses("for (i in 0..=10) { }");
    }

    // ─── Unsafe / Defer ────────────────────────────────────────────

    #[test]
    fn test_parse_unsafe_block() {
        assert_parses("unsafe { let x = 42; }");
    }

    #[test]
    fn test_parse_defer() {
        assert_parses("fn f() { defer cleanup(); }");
    }

    // ─── Type Alias ────────────────────────────────────────────────

    #[test]
    fn test_parse_type_alias() {
        assert_parses("type MyInt = int;");
    }

    #[test]
    fn test_parse_type_alias_generic() {
        assert_parses("type Pair<T> = T;");
    }

    // ─── Import / Module ───────────────────────────────────────────

    #[test]
    fn test_parse_import() {
        assert_parses("import std.io;");
    }

    #[test]
    fn test_parse_module() {
        assert_parses("module my.package;");
    }

    // ─── Closures ──────────────────────────────────────────────────

    #[test]
    fn test_parse_closure_expression_body() {
        assert_parses("let f = |x: int| x + 1;");
    }

    #[test]
    fn test_parse_closure_block_body() {
        assert_parses("let f = |x: int| { return x + 1; };");
    }

    #[test]
    fn test_parse_closure_one_param() {
        assert_parses("let f = |x: int| x * 2;");
    }

    // ─── Edge Cases ────────────────────────────────────────────────

    #[test]
    fn test_parse_empty_program() {
        let result = parse("");
        assert!(result.is_ok());
        assert_eq!(result.unwrap().statements.len(), 0);
    }

    #[test]
    fn test_parse_deeply_nested_if() {
        assert_parses(r#"
            fn deep() {
                if (true) {
                    if (true) {
                        if (true) {
                            let x = 1;
                        }
                    }
                }
            }
        "#);
    }

    #[test]
    fn test_parse_deeply_nested_expressions() {
        assert_parses("let x = ((((1 + 2))));");
    }

    #[test]
    fn test_parse_multiple_statements() {
        assert_parses(r#"
            let a = 1;
            let b = 2;
            let c = a + b;
        "#);
    }

    #[test]
    fn test_parse_complex_program() {
        assert_parses(r#"
            struct Point { x: int; y: int; }
            enum Color { Red, Green, Blue }
            trait Drawable { fn draw(); }
            fn main() {
                let p = Point { x: 0, y: 0 };
                let c = Color::Red;
                if (true) {
                    for (let i = 0; i < 10; i = i + 1) {
                        let val = i * 2;
                    }
                }
            }
        "#);
    }

    // ─── Malformed Input ───────────────────────────────────────────

    #[test]
    fn test_parse_unclosed_brace() {
        assert_parse_err("fn f() {");
    }

    #[test]
    fn test_parse_unclosed_paren() {
        assert_parse_err("let x = (1 + 2;");
    }

    #[test]
    fn test_parse_unclosed_bracket() {
        assert_parse_err("let x = [1, 2;");
    }

    #[test]
    fn test_parse_multiple_let_statements() {
        assert_parses("let x = 1; let y = 2; let z = 3;");
    }

    #[test]
    fn test_parse_unexpected_token() {
        assert_parse_err("let = 42;");
    }

    // ─── AST Structure Verification ────────────────────────────────

    #[test]
    fn test_ast_let_structure() {
        let program = parse("let x: int = 42;").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::VariableDeclaration {
                pattern,
                type_annotation,
                initializer,
                is_mutable,
            } => {
                assert_eq!(*pattern, Pattern::Identifier("x".to_string()));
                assert_eq!(*type_annotation, Some(Type::Int));
                assert!(initializer.is_some());
                assert!(!is_mutable);
            }
            other => panic!("Expected VariableDeclaration, got {other:?}"),
        }
    }

    #[test]
    fn test_ast_fn_structure() {
        let program = parse("fn add(a: int, b: int) -> int { return a + b; }").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                is_async,
                is_public,
                ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(parameters.len(), 2);
                assert_eq!(*return_type, Some(Type::Int));
                assert!(!is_async);
                assert!(!is_public);
            }
            other => panic!("Expected FunctionDeclaration, got {other:?}"),
        }
    }

    #[test]
    fn test_ast_struct_structure() {
        let program = parse("struct Point { x: int; y: int; }").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::StructDeclaration {
                name,
                fields,
                is_public,
                ..
            } => {
                assert_eq!(name, "Point");
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name, "x");
                assert_eq!(fields[1].name, "y");
                assert!(!is_public);
            }
            other => panic!("Expected StructDeclaration, got {other:?}"),
        }
    }

    #[test]
    fn test_ast_enum_structure() {
        let program = parse("enum Color { Red, Green, Blue }").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::EnumDeclaration {
                name, variants, ..
            } => {
                assert_eq!(name, "Color");
                assert_eq!(variants.len(), 3);
                assert_eq!(variants[0].0, "Red");
                assert_eq!(variants[1].0, "Green");
                assert_eq!(variants[2].0, "Blue");
            }
            other => panic!("Expected EnumDeclaration, got {other:?}"),
        }
    }

    #[test]
    fn test_ast_if_structure() {
        let program = parse("if (true) { let x = 1; } else { let y = 2; }").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert_eq!(*condition, Expression::BoolLiteral(true));
                assert_eq!(then_branch.statements.len(), 1);
                assert!(else_branch.is_some());
            }
            other => panic!("Expected If, got {other:?}"),
        }
    }

    #[test]
    fn test_ast_binary_precedence() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let program = parse("let x = 1 + 2 * 3;").unwrap();
        match &program.statements[0] {
            Statement::VariableDeclaration {
                initializer: Some(expr),
                ..
            } => match expr {
                Expression::Binary {
                    left,
                    operator,
                    right,
                } => {
                    assert_eq!(*operator, Operator::Plus);
                    assert!(matches!(**left, Expression::IntLiteral(1)));
                    // right should be 2 * 3
                    match &**right {
                        Expression::Binary {
                            operator: inner_op, ..
                        } => {
                            assert_eq!(*inner_op, Operator::Star);
                        }
                        other => panic!("Expected Binary for right, got {other:?}"),
                    }
                }
                other => panic!("Expected Binary, got {other:?}"),
            },
            other => panic!("Expected VariableDeclaration, got {other:?}"),
        }
    }
}
