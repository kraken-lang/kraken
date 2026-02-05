//! Comprehensive tests for the trait system.

#[cfg(test)]
mod tests {
    use crate::analyzer::type_checker::TypeChecker;
    use crate::error::CompilerResult;
    use crate::lexer::tokenizer::Tokenizer;
    use crate::parser::parser::Parser;
    use std::path::PathBuf;

    fn type_check_source(source: &str) -> CompilerResult<()> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.kr"));
        let program = parser.parse()?;
        let mut checker = TypeChecker::new(PathBuf::from("test.kr"));
        checker.check_program(&program)
    }

    // ─── Trait declaration tests ───────────────────────────────────

    #[test]
    fn test_trait_declaration_empty() {
        let source = r#"
            trait Empty {
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_declaration_required_method() {
        let source = r#"
            trait Greet {
                fn greet(name: string) -> string;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_declaration_multiple_methods() {
        let source = r#"
            trait Shape {
                fn area() -> float;
                fn perimeter() -> float;
                fn name() -> string;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_declaration_provided_method() {
        let source = r#"
            trait Describable {
                fn describe() -> string {
                    return "unknown";
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_declaration_mixed_required_and_provided() {
        let source = r#"
            trait Animal {
                fn name() -> string;
                fn speak() -> string;
                fn legs() -> int {
                    return 4;
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_declaration_with_generic_params() {
        let source = r#"
            trait Container {
                fn size() -> int;
                fn is_empty() -> bool;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    // ─── Trait implementation tests ────────────────────────────────

    #[test]
    fn test_trait_impl_basic() {
        let source = r#"
            struct Dog {
                name: string;
            }

            trait Speak {
                fn speak() -> string;
            }

            impl Speak for Dog {
                fn speak() -> string {
                    return "woof";
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_impl_multiple_methods() {
        let source = r#"
            struct Circle {
                radius: float;
            }

            trait Shape {
                fn area() -> float;
                fn name() -> string;
            }

            impl Shape for Circle {
                fn area() -> float {
                    return 3.14;
                }

                fn name() -> string {
                    return "circle";
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_impl_with_params() {
        let source = r#"
            struct Calculator {
                value: int;
            }

            trait Compute {
                fn compute(x: int, y: int) -> int;
            }

            impl Compute for Calculator {
                fn compute(x: int, y: int) -> int {
                    return x + y;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_ok(), "Failed: {:?}", result.err());
    }

    #[test]
    fn test_trait_impl_provided_method_not_required() {
        // If a trait method has a default body, it doesn't need to be implemented
        let source = r#"
            struct Foo {
                x: int;
            }

            trait WithDefault {
                fn required() -> int;
                fn optional() -> int {
                    return 0;
                }
            }

            impl WithDefault for Foo {
                fn required() -> int {
                    return 42;
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    // ─── Trait error tests ─────────────────────────────────────────

    #[test]
    fn test_trait_impl_missing_required_method() {
        let source = r#"
            struct Bar {
                x: int;
            }

            trait Required {
                fn must_implement() -> int;
                fn also_required() -> string;
            }

            impl Required for Bar {
                fn must_implement() -> int {
                    return 1;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(
            result.is_err(),
            "Should fail: missing required method 'also_required'"
        );
    }

    #[test]
    fn test_trait_impl_unknown_trait() {
        let source = r#"
            struct Baz {
                x: int;
            }

            impl NonExistent for Baz {
                fn foo() -> int {
                    return 1;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(
            result.is_err(),
            "Should fail: trait 'NonExistent' not found"
        );
    }

    #[test]
    fn test_trait_impl_unknown_type() {
        let source = r#"
            trait Known {
                fn foo() -> int;
            }

            impl Known for UnknownType {
                fn foo() -> int {
                    return 1;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_err(), "Should fail: type 'UnknownType' not found");
    }

    #[test]
    fn test_trait_impl_extra_method() {
        let source = r#"
            struct Widget {
                id: int;
            }

            trait Simple {
                fn get_id() -> int;
            }

            impl Simple for Widget {
                fn get_id() -> int {
                    return 1;
                }
                fn extra_method() -> int {
                    return 2;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(
            result.is_err(),
            "Should fail: 'extra_method' not part of trait"
        );
    }

    #[test]
    fn test_trait_impl_wrong_param_count() {
        let source = r#"
            struct Thing {
                x: int;
            }

            trait Processor {
                fn process(a: int, b: int) -> int;
            }

            impl Processor for Thing {
                fn process(a: int) -> int {
                    return a;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_err(), "Should fail: wrong parameter count");
    }

    #[test]
    fn test_trait_impl_wrong_return_type() {
        let source = r#"
            struct Item {
                x: int;
            }

            trait Stringify {
                fn to_str() -> string;
            }

            impl Stringify for Item {
                fn to_str() -> int {
                    return 42;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_err(), "Should fail: wrong return type");
    }

    #[test]
    fn test_trait_impl_wrong_param_type() {
        let source = r#"
            struct Adder {
                x: int;
            }

            trait Add {
                fn add(a: int, b: int) -> int;
            }

            impl Add for Adder {
                fn add(a: string, b: int) -> int {
                    return b;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_err(), "Should fail: wrong parameter type");
    }

    #[test]
    fn test_trait_impl_duplicate() {
        let source = r#"
            struct Dup {
                x: int;
            }

            trait Marker {
                fn mark() -> int;
            }

            impl Marker for Dup {
                fn mark() -> int {
                    return 1;
                }
            }

            impl Marker for Dup {
                fn mark() -> int {
                    return 2;
                }
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_err(), "Should fail: duplicate trait impl");
    }

    // ─── Super trait tests ─────────────────────────────────────────

    #[test]
    fn test_super_trait_exists() {
        let source = r#"
            trait Base {
                fn base_method() -> int;
            }

            trait Derived: Base {
                fn derived_method() -> string;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_super_trait_not_found() {
        let source = r#"
            trait Child: NonExistentParent {
                fn method() -> int;
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_err(), "Should fail: super trait not found");
    }

    // ─── Trait with associated types ───────────────────────────────

    #[test]
    fn test_trait_with_associated_type() {
        let source = r#"
            trait Iterator {
                type Item;
                fn next() -> int;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_trait_with_bounded_associated_type() {
        let source = r#"
            trait Display {
                fn display() -> string;
            }

            trait Printable {
                type Output: Display;
                fn print() -> string;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    // ─── Multiple trait impls for different types ──────────────────

    #[test]
    fn test_multiple_types_implement_same_trait() {
        let source = r#"
            struct Cat {
                name: string;
            }

            struct Dog {
                name: string;
            }

            trait Speak {
                fn speak() -> string;
            }

            impl Speak for Cat {
                fn speak() -> string {
                    return "meow";
                }
            }

            impl Speak for Dog {
                fn speak() -> string {
                    return "woof";
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_type_implements_multiple_traits() {
        let source = r#"
            struct Robot {
                id: int;
            }

            trait Named {
                fn name() -> string;
            }

            trait Powered {
                fn power_level() -> int;
            }

            impl Named for Robot {
                fn name() -> string {
                    return "robot";
                }
            }

            impl Powered for Robot {
                fn power_level() -> int {
                    return 100;
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    // ─── Trait with enum types ─────────────────────────────────────

    #[test]
    fn test_trait_impl_for_enum() {
        let source = r#"
            enum Color {
                Red,
                Green,
                Blue,
            }

            trait Describable {
                fn describe() -> string;
            }

            impl Describable for Color {
                fn describe() -> string {
                    return "a color";
                }
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    // ─── Duplicate trait declaration ───────────────────────────────

    #[test]
    fn test_duplicate_trait_declaration() {
        let source = r#"
            trait Dup {
                fn method() -> int;
            }

            trait Dup {
                fn other() -> string;
            }
        "#;
        let result = type_check_source(source);
        assert!(result.is_err(), "Should fail: duplicate trait declaration");
    }

    // ─── Async trait methods ───────────────────────────────────────

    #[test]
    fn test_async_trait_method() {
        let source = r#"
            trait AsyncFetcher {
                async fn fetch() -> string;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    // ─── Trait object type parsing ─────────────────────────────────

    #[test]
    fn test_dyn_trait_type() {
        let source = r#"
            trait Drawable {
                fn draw() -> int;
            }

            fn render(obj: dyn Drawable) -> int {
                return 0;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_dyn_trait_with_bounds() {
        let source = r#"
            trait Sendable {
                fn send() -> int;
            }

            fn dispatch(obj: dyn Sendable + Send + Sync) -> int {
                return 0;
            }
        "#;
        // This tests that the parser handles dyn Trait + Bound + Bound
        assert!(type_check_source(source).is_ok());
    }
}
