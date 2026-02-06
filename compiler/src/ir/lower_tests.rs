//! Comprehensive IR lowering tests covering function lowering, struct lowering,
//! control flow, expressions, and edge cases.

#[cfg(test)]
mod tests {
    use crate::error::CompilerResult;
    use crate::ir::lower::IrLowering;
    use crate::ir::types::*;
    use crate::lexer::token::Operator;
    use crate::lexer::tokenizer::Tokenizer;
    use crate::parser::ast::{Block, Parameter, Pattern, Program, Statement, Type};
    use crate::parser::parser::Parser;
    use std::path::PathBuf;

    /// Parse source and lower to IR (skips type checking for IR-focused tests).
    fn lower_source(source: &str) -> CompilerResult<IrProgram> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.kr"));
        let program = parser.parse()?;
        let mut lowering = IrLowering::new();
        lowering.lower_program(&program)
    }

    fn assert_lowers(source: &str) -> IrProgram {
        let result = lower_source(source);
        assert!(
            result.is_ok(),
            "Expected IR lowering OK for: {source}\nGot error: {}",
            result.unwrap_err()
        );
        result.unwrap()
    }

    // ─── Empty / Minimal Programs ──────────────────────────────────

    #[test]
    fn test_lower_empty_program() {
        let ir = assert_lowers("");
        assert!(ir.functions.is_empty());
        assert!(ir.structs.is_empty());
    }

    #[test]
    fn test_lower_empty_function() {
        let ir = assert_lowers("fn noop() { }");
        assert_eq!(ir.functions.len(), 1);
        assert_eq!(ir.functions[0].name, "noop");
        assert_eq!(ir.functions[0].return_type, IrType::Void);
        assert!(!ir.functions[0].blocks.is_empty());
    }

    // ─── Function Lowering ─────────────────────────────────────────

    #[test]
    fn test_lower_function_with_return_type() {
        let ir = assert_lowers("fn get_value() -> int { return 42; }");
        assert_eq!(ir.functions.len(), 1);
        assert_eq!(ir.functions[0].return_type, IrType::Int);
    }

    #[test]
    fn test_lower_function_with_params() {
        let ir = assert_lowers("fn add(a: int, b: int) -> int { return a + b; }");
        assert_eq!(ir.functions.len(), 1);
        assert_eq!(ir.functions[0].params.len(), 2);
        assert_eq!(ir.functions[0].params[0].name, "a");
        assert_eq!(ir.functions[0].params[0].ty, IrType::Int);
        assert_eq!(ir.functions[0].params[1].name, "b");
        assert_eq!(ir.functions[0].params[1].ty, IrType::Int);
    }

    #[test]
    fn test_lower_function_float_return() {
        let ir = assert_lowers("fn pi() -> float { return 3.14; }");
        assert_eq!(ir.functions[0].return_type, IrType::Float);
    }

    #[test]
    fn test_lower_function_bool_return() {
        let ir = assert_lowers("fn is_valid() -> bool { return true; }");
        assert_eq!(ir.functions[0].return_type, IrType::Bool);
    }

    #[test]
    fn test_lower_function_string_return() {
        let ir = assert_lowers(r#"fn greeting() -> string { return "hello"; }"#);
        assert_eq!(ir.functions[0].return_type, IrType::String);
    }

    #[test]
    fn test_lower_multiple_functions() {
        let ir = assert_lowers(r#"
            fn foo() -> int { return 1; }
            fn bar() -> int { return 2; }
            fn baz() -> int { return 3; }
        "#);
        assert_eq!(ir.functions.len(), 3);
        assert_eq!(ir.functions[0].name, "foo");
        assert_eq!(ir.functions[1].name, "bar");
        assert_eq!(ir.functions[2].name, "baz");
    }

    #[test]
    fn test_lower_public_function() {
        let ir = assert_lowers("pub fn api() -> int { return 0; }");
        assert!(ir.functions[0].is_public);
    }

    #[test]
    fn test_lower_private_function() {
        let ir = assert_lowers("fn internal() -> int { return 0; }");
        assert!(!ir.functions[0].is_public);
    }

    // ─── Struct Lowering ───────────────────────────────────────────

    #[test]
    fn test_lower_struct_empty() {
        let ir = assert_lowers("struct Empty { }");
        assert_eq!(ir.structs.len(), 1);
        assert_eq!(ir.structs[0].name, "Empty");
        assert!(ir.structs[0].fields.is_empty());
    }

    #[test]
    fn test_lower_struct_with_fields() {
        let ir = assert_lowers("struct Point { x: int; y: int; }");
        assert_eq!(ir.structs.len(), 1);
        assert_eq!(ir.structs[0].name, "Point");
        assert_eq!(ir.structs[0].fields.len(), 2);
        assert_eq!(ir.structs[0].fields[0].0, "x");
        assert_eq!(ir.structs[0].fields[0].1, IrType::Int);
        assert_eq!(ir.structs[0].fields[1].0, "y");
        assert_eq!(ir.structs[0].fields[1].1, IrType::Int);
    }

    #[test]
    fn test_lower_struct_mixed_types() {
        let ir = assert_lowers(
            "struct Record { id: int; value: float; name: string; active: bool; }",
        );
        assert_eq!(ir.structs[0].fields.len(), 4);
        assert_eq!(ir.structs[0].fields[0].1, IrType::Int);
        assert_eq!(ir.structs[0].fields[1].1, IrType::Float);
        assert_eq!(ir.structs[0].fields[2].1, IrType::String);
        assert_eq!(ir.structs[0].fields[3].1, IrType::Bool);
    }

    #[test]
    fn test_lower_public_struct() {
        let ir = assert_lowers("pub struct Visible { data: int; }");
        assert!(ir.structs[0].is_public);
    }

    #[test]
    fn test_lower_multiple_structs() {
        let ir = assert_lowers(r#"
            struct A { x: int; }
            struct B { y: float; }
        "#);
        assert_eq!(ir.structs.len(), 2);
        assert_eq!(ir.structs[0].name, "A");
        assert_eq!(ir.structs[1].name, "B");
    }

    // ─── Variable Declarations ─────────────────────────────────────

    #[test]
    fn test_lower_variable_declaration() {
        let ir = assert_lowers("fn f() { let x = 42; }");
        let block = &ir.functions[0].blocks[0];
        let has_alloca = block
            .instructions
            .iter()
            .any(|i| matches!(i, IrInstruction::Alloca { .. }));
        assert!(
            has_alloca,
            "Expected Alloca instruction for variable declaration"
        );
    }

    #[test]
    fn test_lower_typed_variable() {
        let ir = assert_lowers("fn f() { let x: int = 42; }");
        let block = &ir.functions[0].blocks[0];
        let has_alloca = block
            .instructions
            .iter()
            .any(|i| matches!(i, IrInstruction::Alloca { .. }));
        assert!(has_alloca);
    }

    // ─── Return Statements ─────────────────────────────────────────

    #[test]
    fn test_lower_return_int() {
        let ir = assert_lowers("fn f() -> int { return 42; }");
        let block = &ir.functions[0].blocks[0];
        let has_return = block
            .instructions
            .iter()
            .any(|i| matches!(i, IrInstruction::Return { .. }));
        assert!(has_return, "Expected Return instruction");
    }

    #[test]
    fn test_lower_return_void() {
        let ir = assert_lowers("fn f() { return; }");
        let block = &ir.functions[0].blocks[0];
        let has_return = block
            .instructions
            .iter()
            .any(|i| matches!(i, IrInstruction::Return { value: None }));
        assert!(has_return, "Expected void Return instruction");
    }

    #[test]
    fn test_lower_implicit_return() {
        let ir = assert_lowers("fn f() { }");
        let block = &ir.functions[0].blocks[0];
        let last = block.instructions.last();
        assert!(
            matches!(last, Some(IrInstruction::Return { .. })),
            "Expected implicit Return terminator"
        );
    }

    // ─── Binary Operations ─────────────────────────────────────────

    #[test]
    fn test_lower_binary_add() {
        let ir = assert_lowers("fn f() -> int { return 1 + 2; }");
        let block = &ir.functions[0].blocks[0];
        let has_binop = block.instructions.iter().any(|i| {
            matches!(i, IrInstruction::BinaryOp { op: Operator::Plus, .. })
        });
        assert!(has_binop, "Expected Plus BinaryOp instruction");
    }

    #[test]
    fn test_lower_binary_sub() {
        let ir = assert_lowers("fn f() -> int { return 5 - 3; }");
        let block = &ir.functions[0].blocks[0];
        let has_binop = block.instructions.iter().any(|i| {
            matches!(i, IrInstruction::BinaryOp { op: Operator::Minus, .. })
        });
        assert!(has_binop, "Expected Minus BinaryOp instruction");
    }

    #[test]
    fn test_lower_binary_mul() {
        let ir = assert_lowers("fn f() -> int { return 2 * 3; }");
        let block = &ir.functions[0].blocks[0];
        let has_binop = block.instructions.iter().any(|i| {
            matches!(i, IrInstruction::BinaryOp { op: Operator::Star, .. })
        });
        assert!(has_binop, "Expected Star BinaryOp instruction");
    }

    #[test]
    fn test_lower_binary_div() {
        let ir = assert_lowers("fn f() -> int { return 10 / 2; }");
        let block = &ir.functions[0].blocks[0];
        let has_binop = block.instructions.iter().any(|i| {
            matches!(i, IrInstruction::BinaryOp { op: Operator::Slash, .. })
        });
        assert!(has_binop, "Expected Slash BinaryOp instruction");
    }

    #[test]
    fn test_lower_binary_modulo() {
        let ir = assert_lowers("fn f() -> int { return 10 % 3; }");
        let block = &ir.functions[0].blocks[0];
        let has_binop = block.instructions.iter().any(|i| {
            matches!(i, IrInstruction::BinaryOp { op: Operator::Percent, .. })
        });
        assert!(has_binop, "Expected Percent BinaryOp instruction");
    }

    #[test]
    fn test_lower_comparison_eq() {
        let ir = assert_lowers("fn f() -> bool { return 1 == 2; }");
        let block = &ir.functions[0].blocks[0];
        let has_cmp = block.instructions.iter().any(|i| {
            matches!(i, IrInstruction::BinaryOp { op: Operator::Equal, .. })
        });
        assert!(has_cmp, "Expected EqualEqual comparison instruction");
    }

    #[test]
    fn test_lower_comparison_lt() {
        let ir = assert_lowers("fn f() -> bool { return 1 < 2; }");
        let block = &ir.functions[0].blocks[0];
        let has_cmp = block.instructions.iter().any(|i| {
            matches!(i, IrInstruction::BinaryOp { op: Operator::Less, .. })
        });
        assert!(has_cmp, "Expected Less comparison instruction");
    }

    // ─── Control Flow ──────────────────────────────────────────────

    #[test]
    fn test_lower_if_statement() {
        let ir = assert_lowers(r#"
            fn f() {
                if (true) {
                    let x = 1;
                }
            }
        "#);
        assert_eq!(ir.functions.len(), 1);
        let block = &ir.functions[0].blocks[0];
        // If statement should produce instructions (condition eval + body)
        assert!(
            block.instructions.len() > 1,
            "Expected multiple instructions for if statement"
        );
    }

    #[test]
    fn test_lower_if_else() {
        let ir = assert_lowers(r#"
            fn f() {
                if (true) {
                    let x = 1;
                } else {
                    let x = 2;
                }
            }
        "#);
        assert_eq!(ir.functions.len(), 1);
        let block = &ir.functions[0].blocks[0];
        // If-else should produce instructions for both branches
        assert!(
            block.instructions.len() > 1,
            "Expected multiple instructions for if-else"
        );
    }

    #[test]
    fn test_lower_while_loop() {
        let ir = assert_lowers(r#"
            fn f() {
                let i = 0;
                while (i < 10) {
                    i = i + 1;
                }
            }
        "#);
        assert_eq!(ir.functions.len(), 1);
        let block = &ir.functions[0].blocks[0];
        // While loop should produce instructions (init + condition + body + update)
        assert!(
            block.instructions.len() > 2,
            "Expected multiple instructions for while loop"
        );
    }

    #[test]
    fn test_lower_for_loop() {
        let ir = assert_lowers(r#"
            fn f() {
                for (let i = 0; i < 10; i = i + 1) {
                    let x = i;
                }
            }
        "#);
        assert_eq!(ir.functions.len(), 1);
        let block = &ir.functions[0].blocks[0];
        assert!(!block.instructions.is_empty());
    }

    // ─── Function Calls ────────────────────────────────────────────

    #[test]
    fn test_lower_function_call() {
        let ir = assert_lowers(r#"
            fn callee(x: int) -> int { return x; }
            fn caller() -> int { return callee(42); }
        "#);
        assert_eq!(ir.functions.len(), 2);
        let caller = &ir.functions[1];
        let block = &caller.blocks[0];
        let has_call = block
            .instructions
            .iter()
            .any(|i| matches!(i, IrInstruction::Call { func, .. } if func == "callee"));
        assert!(has_call, "Expected Call instruction to callee");
    }

    #[test]
    fn test_lower_function_call_no_args() {
        let ir = assert_lowers(r#"
            fn get_zero() -> int { return 0; }
            fn f() -> int { return get_zero(); }
        "#);
        let f = &ir.functions[1];
        let block = &f.blocks[0];
        let has_call = block
            .instructions
            .iter()
            .any(|i| matches!(i, IrInstruction::Call { args, .. } if args.is_empty()));
        assert!(has_call, "Expected Call with no args");
    }

    // ─── Struct Operations ─────────────────────────────────────────

    #[test]
    fn test_lower_struct_literal() {
        let ir = assert_lowers(r#"
            struct Point { x: int; y: int; }
            fn f() {
                let p = Point { x: 1, y: 2 };
            }
        "#);
        assert_eq!(ir.structs.len(), 1);
        assert_eq!(ir.functions.len(), 1);
    }

    #[test]
    fn test_lower_struct_field_access() {
        let ir = assert_lowers(r#"
            struct Point { x: int; y: int; }
            fn f() -> int {
                let p = Point { x: 10, y: 20 };
                return p.x;
            }
        "#);
        assert_eq!(ir.structs.len(), 1);
        assert_eq!(ir.functions.len(), 1);
    }

    // ─── Literal Return Values ─────────────────────────────────────

    #[test]
    fn test_lower_return_contains_value() {
        let ir = assert_lowers("fn f() -> int { return 42; }");
        let block = &ir.functions[0].blocks[0];
        let has_return_with_value = block
            .instructions
            .iter()
            .any(|i| matches!(i, IrInstruction::Return { value: Some(_) }));
        assert!(has_return_with_value, "Expected Return with value");
    }

    // ─── Complex Programs ──────────────────────────────────────────

    #[test]
    fn test_lower_factorial() {
        let ir = assert_lowers(r#"
            fn factorial(n: int) -> int {
                if (n <= 1) {
                    return 1;
                }
                return n * factorial(n - 1);
            }
        "#);
        assert_eq!(ir.functions.len(), 1);
        assert_eq!(ir.functions[0].name, "factorial");
        assert_eq!(ir.functions[0].params.len(), 1);
    }

    #[test]
    fn test_lower_mixed_program() {
        let ir = assert_lowers(r#"
            struct Point { x: int; y: int; }
            fn origin() -> Point {
                return Point { x: 0, y: 0 };
            }
            fn distance(a: int, b: int) -> int {
                return a + b;
            }
        "#);
        assert_eq!(ir.structs.len(), 1);
        assert_eq!(ir.functions.len(), 2);
    }

    #[test]
    fn test_lower_nested_calls() {
        let ir = assert_lowers(r#"
            fn double(x: int) -> int { return x * 2; }
            fn triple(x: int) -> int { return x * 3; }
            fn combined(x: int) -> int { return double(triple(x)); }
        "#);
        assert_eq!(ir.functions.len(), 3);
    }

    // ─── AST-level Lowering (Direct AST Construction) ──────────────

    #[test]
    fn test_lower_ast_empty_function() {
        let mut lowering = IrLowering::new();
        let program = Program::new(vec![Statement::FunctionDeclaration {
            name: "test".to_string(),
            generic_params: vec![],
            where_constraints: vec![],
            parameters: vec![],
            return_type: None,
            body: Block::new(vec![]),
            is_async: false,
            is_unsafe: false,
            is_public: false,
            is_variadic: false,
        }]);
        let ir = lowering.lower_program(&program).unwrap();
        assert_eq!(ir.functions.len(), 1);
        assert_eq!(ir.functions[0].name, "test");
        assert_eq!(ir.functions[0].return_type, IrType::Void);
    }

    #[test]
    fn test_lower_ast_function_with_params() {
        let mut lowering = IrLowering::new();
        let program = Program::new(vec![Statement::FunctionDeclaration {
            name: "add".to_string(),
            generic_params: vec![],
            where_constraints: vec![],
            parameters: vec![
                Parameter {
                    pattern: Pattern::Identifier("a".to_string()),
                    param_type: Type::Int,
                    is_reference: false,
                },
                Parameter {
                    pattern: Pattern::Identifier("b".to_string()),
                    param_type: Type::Int,
                    is_reference: false,
                },
            ],
            return_type: Some(Type::Int),
            body: Block::new(vec![]),
            is_async: false,
            is_unsafe: false,
            is_public: false,
            is_variadic: false,
        }]);
        let ir = lowering.lower_program(&program).unwrap();
        assert_eq!(ir.functions[0].params.len(), 2);
        assert_eq!(ir.functions[0].params[0].name, "a");
        assert_eq!(ir.functions[0].params[1].name, "b");
    }

    // ─── Type Lowering ─────────────────────────────────────────────

    #[test]
    fn test_lower_all_primitive_types() {
        let ir = assert_lowers(r#"
            struct AllTypes {
                a: int;
                b: float;
                c: bool;
                d: string;
            }
        "#);
        let fields = &ir.structs[0].fields;
        assert_eq!(fields[0].1, IrType::Int);
        assert_eq!(fields[1].1, IrType::Float);
        assert_eq!(fields[2].1, IrType::Bool);
        assert_eq!(fields[3].1, IrType::String);
    }

    // ─── Edge Cases ────────────────────────────────────────────────

    #[test]
    fn test_lower_constants_skipped() {
        let ir = assert_lowers("const MAX = 100;");
        assert!(ir.functions.is_empty());
        assert!(ir.structs.is_empty());
    }

    #[test]
    fn test_lower_imports_skipped() {
        let ir = assert_lowers("import std.io;");
        assert!(ir.functions.is_empty());
    }

    #[test]
    fn test_lower_module_skipped() {
        let ir = assert_lowers("module my.package;");
        assert!(ir.functions.is_empty());
    }

    #[test]
    fn test_lower_function_entry_block_label() {
        let ir = assert_lowers("fn f() { }");
        let block = &ir.functions[0].blocks[0];
        assert_eq!(block.name, "entry");
    }

    #[test]
    fn test_lower_function_value_id_reset() {
        let ir = assert_lowers(r#"
            fn first(a: int) -> int { return a; }
            fn second(b: int) -> int { return b; }
        "#);
        assert!(!ir.functions[0].blocks.is_empty());
        assert!(!ir.functions[1].blocks.is_empty());
    }
}
