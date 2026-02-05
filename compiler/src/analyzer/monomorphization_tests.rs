//! Comprehensive tests for the monomorphization (generics) system.

#[cfg(test)]
mod tests {
    use crate::analyzer::monomorphization::monomorphize_program;
    use crate::parser::ast::*;
    use std::path::PathBuf;

    fn test_path() -> PathBuf {
        PathBuf::from("test.kr")
    }

    // ─── Helper builders ───────────────────────────────────────────

    fn make_program(stmts: Vec<Statement>) -> Program {
        Program::new(stmts)
    }

    fn make_generic_fn(
        name: &str,
        generic_params: Vec<&str>,
        params: Vec<Parameter>,
        return_type: Option<Type>,
        body: Vec<Statement>,
    ) -> Statement {
        Statement::FunctionDeclaration {
            name: name.to_string(),
            generic_params: generic_params.into_iter().map(String::from).collect(),
            where_constraints: Vec::new(),
            parameters: params,
            return_type,
            body: Block::new(body),
            is_async: false,
            is_unsafe: false,
            is_public: false,
            is_variadic: false,
        }
    }

    fn make_generic_fn_with_where(
        name: &str,
        generic_params: Vec<&str>,
        where_constraints: Vec<WhereConstraint>,
        params: Vec<Parameter>,
        return_type: Option<Type>,
        body: Vec<Statement>,
    ) -> Statement {
        Statement::FunctionDeclaration {
            name: name.to_string(),
            generic_params: generic_params.into_iter().map(String::from).collect(),
            where_constraints,
            parameters: params,
            return_type,
            body: Block::new(body),
            is_async: false,
            is_unsafe: false,
            is_public: false,
            is_variadic: false,
        }
    }

    fn make_concrete_fn(
        name: &str,
        params: Vec<Parameter>,
        return_type: Option<Type>,
        body: Vec<Statement>,
    ) -> Statement {
        Statement::FunctionDeclaration {
            name: name.to_string(),
            generic_params: Vec::new(),
            where_constraints: Vec::new(),
            parameters: params,
            return_type,
            body: Block::new(body),
            is_async: false,
            is_unsafe: false,
            is_public: false,
            is_variadic: false,
        }
    }

    fn make_param(name: &str, ty: Type) -> Parameter {
        Parameter {
            pattern: Pattern::Identifier(name.to_string()),
            param_type: ty,
            is_reference: false,
        }
    }

    fn make_generic_struct(
        name: &str,
        generic_params: Vec<&str>,
        fields: Vec<StructField>,
    ) -> Statement {
        Statement::StructDeclaration {
            name: name.to_string(),
            generic_params: generic_params.into_iter().map(String::from).collect(),
            where_constraints: Vec::new(),
            fields,
            is_public: false,
            repr: None,
        }
    }

    fn make_generic_enum(
        name: &str,
        generic_params: Vec<&str>,
        variants: Vec<(String, Option<EnumVariantPayload>)>,
    ) -> Statement {
        Statement::EnumDeclaration {
            name: name.to_string(),
            generic_params: generic_params.into_iter().map(String::from).collect(),
            where_constraints: Vec::new(),
            variants,
            is_public: false,
        }
    }

    fn make_field(name: &str, ty: Type) -> StructField {
        StructField {
            name: name.to_string(),
            field_type: ty,
            is_public: false,
        }
    }

    fn make_call(name: &str, type_args: Option<Vec<Type>>, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(Expression::Identifier(name.to_string())),
            type_args,
            arguments: args,
        }
    }

    fn make_struct_literal(
        name: &str,
        type_args: Option<Vec<Type>>,
        fields: Vec<(&str, Expression)>,
    ) -> Expression {
        Expression::StructLiteral {
            name: name.to_string(),
            type_args,
            fields: fields
                .into_iter()
                .map(|(n, e)| (n.to_string(), e))
                .collect(),
        }
    }

    fn make_return(expr: Expression) -> Statement {
        Statement::Return { value: Some(expr) }
    }

    fn make_var_decl(name: &str, ty: Option<Type>, init: Expression) -> Statement {
        Statement::VariableDeclaration {
            pattern: Pattern::Identifier(name.to_string()),
            type_annotation: ty,
            initializer: Some(init),
            is_mutable: false,
        }
    }

    // ─── Generic function tests ────────────────────────────────────

    #[test]
    fn test_generic_identity_function_explicit_int() {
        // fn id<T>(x: T) -> T { return x; }
        // fn main() { let r = id<int>(42); }
        let program = make_program(vec![
            make_generic_fn(
                "id",
                vec!["T"],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "id",
                        Some(vec![Type::Int]),
                        vec![Expression::IntLiteral(42)],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(
            result.is_ok(),
            "Monomorphization failed: {:?}",
            result.err()
        );
        let out = result.unwrap();

        // Generic template should be removed, specialized version should exist
        let has_generic = out.statements.iter().any(|s| {
            matches!(s,
                Statement::FunctionDeclaration { name, generic_params, .. }
                if name == "id" && !generic_params.is_empty()
            )
        });
        assert!(!has_generic, "Generic template should be removed");

        let has_specialized = out.statements.iter().any(|s| {
            matches!(s,
                Statement::FunctionDeclaration { name, generic_params, .. }
                if name.starts_with("id__") && generic_params.is_empty()
            )
        });
        assert!(has_specialized, "Specialized function should exist");
    }

    #[test]
    fn test_generic_identity_function_explicit_string() {
        let program = make_program(vec![
            make_generic_fn(
                "id",
                vec!["T"],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "id",
                        Some(vec![Type::String]),
                        vec![Expression::StringLiteral("hello".into())],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
    }

    #[test]
    fn test_generic_function_inferred_type_args() {
        // fn id<T>(x: T) -> T { return x; }
        // fn main() { let r = id(42); }  // T inferred as int
        let program = make_program(vec![
            make_generic_fn(
                "id",
                vec!["T"],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call("id", None, vec![Expression::IntLiteral(42)]),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok(), "Type inference failed: {:?}", result.err());
    }

    #[test]
    fn test_generic_function_two_type_params() {
        // fn pair<A, B>(a: A, b: B) -> A { return a; }
        // fn main() { let r = pair<int, string>(1, "hi"); }
        let program = make_program(vec![
            make_generic_fn(
                "pair",
                vec!["A", "B"],
                vec![
                    make_param("a", Type::Custom("A".into())),
                    make_param("b", Type::Custom("B".into())),
                ],
                Some(Type::Custom("A".into())),
                vec![make_return(Expression::Identifier("a".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "pair",
                        Some(vec![Type::Int, Type::String]),
                        vec![
                            Expression::IntLiteral(1),
                            Expression::StringLiteral("hi".into()),
                        ],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
    }

    #[test]
    fn test_generic_function_multiple_instantiations() {
        // fn id<T>(x: T) -> T { return x; }
        // fn main() { let a = id<int>(1); let b = id<string>("hi"); }
        let program = make_program(vec![
            make_generic_fn(
                "id",
                vec!["T"],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![
                    make_var_decl(
                        "a",
                        None,
                        make_call("id", Some(vec![Type::Int]), vec![Expression::IntLiteral(1)]),
                    ),
                    make_var_decl(
                        "b",
                        None,
                        make_call(
                            "id",
                            Some(vec![Type::String]),
                            vec![Expression::StringLiteral("hi".into())],
                        ),
                    ),
                ],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
        let out = result.unwrap();

        // Should have two specializations
        let specialized_count = out
            .statements
            .iter()
            .filter(|s| {
                matches!(s,
                    Statement::FunctionDeclaration { name, generic_params, .. }
                    if name.starts_with("id__") && generic_params.is_empty()
                )
            })
            .count();
        assert_eq!(
            specialized_count, 2,
            "Should have two specializations of id"
        );
    }

    // ─── Generic struct tests ──────────────────────────────────────

    #[test]
    fn test_generic_struct_instantiation() {
        // struct Wrapper<T> { value: T; }
        // fn main() { let w = Wrapper<int> { value: 42 }; }
        let program = make_program(vec![
            make_generic_struct(
                "Wrapper",
                vec!["T"],
                vec![make_field("value", Type::Custom("T".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "w",
                    None,
                    make_struct_literal(
                        "Wrapper",
                        Some(vec![Type::Int]),
                        vec![("value", Expression::IntLiteral(42))],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
        let out = result.unwrap();

        let has_specialized = out.statements.iter().any(|s| {
            matches!(s,
                Statement::StructDeclaration { name, generic_params, .. }
                if name.starts_with("Wrapper__") && generic_params.is_empty()
            )
        });
        assert!(has_specialized, "Specialized struct should exist");
    }

    #[test]
    fn test_generic_struct_two_type_params() {
        // struct Pair<A, B> { first: A; second: B; }
        let program = make_program(vec![
            make_generic_struct(
                "Pair",
                vec!["A", "B"],
                vec![
                    make_field("first", Type::Custom("A".into())),
                    make_field("second", Type::Custom("B".into())),
                ],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "p",
                    None,
                    make_struct_literal(
                        "Pair",
                        Some(vec![Type::Int, Type::String]),
                        vec![
                            ("first", Expression::IntLiteral(1)),
                            ("second", Expression::StringLiteral("hi".into())),
                        ],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
    }

    // ─── Generic enum tests ────────────────────────────────────────

    #[test]
    fn test_generic_enum_declaration() {
        // enum Option<T> { Some(T), None }
        let program = make_program(vec![
            make_generic_enum(
                "Option",
                vec!["T"],
                vec![
                    (
                        "Some".to_string(),
                        Some(EnumVariantPayload::Tuple(vec![Type::Custom("T".into())])),
                    ),
                    ("None".to_string(), None),
                ],
            ),
            make_concrete_fn("main", vec![], None, vec![]),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
        let out = result.unwrap();

        // Generic enum template should be removed
        let has_generic = out.statements.iter().any(|s| {
            matches!(s,
                Statement::EnumDeclaration { name, generic_params, .. }
                if name == "Option" && !generic_params.is_empty()
            )
        });
        assert!(!has_generic, "Generic enum template should be removed");
    }

    #[test]
    fn test_generic_enum_result() {
        // enum Result<T, E> { Ok(T), Err(E) }
        let program = make_program(vec![
            make_generic_enum(
                "Result",
                vec!["T", "E"],
                vec![
                    (
                        "Ok".to_string(),
                        Some(EnumVariantPayload::Tuple(vec![Type::Custom("T".into())])),
                    ),
                    (
                        "Err".to_string(),
                        Some(EnumVariantPayload::Tuple(vec![Type::Custom("E".into())])),
                    ),
                ],
            ),
            make_concrete_fn("main", vec![], None, vec![]),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
    }

    // ─── Where clause / trait bound tests ──────────────────────────

    #[test]
    fn test_where_clause_clone_satisfied() {
        // fn clone_it<T>(x: T) -> T where T: Clone { return x; }
        // fn main() { let r = clone_it<int>(42); }
        let program = make_program(vec![
            make_generic_fn_with_where(
                "clone_it",
                vec!["T"],
                vec![WhereConstraint {
                    type_param: "T".into(),
                    trait_name: "Clone".into(),
                }],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "clone_it",
                        Some(vec![Type::Int]),
                        vec![Expression::IntLiteral(42)],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok(), "Clone bound on int should be satisfied");
    }

    #[test]
    fn test_where_clause_clone_violated() {
        // fn clone_it<T>(x: T) -> T where T: Clone { return x; }
        // fn main() { let r = clone_it<void>(???); }
        let program = make_program(vec![
            make_generic_fn_with_where(
                "clone_it",
                vec!["T"],
                vec![WhereConstraint {
                    type_param: "T".into(),
                    trait_name: "Clone".into(),
                }],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "clone_it",
                        Some(vec![Type::Void]),
                        vec![Expression::NullLiteral],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_err(), "Clone bound on void should fail");
    }

    #[test]
    fn test_where_clause_copy_satisfied() {
        let program = make_program(vec![
            make_generic_fn_with_where(
                "copy_it",
                vec!["T"],
                vec![WhereConstraint {
                    type_param: "T".into(),
                    trait_name: "Copy".into(),
                }],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "copy_it",
                        Some(vec![Type::Int]),
                        vec![Expression::IntLiteral(1)],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok(), "Copy bound on int should be satisfied");
    }

    #[test]
    fn test_where_clause_copy_violated() {
        let program = make_program(vec![
            make_generic_fn_with_where(
                "copy_it",
                vec!["T"],
                vec![WhereConstraint {
                    type_param: "T".into(),
                    trait_name: "Copy".into(),
                }],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "copy_it",
                        Some(vec![Type::String]),
                        vec![Expression::StringLiteral("hi".into())],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_err(), "Copy bound on string should fail");
    }

    #[test]
    fn test_where_clause_display_satisfied() {
        let program = make_program(vec![
            make_generic_fn_with_where(
                "show",
                vec!["T"],
                vec![WhereConstraint {
                    type_param: "T".into(),
                    trait_name: "Display".into(),
                }],
                vec![make_param("x", Type::Custom("T".into()))],
                None,
                vec![],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![Statement::Expression(make_call(
                    "show",
                    Some(vec![Type::Int]),
                    vec![Expression::IntLiteral(42)],
                ))],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok(), "Display bound on int should be satisfied");
    }

    #[test]
    fn test_where_clause_hash_float_violated() {
        let program = make_program(vec![
            make_generic_fn_with_where(
                "hash_it",
                vec!["T"],
                vec![WhereConstraint {
                    type_param: "T".into(),
                    trait_name: "Hash".into(),
                }],
                vec![make_param("x", Type::Custom("T".into()))],
                None,
                vec![],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![Statement::Expression(make_call(
                    "hash_it",
                    Some(vec![Type::Float]),
                    vec![Expression::FloatLiteral(2.78)],
                ))],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_err(), "Hash bound on float should fail");
    }

    #[test]
    fn test_where_clause_multiple_bounds() {
        let program = make_program(vec![
            make_generic_fn_with_where(
                "process",
                vec!["T"],
                vec![
                    WhereConstraint {
                        type_param: "T".into(),
                        trait_name: "Clone".into(),
                    },
                    WhereConstraint {
                        type_param: "T".into(),
                        trait_name: "PartialEq".into(),
                    },
                ],
                vec![make_param("x", Type::Custom("T".into()))],
                None,
                vec![],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![Statement::Expression(make_call(
                    "process",
                    Some(vec![Type::Int]),
                    vec![Expression::IntLiteral(1)],
                ))],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(
            result.is_ok(),
            "Clone + PartialEq on int should be satisfied"
        );
    }

    // ─── Name mangling tests ───────────────────────────────────────

    #[test]
    fn test_name_mangling_distinct() {
        // Two different instantiations should produce different mangled names
        let program = make_program(vec![
            make_generic_fn(
                "id",
                vec!["T"],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![
                    make_var_decl(
                        "a",
                        None,
                        make_call("id", Some(vec![Type::Int]), vec![Expression::IntLiteral(1)]),
                    ),
                    make_var_decl(
                        "b",
                        None,
                        make_call(
                            "id",
                            Some(vec![Type::Bool]),
                            vec![Expression::BoolLiteral(true)],
                        ),
                    ),
                ],
            ),
        ]);

        let result = monomorphize_program(program, test_path()).unwrap();
        let specialized: Vec<&str> = result
            .statements
            .iter()
            .filter_map(|s| match s {
                Statement::FunctionDeclaration {
                    name,
                    generic_params,
                    ..
                } if name.starts_with("id__") && generic_params.is_empty() => Some(name.as_str()),
                _ => None,
            })
            .collect();

        assert_eq!(specialized.len(), 2);
        assert_ne!(specialized[0], specialized[1]);
    }

    // ─── Error case tests ──────────────────────────────────────────

    #[test]
    fn test_wrong_number_of_type_args() {
        // fn id<T>(x: T) -> T { return x; }
        // fn main() { let r = id<int, string>(42); }  // Too many type args
        let program = make_program(vec![
            make_generic_fn(
                "id",
                vec!["T"],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![make_var_decl(
                    "r",
                    None,
                    make_call(
                        "id",
                        Some(vec![Type::Int, Type::String]),
                        vec![Expression::IntLiteral(42)],
                    ),
                )],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_err(), "Wrong number of type args should fail");
    }

    #[test]
    fn test_generic_call_to_non_generic_function() {
        let program = make_program(vec![
            make_concrete_fn("foo", vec![make_param("x", Type::Int)], None, vec![]),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![Statement::Expression(make_call(
                    "foo",
                    Some(vec![Type::Int]),
                    vec![Expression::IntLiteral(1)],
                ))],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(
            result.is_err(),
            "Generic call to non-generic function should fail"
        );
    }

    // ─── Deduplication tests ───────────────────────────────────────

    #[test]
    fn test_duplicate_instantiation_deduplication() {
        // Same type args used twice should only produce one specialization
        let program = make_program(vec![
            make_generic_fn(
                "id",
                vec!["T"],
                vec![make_param("x", Type::Custom("T".into()))],
                Some(Type::Custom("T".into())),
                vec![make_return(Expression::Identifier("x".into()))],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![
                    make_var_decl(
                        "a",
                        None,
                        make_call("id", Some(vec![Type::Int]), vec![Expression::IntLiteral(1)]),
                    ),
                    make_var_decl(
                        "b",
                        None,
                        make_call("id", Some(vec![Type::Int]), vec![Expression::IntLiteral(2)]),
                    ),
                ],
            ),
        ]);

        let result = monomorphize_program(program, test_path()).unwrap();
        let specialized_count = result
            .statements
            .iter()
            .filter(|s| {
                matches!(s,
                    Statement::FunctionDeclaration { name, generic_params, .. }
                    if name.starts_with("id__") && generic_params.is_empty()
                )
            })
            .count();

        assert_eq!(
            specialized_count, 1,
            "Duplicate instantiation should be deduplicated"
        );
    }

    // ─── Unsupported trait bound test ──────────────────────────────

    #[test]
    fn test_unsupported_trait_bound() {
        let program = make_program(vec![
            make_generic_fn_with_where(
                "foo",
                vec!["T"],
                vec![WhereConstraint {
                    type_param: "T".into(),
                    trait_name: "NonExistentTrait".into(),
                }],
                vec![make_param("x", Type::Custom("T".into()))],
                None,
                vec![],
            ),
            make_concrete_fn(
                "main",
                vec![],
                None,
                vec![Statement::Expression(make_call(
                    "foo",
                    Some(vec![Type::Int]),
                    vec![Expression::IntLiteral(1)],
                ))],
            ),
        ]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_err(), "Unsupported trait bound should fail");
    }

    // ─── Empty program test ────────────────────────────────────────

    #[test]
    fn test_empty_program() {
        let program = make_program(vec![]);
        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
        assert!(result.unwrap().statements.is_empty());
    }

    #[test]
    fn test_program_without_generics() {
        let program = make_program(vec![make_concrete_fn(
            "main",
            vec![],
            None,
            vec![make_var_decl(
                "x",
                Some(Type::Int),
                Expression::IntLiteral(42),
            )],
        )]);

        let result = monomorphize_program(program, test_path());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().statements.len(), 1);
    }
}
