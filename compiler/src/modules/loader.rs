use crate::lexer::tokenizer::Tokenizer;
use crate::parser::ast::{
    Block, ClosureBody, Expression, MatchArm, Pattern, Program, Statement, Type,
};
use crate::parser::Parser;
use anyhow::{Context, Result};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use tokio::task;

#[derive(Clone)]
struct CachedModule {
    program: Program,
    declared_module: Option<Vec<String>>,
}

pub async fn load_program(entry_file: &Path) -> Result<Program> {
    let entry = entry_file.to_path_buf();

    task::spawn_blocking(move || load_program_sync(&entry))
        .await
        .context("Module loader task join failed")?
}

fn load_program_sync(entry_file: &Path) -> Result<Program> {
    let entry_file = entry_file.canonicalize().with_context(|| {
        format!(
            "Failed to canonicalize entry file: {}",
            entry_file.display()
        )
    })?;

    let project_root = entry_file
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Entry file has no parent directory"))?
        .to_path_buf();

    let mut stack: Vec<PathBuf> = Vec::new();
    let mut cache: HashMap<PathBuf, CachedModule> = HashMap::new();

    load_program_inner_sync(&entry_file, &project_root, None, &mut stack, &mut cache)
        .map(|m| m.program)
}

fn load_program_inner_sync(
    file: &Path,
    project_root: &Path,
    expected_module: Option<Vec<String>>,
    stack: &mut Vec<PathBuf>,
    cache: &mut HashMap<PathBuf, CachedModule>,
) -> Result<CachedModule> {
    let file = file
        .canonicalize()
        .with_context(|| format!("Failed to canonicalize file: {}", file.display()))?;

    if let Some(m) = cache.get(&file) {
        validate_expected_module(
            &file,
            project_root,
            &m.declared_module,
            expected_module.as_deref(),
        )?;
        return Ok(m.clone());
    }

    if stack.contains(&file) {
        let mut cycle = stack
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>();
        cycle.push(file.display().to_string());
        anyhow::bail!("Import cycle detected: {}", cycle.join(" -> "));
    }

    stack.push(file.clone());

    let source = std::fs::read_to_string(&file)
        .with_context(|| format!("Failed to read source file: {}", file.display()))?;

    let mut tokenizer = Tokenizer::new(source, file.clone());
    let tokens = tokenizer.tokenize().context("Lexer error")?;

    let mut parser = Parser::new(tokens, file.clone());
    let program = parser.parse().context("Parser error")?;

    let declared_module = validate_module_declaration(&file, project_root, &program)?;
    validate_expected_module(
        &file,
        project_root,
        &declared_module,
        expected_module.as_deref(),
    )?;

    let module_id = file
        .strip_prefix(project_root)
        .unwrap_or(&file)
        .to_string_lossy()
        .to_string();
    let private_mangle = build_private_mangle_map(&module_id, &program);
    let program = apply_module_mangling(&file, program, &private_mangle)?;

    let importer_dir = file
        .parent()
        .ok_or_else(|| anyhow::anyhow!("File has no parent directory"))?
        .to_path_buf();

    let mut merged_statements: Vec<Statement> = Vec::new();

    for statement in &program.statements {
        if let Statement::Import { path } = statement {
            let import_path = resolve_import_path(&importer_dir, project_root, path)
                .with_context(|| format!("Failed to resolve import {}", path.join(".")))?;
            let imported_module = load_program_inner_sync(
                &import_path,
                project_root,
                Some(path.clone()),
                stack,
                cache,
            )?;
            merged_statements.extend(imported_module.program.statements);
        }
    }

    for statement in program.statements.into_iter() {
        if !matches!(
            statement,
            Statement::Import { .. } | Statement::Module { .. }
        ) {
            merged_statements.push(statement);
        }
    }

    let merged = Program::new(merged_statements);
    let cached = CachedModule {
        program: merged,
        declared_module,
    };
    cache.insert(file.clone(), cached.clone());

    stack.pop();
    Ok(cached)
}

fn validate_module_declaration(
    file: &Path,
    project_root: &Path,
    program: &Program,
) -> Result<Option<Vec<String>>> {
    let mut decl: Option<(usize, Vec<String>)> = None;

    for (idx, s) in program.statements.iter().enumerate() {
        if let Statement::Module { path } = s {
            if decl.is_some() {
                anyhow::bail!(
                    "Multiple module declarations are not allowed in a single file: {}",
                    file.display()
                );
            }
            decl = Some((idx, path.clone()));
        }
    }

    let Some((idx, module_path)) = decl else {
        return Ok(None);
    };

    if idx != 0 {
        anyhow::bail!(
            "Module declaration must be the first statement in file: {}",
            file.display()
        );
    }

    let expected = expected_module_path(file, project_root)?;
    if module_path != expected {
        anyhow::bail!(
            "Module declaration does not match file path. declared={} expected={} file={}",
            module_path.join("."),
            expected.join("."),
            file.display()
        );
    }

    Ok(Some(module_path))
}

fn validate_expected_module(
    file: &Path,
    project_root: &Path,
    declared: &Option<Vec<String>>,
    expected: Option<&[String]>,
) -> Result<()> {
    let Some(expected) = expected else {
        return Ok(());
    };

    let Some(declared) = declared else {
        anyhow::bail!(
            "Imported module must declare its module path with `module ...;` (expected={} file={})",
            expected.join("."),
            file.display()
        );
    };

    if declared.as_slice() != expected {
        anyhow::bail!(
            "Imported module declaration mismatch. declared={} expected={} file={}",
            declared.join("."),
            expected.join("."),
            file.display()
        );
    }

    // Redundant safety: also ensure declared matches file path relative to project root.
    let expected_by_file = expected_module_path(file, project_root)?;
    if *declared != expected_by_file {
        anyhow::bail!(
            "Module declaration does not match file path. declared={} expected={} file={}",
            declared.join("."),
            expected_by_file.join("."),
            file.display()
        );
    }

    Ok(())
}

fn expected_module_path(file: &Path, project_root: &Path) -> Result<Vec<String>> {
    let rel = file.strip_prefix(project_root).unwrap_or(file);

    let stem = rel
        .file_stem()
        .ok_or_else(|| anyhow::anyhow!("File has no stem: {}", file.display()))?
        .to_string_lossy()
        .to_string();

    let mut parts: Vec<String> = rel
        .parent()
        .map(|p| {
            p.components()
                .map(|c| c.as_os_str().to_string_lossy().to_string())
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    parts.push(stem);
    Ok(parts)
}

fn build_private_mangle_map(module_id: &str, program: &Program) -> HashMap<String, String> {
    let mut map = HashMap::new();

    for statement in &program.statements {
        match statement {
            Statement::FunctionDeclaration {
                name, is_public, ..
            } => {
                if name != "main" && !*is_public {
                    map.insert(name.clone(), mangle_symbol(module_id, name));
                }
            }
            Statement::StructDeclaration {
                name, is_public, ..
            }
            | Statement::ClassDeclaration {
                name, is_public, ..
            } => {
                if !*is_public {
                    map.insert(name.clone(), mangle_symbol(module_id, name));
                }
            }
            _ => {}
        }
    }

    map
}

fn mangle_symbol(module_id: &str, name: &str) -> String {
    let mut hasher = DefaultHasher::new();
    module_id.hash(&mut hasher);
    let h = hasher.finish();
    format!("__m{h:016x}_{name}")
}

fn apply_module_mangling(
    file: &Path,
    program: Program,
    private_mangle: &HashMap<String, String>,
) -> Result<Program> {
    let mut out = Vec::with_capacity(program.statements.len());
    for stmt in program.statements {
        out.push(rewrite_statement(file, stmt, private_mangle)?);
    }
    Ok(Program::new(out))
}

fn rewrite_statement(
    file: &Path,
    statement: Statement,
    private_mangle: &HashMap<String, String>,
) -> Result<Statement> {
    match statement {
        Statement::Module { .. } => Ok(statement),
        Statement::Import { .. } => Ok(statement),

        Statement::FunctionDeclaration {
            name,
            generic_params,
            where_constraints,
            parameters,
            return_type,
            body,
            is_async,
            is_unsafe,
            is_public,
        } => {
            if is_public {
                for p in &parameters {
                    if let Some(t) = first_private_type_reference(&p.param_type, private_mangle) {
                        anyhow::bail!(
                            "Public function {name} uses private type {t} in parameter type ({})",
                            file.display()
                        );
                    }
                }
                if let Some(rt) = &return_type {
                    if let Some(t) = first_private_type_reference(rt, private_mangle) {
                        anyhow::bail!(
                            "Public function {name} uses private type {t} in return type ({})",
                            file.display()
                        );
                    }
                }
            }

            let new_name = private_mangle.get(&name).cloned().unwrap_or(name);
            let mut new_params = Vec::with_capacity(parameters.len());
            for p in parameters {
                let mut p = p;
                p.param_type = rewrite_type(p.param_type, private_mangle);
                new_params.push(p);
            }

            let new_return = return_type.map(|t| rewrite_type(t, private_mangle));
            let new_body = rewrite_block(body, private_mangle)?;

            Ok(Statement::FunctionDeclaration {
                name: new_name,
                generic_params,
                where_constraints,
                parameters: new_params,
                return_type: new_return,
                body: new_body,
                is_async,
                is_unsafe,
                is_public,
            })
        }

        Statement::StructDeclaration {
            name,
            generic_params,
            where_constraints,
            fields,
            is_public,
        } => {
            if is_public {
                for f in &fields {
                    if let Some(t) = first_private_type_reference(&f.field_type, private_mangle) {
                        anyhow::bail!(
                            "Public type {name} uses private type {t} in field type ({})",
                            file.display()
                        );
                    }
                }
            }

            let new_name = private_mangle.get(&name).cloned().unwrap_or(name);
            let mut new_fields = Vec::with_capacity(fields.len());
            for mut f in fields {
                f.field_type = rewrite_type(f.field_type, private_mangle);
                new_fields.push(f);
            }

            Ok(Statement::StructDeclaration {
                name: new_name,
                generic_params,
                where_constraints,
                fields: new_fields,
                is_public,
            })
        }

        Statement::ClassDeclaration {
            name,
            fields,
            methods,
            is_public,
        } => {
            if is_public {
                for f in &fields {
                    if let Some(t) = first_private_type_reference(&f.field_type, private_mangle) {
                        anyhow::bail!(
                            "Public type {name} uses private type {t} in field type ({})",
                            file.display()
                        );
                    }
                }
            }

            let new_name = private_mangle.get(&name).cloned().unwrap_or(name);
            let mut new_fields = Vec::with_capacity(fields.len());
            for mut f in fields {
                f.field_type = rewrite_type(f.field_type, private_mangle);
                new_fields.push(f);
            }

            let mut new_methods = Vec::with_capacity(methods.len());
            for m in methods {
                new_methods.push(rewrite_statement(file, m, private_mangle)?);
            }

            Ok(Statement::ClassDeclaration {
                name: new_name,
                fields: new_fields,
                methods: new_methods,
                is_public,
            })
        }

        Statement::VariableDeclaration {
            pattern,
            type_annotation,
            initializer,
            is_mutable,
        } => Ok(Statement::VariableDeclaration {
            pattern: rewrite_pattern(pattern, private_mangle),
            type_annotation: type_annotation.map(|t| rewrite_type(t, private_mangle)),
            initializer: initializer.map(|e| rewrite_expression(e, private_mangle)),
            is_mutable,
        }),

        Statement::ConstantDeclaration {
            name,
            type_annotation,
            initializer,
        } => Ok(Statement::ConstantDeclaration {
            name,
            type_annotation: type_annotation.map(|t| rewrite_type(t, private_mangle)),
            initializer: rewrite_expression(initializer, private_mangle),
        }),

        Statement::Return { value } => Ok(Statement::Return {
            value: value.map(|v| rewrite_expression(v, private_mangle)),
        }),

        Statement::Expression(expr) => Ok(Statement::Expression(rewrite_expression(
            expr,
            private_mangle,
        ))),

        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => Ok(Statement::If {
            condition: rewrite_expression(condition, private_mangle),
            then_branch: rewrite_block(then_branch, private_mangle)?,
            else_branch: match else_branch {
                Some(b) => Some(rewrite_block(b, private_mangle)?),
                None => None,
            },
        }),

        Statement::While { condition, body } => Ok(Statement::While {
            condition: rewrite_expression(condition, private_mangle),
            body: rewrite_block(body, private_mangle)?,
        }),

        Statement::For {
            initializer,
            condition,
            increment,
            body,
        } => Ok(Statement::For {
            initializer: match initializer {
                Some(s) => Some(Box::new(rewrite_statement(file, *s, private_mangle)?)),
                None => None,
            },
            condition: condition.map(|e| rewrite_expression(e, private_mangle)),
            increment: increment.map(|e| rewrite_expression(e, private_mangle)),
            body: rewrite_block(body, private_mangle)?,
        }),

        Statement::ForIn {
            variable,
            iterable,
            body,
        } => Ok(Statement::ForIn {
            variable,
            iterable: rewrite_expression(iterable, private_mangle),
            body: rewrite_block(body, private_mangle)?,
        }),

        Statement::Match { expression, arms } => {
            let mut new_arms = Vec::with_capacity(arms.len());
            for arm in arms {
                new_arms.push(rewrite_match_arm(arm, private_mangle)?);
            }

            Ok(Statement::Match {
                expression: rewrite_expression(expression, private_mangle),
                arms: new_arms,
            })
        }

        Statement::Defer { statement } => Ok(Statement::Defer {
            statement: Box::new(rewrite_statement(file, *statement, private_mangle)?),
        }),

        Statement::Unsafe { block } => Ok(Statement::Unsafe {
            block: rewrite_block(block, private_mangle)?,
        }),

        Statement::Break
        | Statement::Continue
        | Statement::InterfaceDeclaration { .. }
        | Statement::EnumDeclaration { .. }
        | Statement::TypeAlias { .. }
        | Statement::ImplBlock { .. } => Ok(statement),
    }
}

fn rewrite_block(block: Block, private_mangle: &HashMap<String, String>) -> Result<Block> {
    let mut out = Vec::with_capacity(block.statements.len());
    for stmt in block.statements {
        // Reuse a dummy path for nested statements (only used for error strings; errors are emitted at top level)
        out.push(rewrite_statement(
            Path::new("<block>"),
            stmt,
            private_mangle,
        )?);
    }
    Ok(Block::new(out))
}

fn rewrite_match_arm(arm: MatchArm, private_mangle: &HashMap<String, String>) -> Result<MatchArm> {
    Ok(MatchArm {
        pattern: rewrite_pattern(arm.pattern, private_mangle),
        guard: arm.guard.map(|g| rewrite_expression(g, private_mangle)),
        body: rewrite_block(arm.body, private_mangle)?,
    })
}

fn rewrite_pattern(pattern: Pattern, private_mangle: &HashMap<String, String>) -> Pattern {
    match pattern {
        Pattern::Literal(e) => Pattern::Literal(rewrite_expression(e, private_mangle)),
        Pattern::Tuple { patterns } => Pattern::Tuple {
            patterns: patterns
                .into_iter()
                .map(|p| rewrite_pattern(p, private_mangle))
                .collect(),
        },
        Pattern::Range {
            start,
            end,
            inclusive,
        } => Pattern::Range {
            start: Box::new(rewrite_expression(*start, private_mangle)),
            end: Box::new(rewrite_expression(*end, private_mangle)),
            inclusive,
        },
        Pattern::Or { patterns } => Pattern::Or {
            patterns: patterns
                .into_iter()
                .map(|p| rewrite_pattern(p, private_mangle))
                .collect(),
        },
        Pattern::Struct {
            struct_name,
            fields,
            partial,
        } => Pattern::Struct {
            struct_name,
            fields: fields
                .into_iter()
                .map(|(name, pat)| (name, rewrite_pattern(pat, private_mangle)))
                .collect(),
            partial,
        },
        Pattern::Identifier(_) | Pattern::Wildcard | Pattern::EnumVariant { .. } => pattern,
    }
}

fn rewrite_expression(expr: Expression, private_mangle: &HashMap<String, String>) -> Expression {
    match expr {
        Expression::Call {
            callee,
            type_args,
            arguments,
        } => {
            let callee = match *callee {
                Expression::Identifier(name) => {
                    if let Some(m) = private_mangle.get(&name) {
                        Box::new(Expression::Identifier(m.clone()))
                    } else {
                        Box::new(Expression::Identifier(name))
                    }
                }
                other => Box::new(rewrite_expression(other, private_mangle)),
            };

            let arguments = arguments
                .into_iter()
                .map(|a| rewrite_expression(a, private_mangle))
                .collect();

            let type_args = type_args.map(|args| {
                args.into_iter()
                    .map(|t| rewrite_type(t, private_mangle))
                    .collect()
            });

            Expression::Call {
                callee,
                type_args,
                arguments,
            }
        }

        Expression::Array { elements } => Expression::Array {
            elements: elements
                .into_iter()
                .map(|e| rewrite_expression(e, private_mangle))
                .collect(),
        },

        Expression::Binary {
            left,
            operator,
            right,
        } => Expression::Binary {
            left: Box::new(rewrite_expression(*left, private_mangle)),
            operator,
            right: Box::new(rewrite_expression(*right, private_mangle)),
        },

        Expression::Unary { operator, operand } => Expression::Unary {
            operator,
            operand: Box::new(rewrite_expression(*operand, private_mangle)),
        },

        Expression::Index { array, index } => Expression::Index {
            array: Box::new(rewrite_expression(*array, private_mangle)),
            index: Box::new(rewrite_expression(*index, private_mangle)),
        },

        Expression::Slice { array, start, end } => Expression::Slice {
            array: Box::new(rewrite_expression(*array, private_mangle)),
            start: Box::new(rewrite_expression(*start, private_mangle)),
            end: Box::new(rewrite_expression(*end, private_mangle)),
        },

        Expression::MemberAccess { object, member } => Expression::MemberAccess {
            object: Box::new(rewrite_expression(*object, private_mangle)),
            member,
        },

        Expression::StructLiteral {
            name,
            type_args,
            fields,
        } => {
            let name = private_mangle.get(&name).cloned().unwrap_or(name);
            let fields = fields
                .into_iter()
                .map(|(n, e)| (n, rewrite_expression(e, private_mangle)))
                .collect();

            let type_args = type_args.map(|args| {
                args.into_iter()
                    .map(|t| rewrite_type(t, private_mangle))
                    .collect()
            });

            Expression::StructLiteral {
                name,
                type_args,
                fields,
            }
        }

        Expression::Assignment { target, value } => Expression::Assignment {
            target: Box::new(rewrite_expression(*target, private_mangle)),
            value: Box::new(rewrite_expression(*value, private_mangle)),
        },

        Expression::Reference { expression } => Expression::Reference {
            expression: Box::new(rewrite_expression(*expression, private_mangle)),
        },

        Expression::Dereference { expression } => Expression::Dereference {
            expression: Box::new(rewrite_expression(*expression, private_mangle)),
        },

        Expression::Await { expression } => Expression::Await {
            expression: Box::new(rewrite_expression(*expression, private_mangle)),
        },

        Expression::Spawn { body } => {
            let statements: Vec<_> = body
                .statements
                .into_iter()
                .filter_map(|s| rewrite_statement(Path::new("<spawn>"), s, private_mangle).ok())
                .collect();
            Expression::Spawn {
                body: Block::new(statements),
            }
        }

        Expression::Tuple { elements } => Expression::Tuple {
            elements: elements
                .into_iter()
                .map(|e| rewrite_expression(e, private_mangle))
                .collect(),
        },

        Expression::TupleIndex { tuple, index } => Expression::TupleIndex {
            tuple: Box::new(rewrite_expression(*tuple, private_mangle)),
            index,
        },

        Expression::Range {
            start,
            end,
            inclusive,
        } => Expression::Range {
            start: Box::new(rewrite_expression(*start, private_mangle)),
            end: Box::new(rewrite_expression(*end, private_mangle)),
            inclusive,
        },

        Expression::Try { expression } => Expression::Try {
            expression: Box::new(rewrite_expression(*expression, private_mangle)),
        },

        Expression::Closure {
            parameters,
            return_type,
            body,
            is_move,
        } => {
            let rewritten_body = match body {
                ClosureBody::Expression(expr) => {
                    ClosureBody::Expression(Box::new(rewrite_expression(*expr, private_mangle)))
                }
                ClosureBody::Block(block) => {
                    // Rewrite block statements directly
                    let dummy_path = Path::new("");
                    let rewritten_statements: Vec<Statement> = block
                        .statements
                        .into_iter()
                        .filter_map(|stmt| rewrite_statement(dummy_path, stmt, private_mangle).ok())
                        .collect();
                    ClosureBody::Block(Block::new(rewritten_statements))
                }
            };

            Expression::Closure {
                parameters,
                return_type,
                body: rewritten_body,
                is_move,
            }
        }

        Expression::IntLiteral(_)
        | Expression::FloatLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::BoolLiteral(_)
        | Expression::NullLiteral
        | Expression::Identifier(_)
        | Expression::EnumVariant { .. } => expr,
    }
}

fn first_private_type_reference(
    ty: &Type,
    private_mangle: &HashMap<String, String>,
) -> Option<String> {
    match ty {
        Type::Custom(name) => private_mangle.contains_key(name).then(|| name.clone()),
        Type::Array { element_type, .. } => {
            first_private_type_reference(element_type, private_mangle)
        }
        Type::Reference { inner_type, .. } => {
            first_private_type_reference(inner_type, private_mangle)
        }
        Type::Pointer { inner_type, .. } => {
            first_private_type_reference(inner_type, private_mangle)
        }
        Type::Generic { type_params, .. } => type_params
            .iter()
            .find_map(|t| first_private_type_reference(t, private_mangle)),
        _ => None,
    }
}

fn rewrite_type(ty: Type, private_mangle: &HashMap<String, String>) -> Type {
    match ty {
        Type::Custom(name) => {
            if let Some(m) = private_mangle.get(&name) {
                Type::Custom(m.clone())
            } else {
                Type::Custom(name)
            }
        }
        Type::Array { element_type, size } => Type::Array {
            element_type: Box::new(rewrite_type(*element_type, private_mangle)),
            size,
        },
        Type::Reference {
            inner_type,
            is_mutable,
        } => Type::Reference {
            inner_type: Box::new(rewrite_type(*inner_type, private_mangle)),
            is_mutable,
        },
        Type::Pointer {
            inner_type,
            is_mutable,
        } => Type::Pointer {
            inner_type: Box::new(rewrite_type(*inner_type, private_mangle)),
            is_mutable,
        },
        Type::Generic { name, type_params } => Type::Generic {
            name,
            type_params: type_params
                .into_iter()
                .map(|t| rewrite_type(t, private_mangle))
                .collect(),
        },
        _ => ty,
    }
}

fn resolve_import_path(
    importer_dir: &Path,
    project_root: &Path,
    path: &[String],
) -> Result<PathBuf> {
    let rel = path.iter().collect::<PathBuf>();

    let candidates = [
        importer_dir.join(&rel).with_extension("kr"),
        importer_dir.join(&rel).with_extension("krak"),
        project_root.join(&rel).with_extension("kr"),
        project_root.join(&rel).with_extension("krak"),
    ];

    for c in candidates {
        if c.exists() {
            return Ok(c);
        }
    }

    anyhow::bail!(
        "Import not found: {} (searched relative to {} and project root {})",
        path.join("."),
        importer_dir.display(),
        project_root.display()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn loader_expands_imports() -> Result<()> {
        let program =
            load_program(Path::new("../tests/programs/modules/simple_import_main.kr")).await?;

        let has_forty_two = program.statements.iter().any(|s| match s {
            Statement::FunctionDeclaration { name, .. } => name == "forty_two",
            _ => false,
        });

        if !has_forty_two {
            anyhow::bail!("Expected merged program to contain imported function forty_two");
        }

        Ok(())
    }
}
