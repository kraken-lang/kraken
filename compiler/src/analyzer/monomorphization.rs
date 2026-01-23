use crate::error::{CompilerError, CompilerResult, SourceLocation};
use crate::parser::ast::{
    Block, ClosureBody, EnumVariantPayload, Expression, Parameter, Pattern, Program, Statement,
    Type, WhereConstraint,
};
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;

fn type_mangle_part(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Bool => "bool".to_string(),
        Type::String => "string".to_string(),
        Type::Str => "str".to_string(),
        Type::Bytes => "bytes".to_string(),
        Type::Void => "void".to_string(),
        Type::VecInt => "VecInt".to_string(),
        Type::VecString => "VecString".to_string(),
        Type::VecBytes => "VecBytes".to_string(),
        Type::MapStringInt => "MapStringInt".to_string(),
        Type::MapStringString => "MapStringString".to_string(),
        Type::SliceInt => "SliceInt".to_string(),
        Type::SliceString => "SliceString".to_string(),
        Type::SliceBytes => "SliceBytes".to_string(),
        Type::Array { element_type, .. } => format!("arr_{}", type_mangle_part(element_type)),
        Type::Reference { inner_type, .. } => format!("ref_{}", type_mangle_part(inner_type)),
        Type::Pointer { inner_type, .. } => format!("ptr_{}", type_mangle_part(inner_type)),
        Type::RawPointer { inner_type, .. } => format!("raw_{}", type_mangle_part(inner_type)),
        Type::Custom(name) => format!("cust_{name}"),
        Type::Generic { name, type_params } => {
            let mut out = format!("gen_{name}");
            for p in type_params {
                out.push_str("__");
                out.push_str(&type_mangle_part(p));
            }
            out
        }
        Type::Tuple { element_types } => {
            let mut out = "tuple".to_string();
            for elem_ty in element_types {
                out.push_str("__");
                out.push_str(&type_mangle_part(elem_ty));
            }
            out
        }
        Type::Function {
            param_types,
            return_type,
        } => {
            let mut out = "fn".to_string();
            for param_ty in param_types {
                out.push_str("__");
                out.push_str(&type_mangle_part(param_ty));
            }
            out.push_str("__ret_");
            out.push_str(&type_mangle_part(return_type));
            out
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct InstKey {
    name: String,
    args: Vec<Type>,
}

pub fn monomorphize_program(program: Program, file_path: PathBuf) -> CompilerResult<Program> {
    let mut mono = Monomorphizer::new(file_path);
    mono.run(program)
}

struct Monomorphizer {
    file_path: PathBuf,
    generic_fns: HashMap<String, Statement>,
    generic_structs: HashMap<String, Statement>,
    generic_traits: HashMap<String, Statement>,
    trait_impls: Vec<Statement>,
    seen: HashSet<InstKey>,
    queue: VecDeque<InstKey>,
    inst_map: HashMap<InstKey, String>,
}

impl Monomorphizer {
    fn new(file_path: PathBuf) -> Self {
        Self {
            file_path,
            generic_fns: HashMap::new(),
            generic_structs: HashMap::new(),
            generic_traits: HashMap::new(),
            trait_impls: Vec::new(),
            seen: HashSet::new(),
            queue: VecDeque::new(),
            inst_map: HashMap::new(),
        }
    }

    fn run(&mut self, program: Program) -> CompilerResult<Program> {
        let mut program = program;
        // Step 1 (skeleton): collect generic templates.
        // Later steps will specialize these templates based on explicit call-site type arguments.
        for stmt in &program.statements {
            match stmt {
                Statement::FunctionDeclaration {
                    name,
                    generic_params,
                    ..
                } if !generic_params.is_empty() => {
                    self.generic_fns.insert(name.clone(), stmt.clone());
                }
                Statement::StructDeclaration {
                    name,
                    generic_params,
                    ..
                } if !generic_params.is_empty() => {
                    self.generic_structs.insert(name.clone(), stmt.clone());
                }
                Statement::TraitDeclaration {
                    name,
                    generic_params,
                    ..
                } if !generic_params.is_empty() => {
                    self.generic_traits.insert(name.clone(), stmt.clone());
                }
                Statement::TraitImpl { .. } => {
                    self.trait_impls.push(stmt.clone());
                }
                _ => {}
            }
        }

        // Infer missing call-site type arguments for generic function calls (e.g. id(1) -> id<int>(1)).
        // This keeps later stages unchanged: monomorphization still fully erases generics before type checking.
        self.infer_generic_call_sites(&mut program)?;

        // Discover explicit instantiations in the current program.
        for stmt in &program.statements {
            self.scan_statement(stmt)?;
        }

        // Drain instantiation queue, producing specialized declarations.
        let mut generated: Vec<Statement> = Vec::new();
        while let Some(key) = self.queue.pop_front() {
            let Some(mangled) = self.inst_map.get(&key).cloned() else {
                return Err(self.type_error("Missing instantiation mapping"));
            };

            if self.generic_fns.contains_key(&key.name) {
                let stmt = self.specialize_function(&key.name, &key.args, &mangled)?;
                self.scan_statement(&stmt)?;
                generated.push(stmt);
            } else if self.generic_structs.contains_key(&key.name) {
                let stmt = self.specialize_struct(&key.name, &key.args, &mangled)?;
                self.scan_statement(&stmt)?;
                generated.push(stmt);
            } else {
                return Err(
                    self.type_error(format!("No generic template found for '{}'", key.name))
                );
            }
        }

        // Rewrite the whole program to use specialized names.
        // Generic templates are removed from the output so later stages never see generics.
        let mut out: Vec<Statement> = Vec::new();
        for stmt in program.statements {
            if self.is_generic_template(&stmt) {
                continue;
            }
            out.push(self.rewrite_statement(stmt)?);
        }

        // Append generated specializations.
        // (We intentionally do not rewrite these again; they are generated already rewritten.)
        out.extend(generated);

        Ok(Program::new(out))
    }

    fn infer_generic_call_sites(&mut self, program: &mut Program) -> CompilerResult<()> {
        let mut env: HashMap<String, Type> = HashMap::new();
        for stmt in &mut program.statements {
            self.infer_statement(stmt, &mut env)?;
        }
        Ok(())
    }

    fn infer_statement(
        &mut self,
        stmt: &mut Statement,
        env: &mut HashMap<String, Type>,
    ) -> CompilerResult<()> {
        match stmt {
            Statement::VariableDeclaration {
                pattern,
                type_annotation,
                initializer,
                is_mutable: _,
            } => {
                if let Some(init) = initializer {
                    self.infer_expression(init, env)?;
                }
                // Bind pattern variables in the environment
                if let Some(ty) = type_annotation {
                    self.bind_pattern_to_env(pattern, ty.clone(), env);
                } else if let Some(_init) = initializer {
                    // For now, use Int as default type when no annotation
                    // TODO: Proper type inference from initializer
                    self.bind_pattern_to_env(pattern, Type::Int, env);
                }
                Ok(())
            }

            Statement::ConstantDeclaration {
                name,
                type_annotation,
                initializer,
            } => {
                self.infer_expression(initializer, env)?;
                if let Some(ty) = type_annotation.clone() {
                    env.insert(name.clone(), ty);
                } else if let Some(init_ty) = self.expression_type(initializer, env)? {
                    env.insert(name.clone(), init_ty);
                }
                Ok(())
            }

            Statement::FunctionDeclaration {
                parameters, body, ..
            } => {
                let mut fn_env = env.clone();
                for p in parameters {
                    // Bind parameter pattern to environment
                    self.bind_pattern_to_env(&p.pattern, p.param_type.clone(), &mut fn_env);
                }
                self.infer_block(body, &mut fn_env)
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.infer_expression(condition, env)?;
                let mut then_env = env.clone();
                self.infer_block(then_branch, &mut then_env)?;
                if let Some(else_b) = else_branch {
                    let mut else_env = env.clone();
                    self.infer_block(else_b, &mut else_env)?;
                }
                Ok(())
            }

            Statement::While { condition, body } => {
                self.infer_expression(condition, env)?;
                let mut loop_env = env.clone();
                self.infer_block(body, &mut loop_env)
            }

            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                let mut loop_env = env.clone();
                if let Some(init) = initializer {
                    self.infer_statement(init, &mut loop_env)?;
                }
                if let Some(cond) = condition {
                    self.infer_expression(cond, &mut loop_env)?;
                }
                if let Some(inc) = increment {
                    self.infer_expression(inc, &mut loop_env)?;
                }
                self.infer_block(body, &mut loop_env)
            }

            Statement::ForIn {
                variable,
                iterable,
                body,
            } => {
                self.infer_expression(iterable, env)?;
                let mut loop_env = env.clone();
                loop_env.insert(variable.clone(), Type::Int);
                self.infer_block(body, &mut loop_env)
            }

            Statement::Return { value } => {
                if let Some(v) = value {
                    self.infer_expression(v, env)?;
                }
                Ok(())
            }

            Statement::Expression(expr) => self.infer_expression(expr, env),

            Statement::Match { expression, arms } => {
                self.infer_expression(expression, env)?;
                for arm in arms {
                    let mut arm_env = env.clone();
                    self.infer_block(&mut arm.body, &mut arm_env)?;
                }
                Ok(())
            }

            Statement::Defer { statement } => self.infer_statement(statement, env),

            Statement::Unsafe { block } => {
                for stmt in &mut block.statements {
                    self.infer_statement(stmt, env)?;
                }
                Ok(())
            }

            Statement::Module { .. }
            | Statement::Import { .. }
            | Statement::StructDeclaration { .. }
            | Statement::ClassDeclaration { .. }
            | Statement::InterfaceDeclaration { .. }
            | Statement::EnumDeclaration { .. }
            | Statement::Break
            | Statement::Continue
            | Statement::TypeAlias { .. }
            | Statement::ImplBlock { .. }
            | Statement::TraitDeclaration { .. }
            | Statement::TraitImpl { .. } => Ok(()),
        }
    }

    fn infer_block(
        &mut self,
        block: &mut Block,
        env: &mut HashMap<String, Type>,
    ) -> CompilerResult<()> {
        let mut local = env.clone();
        for stmt in &mut block.statements {
            self.infer_statement(stmt, &mut local)?;
        }
        Ok(())
    }

    fn infer_expression(
        &mut self,
        expr: &mut Expression,
        env: &mut HashMap<String, Type>,
    ) -> CompilerResult<()> {
        match expr {
            Expression::Call {
                callee,
                type_args,
                arguments,
            } => {
                self.infer_expression(callee, env)?;
                for a in arguments.iter_mut() {
                    self.infer_expression(a, env)?;
                }

                if type_args.is_none() {
                    if let Expression::Identifier(name) = callee.as_ref() {
                        if let Some(template) = self.generic_fns.get(name).cloned() {
                            let inferred =
                                self.infer_generic_fn_call_args(&template, arguments, env)?;
                            *type_args = Some(inferred);
                        }
                    }
                }
                Ok(())
            }

            Expression::StructLiteral { fields, .. } => {
                for (_, v) in fields.iter_mut() {
                    self.infer_expression(v, env)?;
                }
                Ok(())
            }

            Expression::Binary { left, right, .. } => {
                self.infer_expression(left, env)?;
                self.infer_expression(right, env)
            }

            Expression::Unary { operand, .. }
            | Expression::Reference {
                expression: operand,
            }
            | Expression::Dereference {
                expression: operand,
            }
            | Expression::Await {
                expression: operand,
            } => self.infer_expression(operand, env),

            Expression::Array { elements } => {
                for e in elements.iter_mut() {
                    self.infer_expression(e, env)?;
                }
                Ok(())
            }

            Expression::Index { array, index } => {
                self.infer_expression(array, env)?;
                self.infer_expression(index, env)
            }

            Expression::Slice { array, start, end } => {
                self.infer_expression(array, env)?;
                self.infer_expression(start, env)?;
                self.infer_expression(end, env)
            }

            Expression::MemberAccess { object, .. } => self.infer_expression(object, env),

            Expression::Assignment { target, value } => {
                self.infer_expression(target, env)?;
                self.infer_expression(value, env)
            }

            Expression::Spawn { body } => self.infer_block(body, env),

            Expression::EnumVariant { payload, .. } => {
                if let Some(args) = payload {
                    for a in args.iter_mut() {
                        self.infer_expression(a, env)?;
                    }
                }
                Ok(())
            }

            Expression::Tuple { elements } => {
                for elem in elements.iter_mut() {
                    self.infer_expression(elem, env)?;
                }
                Ok(())
            }

            Expression::TupleIndex { tuple, .. } => self.infer_expression(tuple, env),

            Expression::Range {
                start,
                end,
                inclusive: _,
            } => {
                self.infer_expression(start, env)?;
                self.infer_expression(end, env)
            }

            Expression::Try { expression } => self.infer_expression(expression, env),

            Expression::Closure {
                parameters: _,
                return_type: _,
                body,
                is_move: _,
            } => {
                // Type check closure body
                match body {
                    ClosureBody::Expression(expr) => self.infer_expression(expr, env),
                    ClosureBody::Block(block) => {
                        for stmt in &mut block.statements {
                            self.infer_statement(stmt, env)?;
                        }
                        Ok(())
                    }
                }
            }

            Expression::IntLiteral(_)
            | Expression::FloatLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::BoolLiteral(_)
            | Expression::NullLiteral
            | Expression::Identifier(_) => Ok(()),
        }
    }

    fn infer_generic_fn_call_args(
        &mut self,
        template: &Statement,
        arguments: &[Expression],
        env: &HashMap<String, Type>,
    ) -> CompilerResult<Vec<Type>> {
        let Statement::FunctionDeclaration {
            name,
            generic_params,
            parameters,
            ..
        } = template
        else {
            return Err(self.type_error("Invalid function template AST"));
        };

        if generic_params.is_empty() {
            return Err(self.type_error("Attempted to infer type args for non-generic function"));
        }

        if arguments.len() != parameters.len() {
            return Err(self.type_error(format!(
                "Function '{name}' expects {} arguments, got {}",
                parameters.len(),
                arguments.len()
            )));
        }

        let mut subst: HashMap<String, Type> = HashMap::new();
        for (param, arg_expr) in parameters.iter().zip(arguments.iter()) {
            let Some(arg_ty) = self.expression_type(arg_expr, env)? else {
                return Err(self.type_error(format!(
                    "Cannot infer type arguments for generic function '{name}'.\n\
                     Hint: Argument types must be known. Consider:\n\
                     1. Adding explicit type annotations to variables\n\
                     2. Using turbofish syntax: {name}::<T>(...)\n\
                     3. Providing explicit type arguments: {name}<T>(...)"
                )));
            };
            self.unify_generic_param_types(&param.param_type, &arg_ty, generic_params, &mut subst)?;
        }

        let mut out: Vec<Type> = Vec::new();
        for gp in generic_params {
            let Some(bound) = subst.get(gp).cloned() else {
                return Err(self.type_error(format!(
                    "Cannot infer type parameter '{gp}' for generic function '{name}'.\n\
                     Hint: The type parameter could not be determined from the arguments.\n\
                     Use explicit type arguments: {name}::<{gp}>(...) or {name}<{gp}>(...)"
                )));
            };
            out.push(bound);
        }

        Ok(out)
    }

    fn unify_generic_param_types(
        &self,
        expected: &Type,
        actual: &Type,
        generic_params: &[String],
        subst: &mut HashMap<String, Type>,
    ) -> CompilerResult<()> {
        match expected {
            Type::Custom(name) if generic_params.iter().any(|p| p == name) => {
                if let Some(prev) = subst.get(name) {
                    if prev != actual {
                        return Err(self.type_error(format!(
                            "Conflicting inference for '{name}': saw '{prev}' and '{actual}'"
                        )));
                    }
                } else {
                    subst.insert(name.clone(), actual.clone());
                }
                Ok(())
            }

            Type::Generic {
                name: expected_name,
                type_params: expected_params,
            } => {
                let Type::Generic {
                    name: actual_name,
                    type_params: actual_params,
                } = actual
                else {
                    return Ok(());
                };

                if expected_name != actual_name {
                    return Ok(());
                }
                if expected_params.len() != actual_params.len() {
                    return Ok(());
                }
                for (e, a) in expected_params.iter().zip(actual_params.iter()) {
                    self.unify_generic_param_types(e, a, generic_params, subst)?;
                }
                Ok(())
            }

            Type::Array { element_type, .. } => {
                if let Type::Array {
                    element_type: actual_el,
                    ..
                } = actual
                {
                    self.unify_generic_param_types(element_type, actual_el, generic_params, subst)
                } else {
                    Ok(())
                }
            }

            Type::Reference { inner_type, .. } => {
                if let Type::Reference {
                    inner_type: actual_inner,
                    ..
                } = actual
                {
                    self.unify_generic_param_types(inner_type, actual_inner, generic_params, subst)
                } else {
                    Ok(())
                }
            }

            Type::Pointer { inner_type, .. } => {
                if let Type::Pointer {
                    inner_type: actual_inner,
                    ..
                } = actual
                {
                    self.unify_generic_param_types(inner_type, actual_inner, generic_params, subst)
                } else {
                    Ok(())
                }
            }

            _ => Ok(()),
        }
    }

    fn expression_type(
        &mut self,
        expr: &Expression,
        env: &HashMap<String, Type>,
    ) -> CompilerResult<Option<Type>> {
        match expr {
            Expression::IntLiteral(_) => Ok(Some(Type::Int)),
            Expression::FloatLiteral(_) => Ok(Some(Type::Float)),
            Expression::StringLiteral(_) => Ok(Some(Type::String)),
            Expression::BoolLiteral(_) => Ok(Some(Type::Bool)),
            Expression::NullLiteral => Ok(Some(Type::Void)),
            Expression::Identifier(name) => Ok(env.get(name).cloned()),

            Expression::Call {
                callee,
                type_args,
                arguments,
            } => {
                if let Expression::Identifier(name) = callee.as_ref() {
                    if let Some(template) = self.generic_fns.get(name).cloned() {
                        let Statement::FunctionDeclaration {
                            generic_params,
                            parameters: _,
                            return_type,
                            ..
                        } = &template
                        else {
                            return Err(self.type_error("Invalid function template AST"));
                        };

                        let args = if let Some(explicit) = type_args {
                            explicit.clone()
                        } else {
                            self.infer_generic_fn_call_args(&template, arguments, env)?
                        };

                        let subst = self.build_subst_map(name, generic_params, &args)?;
                        let ret = return_type.clone().unwrap_or(Type::Void);
                        let ret = self.rewrite_type_with_subst(ret, &subst);
                        return Ok(Some(ret));
                    }
                }

                Ok(None)
            }

            _ => Ok(None),
        }
    }

    fn is_generic_template(&self, stmt: &Statement) -> bool {
        match stmt {
            Statement::FunctionDeclaration { generic_params, .. } => !generic_params.is_empty(),
            Statement::StructDeclaration { generic_params, .. } => !generic_params.is_empty(),
            _ => false,
        }
    }

    fn build_subst_map(
        &self,
        name: &str,
        params: &[String],
        args: &[Type],
    ) -> CompilerResult<HashMap<String, Type>> {
        if params.len() != args.len() {
            return Err(self.type_error(format!(
                "Generic '{name}' expects {} type arguments, got {}",
                params.len(),
                args.len()
            )));
        }

        Ok(params.iter().cloned().zip(args.iter().cloned()).collect())
    }

    fn specialize_function(
        &mut self,
        name: &str,
        args: &[Type],
        mangled: &str,
    ) -> CompilerResult<Statement> {
        let Some(template) = self.generic_fns.get(name).cloned() else {
            return Err(self.type_error(format!("Missing generic function template '{name}'")));
        };

        let Statement::FunctionDeclaration {
            name: _,
            generic_params,
            where_constraints,
            parameters,
            return_type,
            body,
            is_async,
            is_unsafe,
            is_public,
        } = template
        else {
            return Err(self.type_error("Invalid function template AST"));
        };

        let subst = self.build_subst_map(name, &generic_params, args)?;
        self.enforce_where_constraints(name, &where_constraints, &subst)?;

        let parameters = parameters
            .into_iter()
            .map(|mut p| {
                p.param_type = self.rewrite_type_with_subst(p.param_type, &subst);
                p
            })
            .collect();

        let return_type = return_type.map(|t| self.rewrite_type_with_subst(t, &subst));

        let body = self.rewrite_block_with_subst(body, &subst)?;

        Ok(Statement::FunctionDeclaration {
            name: mangled.to_string(),
            generic_params: Vec::new(),
            where_constraints: Vec::new(),
            parameters,
            return_type,
            body,
            is_async,
            is_unsafe,
            is_public,
        })
    }

    fn specialize_struct(
        &mut self,
        name: &str,
        args: &[Type],
        mangled: &str,
    ) -> CompilerResult<Statement> {
        let Some(template) = self.generic_structs.get(name).cloned() else {
            return Err(self.type_error(format!("Missing generic struct template '{name}'")));
        };

        let Statement::StructDeclaration {
            name: _,
            generic_params,
            where_constraints,
            fields,
            is_public,
        } = template
        else {
            return Err(self.type_error("Invalid struct template AST"));
        };

        let subst = self.build_subst_map(name, &generic_params, args)?;
        self.enforce_where_constraints(name, &where_constraints, &subst)?;

        let fields = fields
            .into_iter()
            .map(|mut f| {
                f.field_type = self.rewrite_type_with_subst(f.field_type, &subst);
                f
            })
            .collect();

        Ok(Statement::StructDeclaration {
            name: mangled.to_string(),
            generic_params: Vec::new(),
            where_constraints: Vec::new(),
            fields,
            is_public,
        })
    }

    fn enforce_where_constraints(
        &self,
        name: &str,
        constraints: &[WhereConstraint],
        subst: &HashMap<String, Type>,
    ) -> CompilerResult<()> {
        for c in constraints {
            let Some(concrete) = subst.get(&c.type_param) else {
                return Err(self.type_error(format!(
                    "Cannot enforce where-clause for '{name}': missing type argument for '{}'",
                    c.type_param
                )));
            };

            match c.trait_name.as_str() {
                "Clone" => {
                    if !Self::is_cloneable_type(concrete) {
                        return Err(self.type_error(format!(
                            "Generic '{name}' requires {}: Clone, but got '{concrete}'",
                            c.type_param
                        )));
                    }
                }
                _ => {
                    return Err(self.type_error(format!(
                        "Unsupported trait constraint '{}' in where-clause of '{name}'",
                        c.trait_name
                    )));
                }
            }
        }

        Ok(())
    }

    fn is_cloneable_type(ty: &Type) -> bool {
        match ty {
            Type::Void => false,
            Type::Array { element_type, .. } => Self::is_cloneable_type(element_type),
            Type::Reference { inner_type, .. } => Self::is_cloneable_type(inner_type),
            Type::Pointer { inner_type, .. } => Self::is_cloneable_type(inner_type),
            _ => true,
        }
    }

    fn rewrite_block_with_subst(
        &mut self,
        block: Block,
        subst: &HashMap<String, Type>,
    ) -> CompilerResult<Block> {
        let mut out: Vec<Statement> = Vec::new();
        for s in block.statements {
            out.push(self.rewrite_statement_with_subst(s, subst)?);
        }
        Ok(Block::new(out))
    }

    fn rewrite_statement(&mut self, stmt: Statement) -> CompilerResult<Statement> {
        self.rewrite_statement_with_subst(stmt, &HashMap::new())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn rewrite_statement_with_subst(
        &mut self,
        stmt: Statement,
        subst: &HashMap<String, Type>,
    ) -> CompilerResult<Statement> {
        match stmt {
            Statement::Module { .. } | Statement::Import { .. } => Ok(stmt),

            Statement::VariableDeclaration {
                pattern,
                type_annotation,
                initializer,
                is_mutable,
            } => Ok(Statement::VariableDeclaration {
                pattern: self.rewrite_pattern_with_subst(pattern, subst),
                type_annotation: type_annotation.map(|t| self.rewrite_type_with_subst(t, subst)),
                initializer: initializer
                    .map(|e| self.rewrite_expression_with_subst(e, subst))
                    .transpose()?,
                is_mutable,
            }),

            Statement::ConstantDeclaration {
                name,
                type_annotation,
                initializer,
            } => Ok(Statement::ConstantDeclaration {
                name,
                type_annotation: type_annotation.map(|t| self.rewrite_type_with_subst(t, subst)),
                initializer: self.rewrite_expression_with_subst(initializer, subst)?,
            }),

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
                let parameters = parameters
                    .into_iter()
                    .map(|mut p| {
                        p.param_type = self.rewrite_type_with_subst(p.param_type, subst);
                        p
                    })
                    .collect();

                let return_type = return_type.map(|t| self.rewrite_type_with_subst(t, subst));
                let body = self.rewrite_block_with_subst(body, subst)?;

                Ok(Statement::FunctionDeclaration {
                    name,
                    generic_params,
                    where_constraints,
                    parameters,
                    return_type,
                    body,
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
                let fields = fields
                    .into_iter()
                    .map(|mut f| {
                        f.field_type = self.rewrite_type_with_subst(f.field_type, subst);
                        f
                    })
                    .collect();
                Ok(Statement::StructDeclaration {
                    name,
                    generic_params,
                    where_constraints,
                    fields,
                    is_public,
                })
            }

            Statement::ClassDeclaration {
                name,
                fields,
                methods,
                is_public,
            } => {
                let fields = fields
                    .into_iter()
                    .map(|mut f| {
                        f.field_type = self.rewrite_type_with_subst(f.field_type, subst);
                        f
                    })
                    .collect();
                let methods = methods
                    .into_iter()
                    .map(|m| self.rewrite_statement_with_subst(m, subst))
                    .collect::<CompilerResult<Vec<_>>>()?;
                Ok(Statement::ClassDeclaration {
                    name,
                    fields,
                    methods,
                    is_public,
                })
            }

            Statement::InterfaceDeclaration { .. } | Statement::EnumDeclaration { .. } => Ok(stmt),

            Statement::Return { value } => Ok(Statement::Return {
                value: value
                    .map(|e| self.rewrite_expression_with_subst(e, subst))
                    .transpose()?,
            }),

            Statement::Expression(e) => Ok(Statement::Expression(
                self.rewrite_expression_with_subst(e, subst)?,
            )),

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => Ok(Statement::If {
                condition: self.rewrite_expression_with_subst(condition, subst)?,
                then_branch: self.rewrite_block_with_subst(then_branch, subst)?,
                else_branch: match else_branch {
                    Some(b) => Some(self.rewrite_block_with_subst(b, subst)?),
                    None => None,
                },
            }),

            Statement::While { condition, body } => Ok(Statement::While {
                condition: self.rewrite_expression_with_subst(condition, subst)?,
                body: self.rewrite_block_with_subst(body, subst)?,
            }),

            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => Ok(Statement::For {
                initializer: match initializer {
                    Some(s) => Some(Box::new(self.rewrite_statement_with_subst(*s, subst)?)),
                    None => None,
                },
                condition: condition
                    .map(|e| self.rewrite_expression_with_subst(e, subst))
                    .transpose()?,
                increment: increment
                    .map(|e| self.rewrite_expression_with_subst(e, subst))
                    .transpose()?,
                body: self.rewrite_block_with_subst(body, subst)?,
            }),

            Statement::ForIn {
                variable,
                iterable,
                body,
            } => Ok(Statement::ForIn {
                variable,
                iterable: self.rewrite_expression_with_subst(iterable, subst)?,
                body: self.rewrite_block_with_subst(body, subst)?,
            }),

            Statement::Match { expression, arms } => {
                let expression = self.rewrite_expression_with_subst(expression, subst)?;
                let arms = arms
                    .into_iter()
                    .map(|mut a| {
                        a.body = self.rewrite_block_with_subst(a.body, subst)?;
                        Ok(a)
                    })
                    .collect::<CompilerResult<Vec<_>>>()?;
                Ok(Statement::Match { expression, arms })
            }

            Statement::Defer { statement } => Ok(Statement::Defer {
                statement: Box::new(self.rewrite_statement_with_subst(*statement, subst)?),
            }),

            Statement::Unsafe { block } => Ok(Statement::Unsafe {
                block: self.rewrite_block_with_subst(block, subst)?,
            }),

            Statement::Break
            | Statement::Continue
            | Statement::TypeAlias { .. }
            | Statement::ImplBlock { .. }
            | Statement::TraitDeclaration { .. }
            | Statement::TraitImpl { .. } => Ok(stmt),
        }
    }

    fn rewrite_expression_with_subst(
        &mut self,
        expr: Expression,
        subst: &HashMap<String, Type>,
    ) -> CompilerResult<Expression> {
        match expr {
            Expression::Call {
                callee,
                type_args,
                arguments,
            } => {
                let callee = self.rewrite_expression_with_subst(*callee, subst)?;
                let arguments = arguments
                    .into_iter()
                    .map(|a| self.rewrite_expression_with_subst(a, subst))
                    .collect::<CompilerResult<Vec<_>>>()?;

                if let (Expression::Identifier(name), Some(args)) = (&callee, &type_args) {
                    let args = args
                        .iter()
                        .cloned()
                        .map(|t| self.rewrite_type_with_subst(t, subst))
                        .collect::<Vec<_>>();
                    let key = InstKey {
                        name: name.clone(),
                        args,
                    };
                    let Some(mangled) = self.inst_map.get(&key).cloned() else {
                        return Err(self.type_error(format!(
                            "Missing specialization for call to '{}'",
                            key.name
                        )));
                    };
                    return Ok(Expression::Call {
                        callee: Box::new(Expression::Identifier(mangled)),
                        type_args: None,
                        arguments,
                    });
                }

                Ok(Expression::Call {
                    callee: Box::new(callee),
                    type_args: type_args.map(|args| {
                        args.into_iter()
                            .map(|t| self.rewrite_type_with_subst(t, subst))
                            .collect()
                    }),
                    arguments,
                })
            }

            Expression::StructLiteral {
                name,
                type_args,
                fields,
            } => {
                let fields = fields
                    .into_iter()
                    .map(|(n, e)| {
                        let e = self.rewrite_expression_with_subst(e, subst)?;
                        Ok((n, e))
                    })
                    .collect::<CompilerResult<Vec<_>>>()?;

                if let Some(args) = type_args {
                    let args = args
                        .into_iter()
                        .map(|t| self.rewrite_type_with_subst(t, subst))
                        .collect::<Vec<_>>();
                    let key = InstKey { name, args };
                    let Some(mangled) = self.inst_map.get(&key).cloned() else {
                        return Err(self.type_error(format!(
                            "Missing specialization for struct '{}'",
                            key.name
                        )));
                    };
                    return Ok(Expression::StructLiteral {
                        name: mangled,
                        type_args: None,
                        fields,
                    });
                }

                Ok(Expression::StructLiteral {
                    name,
                    type_args: None,
                    fields,
                })
            }

            Expression::Binary {
                left,
                operator,
                right,
            } => Ok(Expression::Binary {
                left: Box::new(self.rewrite_expression_with_subst(*left, subst)?),
                operator,
                right: Box::new(self.rewrite_expression_with_subst(*right, subst)?),
            }),

            Expression::Unary { operator, operand } => Ok(Expression::Unary {
                operator,
                operand: Box::new(self.rewrite_expression_with_subst(*operand, subst)?),
            }),

            Expression::Array { elements } => Ok(Expression::Array {
                elements: elements
                    .into_iter()
                    .map(|e| self.rewrite_expression_with_subst(e, subst))
                    .collect::<CompilerResult<Vec<_>>>()?,
            }),

            Expression::Index { array, index } => Ok(Expression::Index {
                array: Box::new(self.rewrite_expression_with_subst(*array, subst)?),
                index: Box::new(self.rewrite_expression_with_subst(*index, subst)?),
            }),

            Expression::Slice { array, start, end } => Ok(Expression::Slice {
                array: Box::new(self.rewrite_expression_with_subst(*array, subst)?),
                start: Box::new(self.rewrite_expression_with_subst(*start, subst)?),
                end: Box::new(self.rewrite_expression_with_subst(*end, subst)?),
            }),

            Expression::MemberAccess { object, member } => Ok(Expression::MemberAccess {
                object: Box::new(self.rewrite_expression_with_subst(*object, subst)?),
                member,
            }),

            Expression::Assignment { target, value } => Ok(Expression::Assignment {
                target: Box::new(self.rewrite_expression_with_subst(*target, subst)?),
                value: Box::new(self.rewrite_expression_with_subst(*value, subst)?),
            }),

            Expression::Reference { expression } => Ok(Expression::Reference {
                expression: Box::new(self.rewrite_expression_with_subst(*expression, subst)?),
            }),

            Expression::Dereference { expression } => Ok(Expression::Dereference {
                expression: Box::new(self.rewrite_expression_with_subst(*expression, subst)?),
            }),

            Expression::Await { expression } => Ok(Expression::Await {
                expression: Box::new(self.rewrite_expression_with_subst(*expression, subst)?),
            }),

            Expression::Spawn { body } => Ok(Expression::Spawn {
                body: self.rewrite_block_with_subst(body, subst)?,
            }),

            Expression::EnumVariant {
                enum_name,
                variant_name,
                payload,
            } => Ok(Expression::EnumVariant {
                enum_name,
                variant_name,
                payload: match payload {
                    Some(p) => Some(
                        p.into_iter()
                            .map(|e| self.rewrite_expression_with_subst(e, subst))
                            .collect::<CompilerResult<Vec<_>>>()?,
                    ),
                    None => None,
                },
            }),

            Expression::Tuple { elements } => Ok(Expression::Tuple {
                elements: elements
                    .into_iter()
                    .map(|e| self.rewrite_expression_with_subst(e, subst))
                    .collect::<CompilerResult<Vec<_>>>()?,
            }),

            Expression::TupleIndex { tuple, index } => Ok(Expression::TupleIndex {
                tuple: Box::new(self.rewrite_expression_with_subst(*tuple, subst)?),
                index,
            }),

            Expression::Range {
                start,
                end,
                inclusive,
            } => Ok(Expression::Range {
                start: Box::new(self.rewrite_expression_with_subst(*start, subst)?),
                end: Box::new(self.rewrite_expression_with_subst(*end, subst)?),
                inclusive,
            }),

            Expression::Try { expression } => Ok(Expression::Try {
                expression: Box::new(self.rewrite_expression_with_subst(*expression, subst)?),
            }),

            Expression::Closure {
                parameters,
                return_type,
                body,
                is_move,
            } => {
                // Rewrite parameter types
                let rewritten_params = parameters
                    .into_iter()
                    .map(|p| Parameter {
                        pattern: p.pattern,
                        param_type: self.rewrite_type_with_subst(p.param_type, subst),
                        is_reference: p.is_reference,
                    })
                    .collect();

                // Rewrite return type
                let rewritten_return = return_type.map(|t| self.rewrite_type_with_subst(t, subst));

                // Rewrite body
                let rewritten_body = match body {
                    ClosureBody::Expression(expr) => ClosureBody::Expression(Box::new(
                        self.rewrite_expression_with_subst(*expr, subst)?,
                    )),
                    ClosureBody::Block(block) => {
                        ClosureBody::Block(self.rewrite_block_with_subst(block, subst)?)
                    }
                };

                Ok(Expression::Closure {
                    parameters: rewritten_params,
                    return_type: rewritten_return,
                    body: rewritten_body,
                    is_move,
                })
            }

            Expression::IntLiteral(_)
            | Expression::FloatLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::BoolLiteral(_)
            | Expression::NullLiteral
            | Expression::Identifier(_) => Ok(expr),
        }
    }

    fn rewrite_type_with_subst(&mut self, ty: Type, subst: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Custom(name) => subst.get(&name).cloned().unwrap_or(Type::Custom(name)),

            Type::Generic { name, type_params } => {
                let args = type_params
                    .into_iter()
                    .map(|t| self.rewrite_type_with_subst(t, subst))
                    .collect::<Vec<_>>();

                if let Some(lowered) = self.lower_builtin_generic_container(&name, &args) {
                    return lowered;
                }

                let key = InstKey {
                    name: name.clone(),
                    args,
                };
                if let Some(mangled) = self.inst_map.get(&key).cloned() {
                    Type::Custom(mangled)
                } else {
                    Type::Generic {
                        name,
                        type_params: key.args,
                    }
                }
            }

            Type::Array { element_type, size } => Type::Array {
                element_type: Box::new(self.rewrite_type_with_subst(*element_type, subst)),
                size,
            },

            Type::Reference {
                inner_type,
                is_mutable,
            } => Type::Reference {
                inner_type: Box::new(self.rewrite_type_with_subst(*inner_type, subst)),
                is_mutable,
            },

            Type::Pointer {
                inner_type,
                is_mutable,
            } => Type::Pointer {
                inner_type: Box::new(self.rewrite_type_with_subst(*inner_type, subst)),
                is_mutable,
            },

            Type::Tuple { element_types } => Type::Tuple {
                element_types: element_types
                    .into_iter()
                    .map(|t| self.rewrite_type_with_subst(t, subst))
                    .collect(),
            },

            Type::Function {
                param_types,
                return_type,
            } => Type::Function {
                param_types: param_types
                    .into_iter()
                    .map(|t| self.rewrite_type_with_subst(t, subst))
                    .collect(),
                return_type: Box::new(self.rewrite_type_with_subst(*return_type, subst)),
            },

            other => other,
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn rewrite_pattern_with_subst(
        &mut self,
        pattern: Pattern,
        subst: &HashMap<String, Type>,
    ) -> Pattern {
        match pattern {
            Pattern::Tuple { patterns } => Pattern::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|p| self.rewrite_pattern_with_subst(p, subst))
                    .collect(),
            },
            other => other,
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn bind_pattern_to_env(&self, pattern: &Pattern, ty: Type, env: &mut HashMap<String, Type>) {
        match pattern {
            Pattern::Identifier(name) => {
                env.insert(name.clone(), ty);
            }
            Pattern::Tuple { patterns } => {
                if let Type::Tuple { element_types } = ty {
                    for (pat, elem_ty) in patterns.iter().zip(element_types.iter()) {
                        self.bind_pattern_to_env(pat, elem_ty.clone(), env);
                    }
                }
            }
            Pattern::Wildcard
            | Pattern::Literal(_)
            | Pattern::EnumVariant { .. }
            | Pattern::Range { .. } => {
                // These don't bind variables
            }
            Pattern::Or { patterns } => {
                // Or patterns: bind variables from all alternatives
                for pat in patterns {
                    self.bind_pattern_to_env(pat, ty.clone(), env);
                }
            }
            Pattern::Struct {
                struct_name: _,
                fields: _,
                partial: _,
            } => {
                // Struct patterns: bind variables from field patterns
                // Note: Type checking has already validated the struct type and fields
                // For monomorphization, we don't need to do anything special here
                // as the type information is already resolved
            }
        }
    }

    fn enqueue_instantiation(&mut self, name: &str, args: &[Type]) {
        let key = InstKey {
            name: name.to_string(),
            args: args.to_vec(),
        };

        if !self.seen.insert(key.clone()) {
            return;
        }

        if self.inst_map.contains_key(&key) {
            return;
        }

        let mangled = self.mangle_name(&key.name, &key.args);
        self.inst_map.insert(key.clone(), mangled);
        self.queue.push_back(key);
    }

    fn scan_statement(&mut self, stmt: &Statement) -> CompilerResult<()> {
        self.scan_statement_for_generics(stmt, &HashMap::new())
    }

    fn scan_block(&mut self, block: &Block) -> CompilerResult<()> {
        for stmt in &block.statements {
            self.scan_statement_for_generics(stmt, &HashMap::new())?;
        }
        Ok(())
    }

    fn scan_statement_for_generics(
        &mut self,
        stmt: &Statement,
        _generics: &HashMap<String, Type>,
    ) -> CompilerResult<()> {
        match stmt {
            Statement::Module { .. } | Statement::Import { .. } => Ok(()),

            Statement::VariableDeclaration {
                pattern: _,
                type_annotation,
                initializer,
                is_mutable: _,
            } => {
                if let Some(t) = type_annotation {
                    self.scan_type(t)?;
                }
                if let Some(init) = initializer {
                    self.scan_expression(init)?;
                }
                Ok(())
            }

            Statement::ConstantDeclaration {
                type_annotation,
                initializer,
                ..
            } => {
                if let Some(t) = type_annotation {
                    self.scan_type(t)?;
                }
                self.scan_expression(initializer)
            }

            Statement::FunctionDeclaration { body, .. } => self.scan_block(body),

            Statement::StructDeclaration { fields, .. }
            | Statement::ClassDeclaration { fields, .. } => {
                for f in fields {
                    self.scan_type(&f.field_type)?;
                }
                Ok(())
            }

            Statement::Return { value } => {
                if let Some(v) = value {
                    self.scan_expression(v)?;
                }
                Ok(())
            }

            Statement::Expression(e) => self.scan_expression(e),

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.scan_expression(condition)?;
                self.scan_block(then_branch)?;
                if let Some(b) = else_branch {
                    self.scan_block(b)?;
                }
                Ok(())
            }

            Statement::While { condition, body } => {
                self.scan_expression(condition)?;
                self.scan_block(body)
            }

            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = initializer {
                    self.scan_statement(init)?;
                }
                if let Some(cond) = condition {
                    self.scan_expression(cond)?;
                }
                if let Some(inc) = increment {
                    self.scan_expression(inc)?;
                }
                self.scan_block(body)
            }

            Statement::Match { expression, arms } => {
                self.scan_expression(expression)?;
                for arm in arms {
                    self.scan_block(&arm.body)?;
                }
                Ok(())
            }

            Statement::Defer { statement } => self.scan_statement(statement),

            Statement::Unsafe { block } => {
                for stmt in &block.statements {
                    self.scan_statement(stmt)?;
                }
                Ok(())
            }

            Statement::EnumDeclaration { variants, .. } => {
                for (_, payload) in variants {
                    if let Some(payload) = payload {
                        match payload {
                            EnumVariantPayload::Tuple(types) => {
                                for t in types {
                                    self.scan_type(t)?;
                                }
                            }
                            EnumVariantPayload::Struct(fields) => {
                                for (_, field_type) in fields {
                                    self.scan_type(field_type)?;
                                }
                            }
                        }
                    }
                }
                Ok(())
            }

            Statement::InterfaceDeclaration { methods, .. } => {
                for m in methods {
                    for p in &m.parameters {
                        self.scan_type(&p.param_type)?;
                    }
                    if let Some(rt) = &m.return_type {
                        self.scan_type(rt)?;
                    }
                }
                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),

            Statement::ForIn {
                variable: _,
                iterable,
                body,
            } => {
                self.scan_expression(iterable)?;
                self.scan_block(body)?;
                Ok(())
            }

            Statement::TypeAlias { .. }
            | Statement::ImplBlock { .. }
            | Statement::TraitDeclaration { .. }
            | Statement::TraitImpl { .. } => Ok(()),
        }
    }

    fn scan_expression(&mut self, expr: &Expression) -> CompilerResult<()> {
        match expr {
            Expression::Call {
                callee,
                type_args,
                arguments,
            } => {
                if let (Expression::Identifier(name), Some(args)) = (callee.as_ref(), type_args) {
                    if !self.generic_fns.contains_key(name) {
                        return Err(self
                            .type_error(format!("Generic call to non-generic function '{name}'")));
                    }
                    self.enqueue_instantiation(name, args);
                }
                if let Some(args) = type_args {
                    for t in args {
                        self.scan_type(t)?;
                    }
                }
                for a in arguments {
                    self.scan_expression(a)?;
                }
                Ok(())
            }

            Expression::StructLiteral {
                name,
                type_args,
                fields,
            } => {
                if let Some(args) = type_args {
                    if !self.generic_structs.contains_key(name) {
                        return Err(self.type_error(format!(
                            "Generic instantiation of non-generic struct '{name}'"
                        )));
                    }
                    self.enqueue_instantiation(name, args);
                    for t in args {
                        self.scan_type(t)?;
                    }
                }
                for (_, v) in fields {
                    self.scan_expression(v)?;
                }
                Ok(())
            }

            Expression::Binary { left, right, .. } => {
                self.scan_expression(left)?;
                self.scan_expression(right)
            }

            Expression::Unary { operand, .. }
            | Expression::Reference {
                expression: operand,
            }
            | Expression::Dereference {
                expression: operand,
            }
            | Expression::Await {
                expression: operand,
            } => self.scan_expression(operand),

            Expression::Array { elements } => {
                for el in elements {
                    self.scan_expression(el)?;
                }
                Ok(())
            }

            Expression::Index { array, index } => {
                self.scan_expression(array)?;
                self.scan_expression(index)
            }

            Expression::Slice { array, start, end } => {
                self.scan_expression(array)?;
                self.scan_expression(start)?;
                self.scan_expression(end)
            }

            Expression::MemberAccess { object, .. } => self.scan_expression(object),

            Expression::Assignment { target, value } => {
                self.scan_expression(target)?;
                self.scan_expression(value)
            }

            Expression::Spawn { body } => self.scan_block(body),

            Expression::EnumVariant { payload, .. } => {
                if let Some(args) = payload {
                    for a in args {
                        self.scan_expression(a)?;
                    }
                }
                Ok(())
            }

            Expression::Tuple { elements } => {
                for elem in elements {
                    self.scan_expression(elem)?;
                }
                Ok(())
            }

            Expression::TupleIndex { tuple, .. } => self.scan_expression(tuple),

            Expression::Range {
                start,
                end,
                inclusive: _,
            } => {
                self.scan_expression(start)?;
                self.scan_expression(end)?;
                Ok(())
            }

            Expression::Try { expression } => self.scan_expression(expression),

            Expression::Closure {
                parameters: _,
                return_type: _,
                body,
                is_move: _,
            } => match body {
                ClosureBody::Expression(expr) => self.scan_expression(expr),
                ClosureBody::Block(block) => self.scan_block(block),
            },

            Expression::IntLiteral(_)
            | Expression::FloatLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::BoolLiteral(_)
            | Expression::NullLiteral
            | Expression::Identifier(_) => Ok(()),
        }
    }

    fn scan_type(&mut self, ty: &Type) -> CompilerResult<()> {
        match ty {
            Type::Generic { name, type_params } => {
                if self
                    .lower_builtin_generic_container(name, type_params)
                    .is_some()
                {
                    // Builtin container shims (Vec<T>/Map<K,V>) are lowered later; they are not
                    // monomorphized as user-defined generic structs.
                    for p in type_params {
                        self.scan_type(p)?;
                    }
                    return Ok(());
                }
                if !self.generic_structs.contains_key(name) {
                    return Err(self.type_error(format!(
                        "Generic type '{name}<...>' refers to a non-generic type"
                    )));
                }
                self.enqueue_instantiation(name, type_params);
                for p in type_params {
                    self.scan_type(p)?;
                }
                Ok(())
            }
            Type::Array { element_type, .. } => self.scan_type(element_type),
            Type::Reference { inner_type, .. } => self.scan_type(inner_type),
            Type::Pointer { inner_type, .. } => self.scan_type(inner_type),
            Type::Tuple { element_types } => {
                for elem_ty in element_types {
                    self.scan_type(elem_ty)?;
                }
                Ok(())
            }
            Type::Function {
                param_types,
                return_type,
            } => {
                for param_ty in param_types {
                    self.scan_type(param_ty)?;
                }
                self.scan_type(return_type)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn lower_builtin_generic_container(&self, name: &str, params: &[Type]) -> Option<Type> {
        match name {
            "Vec" => {
                if params.len() != 1 {
                    return None;
                }
                match &params[0] {
                    Type::Int => Some(Type::VecInt),
                    Type::String => Some(Type::VecString),
                    Type::Bytes => Some(Type::VecBytes),
                    _ => None,
                }
            }
            "Map" => {
                if params.len() != 2 {
                    return None;
                }
                match (&params[0], &params[1]) {
                    (Type::String, Type::Int) => Some(Type::MapStringInt),
                    (Type::String, Type::String) => Some(Type::MapStringString),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn type_error(&self, msg: impl Into<String>) -> CompilerError {
        CompilerError::type_error(SourceLocation::new(self.file_path.clone(), 0, 0), msg)
    }

    fn mangle_name(&self, name: &str, args: &[Type]) -> String {
        let mut out = String::new();
        out.push_str(name);
        for a in args {
            out.push_str("__");
            out.push_str(&type_mangle_part(a));
        }
        out
    }
}
