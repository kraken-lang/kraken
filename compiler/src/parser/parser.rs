use crate::parser::ast::{
    Block, ClosureBody, EnumVariantPayload, Expression, FunctionSignature, MatchArm, Parameter,
    Pattern, Program, Statement, StructField, Type, WhereConstraint,
};
use crate::{
    error::{CompilerError, CompilerResult, SourceLocation},
    lexer::token::{Keyword, Operator, Token, TokenKind},
};
use std::path::PathBuf;

/// Recursive descent parser for Kraken language.
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    file_path: PathBuf,
}

impl Parser {
    /// Create a new parser from tokens.
    pub fn new(tokens: Vec<Token>, file_path: PathBuf) -> Self {
        Self {
            tokens,
            current: 0,
            file_path,
        }
    }

    /// Parse the token stream into an AST.
    ///
    /// # Returns
    /// The parsed program AST
    ///
    /// # Errors
    /// Returns `CompilerError::ParserError` if parsing fails
    pub fn parse(&mut self) -> CompilerResult<Program> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        Ok(Program::new(statements))
    }

    /// Parse a statement.
    fn parse_statement(&mut self) -> CompilerResult<Statement> {
        // Check for keywords that start statements
        if self.match_keyword(Keyword::Let) {
            self.parse_variable_declaration(false)
        } else if self.match_keyword(Keyword::Const) {
            self.parse_constant_declaration()
        } else if self.match_keyword(Keyword::Async) {
            // async fn declaration
            self.expect_keyword(Keyword::Fn)?;
            self.parse_function_declaration(true, false)
        } else if self.match_keyword(Keyword::Fn) {
            self.parse_function_declaration(false, false)
        } else if self.match_keyword(Keyword::Module) {
            self.parse_module_statement()
        } else if self.match_keyword(Keyword::Import) {
            self.parse_import_statement()
        } else if self.match_keyword(Keyword::Pub) {
            self.parse_public_declaration()
        } else if self.match_keyword(Keyword::Struct) {
            self.parse_struct_declaration(false)
        } else if self.match_keyword(Keyword::Enum) {
            self.parse_enum_declaration(false)
        } else if self.match_keyword(Keyword::Type) {
            self.parse_type_alias(false)
        } else if self.match_keyword(Keyword::Impl) {
            self.parse_impl_block()
        } else if self.match_keyword(Keyword::Class) {
            self.parse_class_declaration(false)
        } else if self.match_keyword(Keyword::Interface) {
            self.parse_interface_declaration()
        } else if self.match_keyword(Keyword::Return) {
            self.parse_return_statement()
        } else if self.match_keyword(Keyword::If) {
            self.parse_if_statement()
        } else if self.match_keyword(Keyword::While) {
            self.parse_while_statement()
        } else if self.match_keyword(Keyword::For) {
            self.parse_for_statement()
        } else if self.match_keyword(Keyword::Match) {
            self.parse_match_statement()
        } else if self.match_keyword(Keyword::Break) {
            self.consume_semicolon()?;
            Ok(Statement::Break)
        } else if self.match_keyword(Keyword::Continue) {
            self.consume_semicolon()?;
            Ok(Statement::Continue)
        } else if self.match_keyword(Keyword::Defer) {
            self.parse_defer_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    /// Parse a variable declaration.
    fn parse_variable_declaration(&mut self, is_mutable: bool) -> CompilerResult<Statement> {
        // Parse pattern - either simple identifier or tuple destructuring
        let pattern = if self.check_token(TokenKind::LeftParen) {
            // Tuple destructuring: let (x, y) = ...
            self.advance();
            let mut patterns = Vec::new();

            if !self.check_token(TokenKind::RightParen) {
                patterns.push(self.parse_destructuring_pattern()?);
                while self.match_token(TokenKind::Comma) {
                    if self.check_token(TokenKind::RightParen) {
                        break;
                    }
                    patterns.push(self.parse_destructuring_pattern()?);
                }
            }

            self.expect_token(TokenKind::RightParen)?;
            Pattern::Tuple { patterns }
        } else {
            // Simple identifier
            let name = self.consume_identifier()?;
            Pattern::Identifier(name)
        };

        let type_annotation = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let initializer = if self.match_operator(Operator::Assign) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume_semicolon()?;

        Ok(Statement::VariableDeclaration {
            pattern,
            type_annotation,
            initializer,
            is_mutable,
        })
    }

    fn parse_destructuring_pattern(&mut self) -> CompilerResult<Pattern> {
        if self.check_token(TokenKind::LeftParen) {
            // Nested tuple pattern
            self.advance();
            let mut patterns = Vec::new();

            if !self.check_token(TokenKind::RightParen) {
                patterns.push(self.parse_destructuring_pattern()?);
                while self.match_token(TokenKind::Comma) {
                    if self.check_token(TokenKind::RightParen) {
                        break;
                    }
                    patterns.push(self.parse_destructuring_pattern()?);
                }
            }

            self.expect_token(TokenKind::RightParen)?;
            Ok(Pattern::Tuple { patterns })
        } else if self.check_token(TokenKind::Identifier) {
            let name = self.consume_identifier()?;
            if name == "_" {
                Ok(Pattern::Wildcard)
            } else {
                Ok(Pattern::Identifier(name))
            }
        } else {
            Err(self.error("Expected identifier or tuple pattern"))
        }
    }

    fn parse_import_statement(&mut self) -> CompilerResult<Statement> {
        let mut path = Vec::new();
        path.push(self.consume_identifier()?);
        while self.match_token(TokenKind::Dot) {
            path.push(self.consume_identifier()?);
        }

        self.consume_semicolon()?;
        Ok(Statement::Import { path })
    }

    fn parse_module_statement(&mut self) -> CompilerResult<Statement> {
        let mut path = Vec::new();
        path.push(self.consume_identifier()?);
        while self.match_token(TokenKind::Dot) {
            path.push(self.consume_identifier()?);
        }

        self.consume_semicolon()?;
        Ok(Statement::Module { path })
    }

    /// Parse a constant declaration.
    fn parse_constant_declaration(&mut self) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;

        let type_annotation = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect_operator(Operator::Assign)?;
        let initializer = self.parse_expression()?;
        self.consume_semicolon()?;

        Ok(Statement::ConstantDeclaration {
            name,
            type_annotation,
            initializer,
        })
    }

    /// Parse a function declaration.
    fn parse_function_declaration(
        &mut self,
        is_async: bool,
        is_public: bool,
    ) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;

        let generic_params = if self.match_operator(Operator::Less) {
            let mut params = Vec::new();
            params.push(self.consume_identifier()?);
            while self.match_token(TokenKind::Comma) {
                params.push(self.consume_identifier()?);
            }
            self.expect_operator(Operator::Greater)?;
            params
        } else {
            Vec::new()
        };

        self.expect_token(TokenKind::LeftParen)?;
        let parameters = self.parse_parameter_list()?;
        self.expect_token(TokenKind::RightParen)?;

        let return_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let where_constraints = if self.match_keyword(Keyword::Where) {
            self.parse_where_constraints()?
        } else {
            Vec::new()
        };

        let body = self.parse_block()?;

        Ok(Statement::FunctionDeclaration {
            name,
            generic_params,
            where_constraints,
            parameters,
            return_type,
            body,
            is_async,
            is_public,
        })
    }

    /// Parse a public declaration.
    fn parse_public_declaration(&mut self) -> CompilerResult<Statement> {
        if self.match_keyword(Keyword::Fn) {
            self.parse_function_declaration(false, true)
        } else if self.match_keyword(Keyword::Struct) {
            self.parse_struct_declaration(true)
        } else if self.match_keyword(Keyword::Enum) {
            self.parse_enum_declaration(true)
        } else if self.match_keyword(Keyword::Type) {
            self.parse_type_alias(true)
        } else if self.match_keyword(Keyword::Class) {
            self.parse_class_declaration(true)
        } else {
            Err(self.error("Expected fn, struct, enum, or class after pub"))
        }
    }

    /// Parse a struct declaration.
    fn parse_struct_declaration(&mut self, is_public: bool) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;

        let generic_params = if self.match_operator(Operator::Less) {
            let mut params = Vec::new();
            params.push(self.consume_identifier()?);
            while self.match_token(TokenKind::Comma) {
                params.push(self.consume_identifier()?);
            }
            self.expect_operator(Operator::Greater)?;
            params
        } else {
            Vec::new()
        };

        let where_constraints = if self.match_keyword(Keyword::Where) {
            self.parse_where_constraints()?
        } else {
            Vec::new()
        };

        self.expect_token(TokenKind::LeftBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(TokenKind::RightBrace)?;

        Ok(Statement::StructDeclaration {
            name,
            generic_params,
            where_constraints,
            fields,
            is_public,
        })
    }

    fn parse_where_constraints(&mut self) -> CompilerResult<Vec<WhereConstraint>> {
        let mut constraints = Vec::new();

        loop {
            let type_param = self.consume_identifier()?;
            self.expect_token(TokenKind::Colon)?;
            let trait_name = self.consume_identifier()?;
            constraints.push(WhereConstraint {
                type_param,
                trait_name,
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(constraints)
    }

    /// Parse an enum declaration.
    fn parse_enum_declaration(&mut self, is_public: bool) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;

        self.expect_token(TokenKind::LeftBrace)?;
        let variants = self.parse_enum_variants()?;
        self.expect_token(TokenKind::RightBrace)?;

        Ok(Statement::EnumDeclaration {
            name,
            variants,
            is_public,
        })
    }

    /// Parse enum variants.
    fn parse_enum_variants(&mut self) -> CompilerResult<Vec<(String, Option<EnumVariantPayload>)>> {
        let mut variants = Vec::new();

        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
            let variant_name = self.consume_identifier()?;

            // Check for payload types: VariantName(Type1, Type2, ...) or VariantName { field: Type, ... }
            let payload = if self.match_token(TokenKind::LeftParen) {
                // Tuple payload
                let mut types = Vec::new();
                if !self.check_token(TokenKind::RightParen) {
                    types.push(self.parse_type()?);
                    while self.match_token(TokenKind::Comma) {
                        types.push(self.parse_type()?);
                    }
                }
                self.expect_token(TokenKind::RightParen)?;
                Some(EnumVariantPayload::Tuple(types))
            } else if self.match_token(TokenKind::LeftBrace) {
                // Struct payload
                let mut fields = Vec::new();
                while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
                    let field_name = self.consume_identifier()?;
                    self.expect_token(TokenKind::Colon)?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name, field_type));

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect_token(TokenKind::RightBrace)?;
                Some(EnumVariantPayload::Struct(fields))
            } else {
                None
            };

            variants.push((variant_name, payload));

            // Variants separated by commas (optional trailing comma)
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(variants)
    }

    /// Parse struct fields.
    fn parse_struct_fields(&mut self) -> CompilerResult<Vec<StructField>> {
        let mut fields = Vec::new();

        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
            let is_public = self.match_keyword(Keyword::Pub);
            let name = self.consume_identifier()?;
            self.expect_token(TokenKind::Colon)?;
            let field_type = self.parse_type()?;
            self.consume_semicolon()?;

            fields.push(StructField {
                name,
                field_type,
                is_public,
            });
        }

        Ok(fields)
    }

    /// Parse a class declaration.
    fn parse_class_declaration(&mut self, is_public: bool) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;

        self.expect_token(TokenKind::LeftBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
            if self.check_keyword(Keyword::Fn) {
                self.advance();
                methods.push(self.parse_function_declaration(false, false)?);
            } else {
                let is_field_public = self.match_keyword(Keyword::Pub);
                let field_name = self.consume_identifier()?;
                self.expect_token(TokenKind::Colon)?;
                let field_type = self.parse_type()?;
                self.consume_semicolon()?;

                fields.push(StructField {
                    name: field_name,
                    field_type,
                    is_public: is_field_public,
                });
            }
        }

        self.expect_token(TokenKind::RightBrace)?;

        Ok(Statement::ClassDeclaration {
            name,
            fields,
            methods,
            is_public,
        })
    }

    /// Parse an interface declaration.
    fn parse_interface_declaration(&mut self) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;

        self.expect_token(TokenKind::LeftBrace)?;
        let mut methods = Vec::new();

        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
            self.expect_keyword(Keyword::Fn)?;
            let method_name = self.consume_identifier()?;

            self.expect_token(TokenKind::LeftParen)?;
            let parameters = self.parse_parameter_list()?;
            self.expect_token(TokenKind::RightParen)?;

            let return_type = if self.match_token(TokenKind::Arrow) {
                Some(self.parse_type()?)
            } else {
                None
            };

            self.consume_semicolon()?;

            methods.push(FunctionSignature {
                name: method_name,
                parameters,
                return_type,
            });
        }

        self.expect_token(TokenKind::RightBrace)?;

        Ok(Statement::InterfaceDeclaration { name, methods })
    }

    /// Parse a type alias: type MyInt = int;
    fn parse_type_alias(&mut self, is_public: bool) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;

        // Parse optional generic parameters: type Result<T> = ...
        let generic_params = if self.match_token(TokenKind::Operator(Operator::Less)) {
            let mut params = Vec::new();
            params.push(self.consume_identifier()?);
            while self.match_token(TokenKind::Comma) {
                params.push(self.consume_identifier()?);
            }
            self.expect_token(TokenKind::Operator(Operator::Greater))?;
            params
        } else {
            Vec::new()
        };

        self.expect_token(TokenKind::Operator(Operator::Assign))?;
        let target_type = self.parse_type()?;
        self.consume_semicolon()?;

        Ok(Statement::TypeAlias {
            name,
            generic_params,
            target_type,
            is_public,
        })
    }

    /// Parse an impl block: impl TypeName { ... }
    fn parse_impl_block(&mut self) -> CompilerResult<Statement> {
        // Parse optional generic parameters: impl<T> Vec<T> { ... }
        let generic_params = if self.match_token(TokenKind::Operator(Operator::Less)) {
            let mut params = Vec::new();
            params.push(self.consume_identifier()?);
            while self.match_token(TokenKind::Comma) {
                params.push(self.consume_identifier()?);
            }
            self.expect_token(TokenKind::Operator(Operator::Greater))?;
            params
        } else {
            Vec::new()
        };

        let type_name = self.consume_identifier()?;

        // Skip generic type arguments if present: impl Vec<T> { ... }
        if self.match_token(TokenKind::Operator(Operator::Less)) {
            // Skip until we find the matching >
            let mut depth = 1;
            while depth > 0 && !self.is_at_end() {
                if self.check_token(TokenKind::Operator(Operator::Less)) {
                    depth += 1;
                } else if self.check_token(TokenKind::Operator(Operator::Greater)) {
                    depth -= 1;
                }
                self.advance();
            }
        }

        self.expect_token(TokenKind::LeftBrace)?;

        let mut methods = Vec::new();
        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
            if self.match_keyword(Keyword::Fn) {
                methods.push(self.parse_function_declaration(false, false)?);
            } else if self.match_keyword(Keyword::Pub) {
                self.expect_keyword(Keyword::Fn)?;
                methods.push(self.parse_function_declaration(false, true)?);
            } else {
                return Err(self.error("Expected fn in impl block"));
            }
        }

        self.expect_token(TokenKind::RightBrace)?;

        Ok(Statement::ImplBlock {
            type_name,
            generic_params,
            methods,
        })
    }

    /// Parse a return statement.
    fn parse_return_statement(&mut self) -> CompilerResult<Statement> {
        let value = if self.check_token(TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.consume_semicolon()?;

        Ok(Statement::Return { value })
    }

    /// Parse an if statement.
    fn parse_if_statement(&mut self) -> CompilerResult<Statement> {
        self.expect_token(TokenKind::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::RightParen)?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.match_keyword(Keyword::Else) {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    /// Parse a while statement.
    fn parse_while_statement(&mut self) -> CompilerResult<Statement> {
        self.expect_token(TokenKind::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::RightParen)?;

        let body = self.parse_block()?;

        Ok(Statement::While { condition, body })
    }

    /// Parse a for statement (C-style or for-in).
    fn parse_for_statement(&mut self) -> CompilerResult<Statement> {
        self.expect_token(TokenKind::LeftParen)?;

        // Check if this is a for-in loop: for (x in range)
        if self.check_token(TokenKind::Identifier) {
            let checkpoint = self.current;
            let var_name = self.consume_identifier()?;

            if self.match_keyword(Keyword::In) {
                // This is a for-in loop
                let iterable = self.parse_expression()?;
                self.expect_token(TokenKind::RightParen)?;
                let body = self.parse_block()?;

                return Ok(Statement::ForIn {
                    variable: var_name,
                    iterable,
                    body,
                });
            } else {
                // Not a for-in loop, restore position and parse as C-style for
                self.current = checkpoint;
            }
        }

        // Parse C-style for loop
        let initializer = if self.check_token(TokenKind::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_statement()?))
        };

        let condition = if self.check_token(TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume_semicolon()?;

        let increment = if self.check_token(TokenKind::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect_token(TokenKind::RightParen)?;

        let body = self.parse_block()?;

        Ok(Statement::For {
            initializer,
            condition,
            increment,
            body,
        })
    }

    /// Parse a match statement.
    fn parse_match_statement(&mut self) -> CompilerResult<Statement> {
        self.expect_token(TokenKind::LeftParen)?;
        let expression = self.parse_expression()?;
        self.expect_token(TokenKind::RightParen)?;

        self.expect_token(TokenKind::LeftBrace)?;
        let mut arms = Vec::new();

        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
            let pattern = self.parse_pattern()?;

            // Check for guard clause: pattern if condition
            let guard = if self.match_keyword(Keyword::If) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            self.expect_token(TokenKind::Arrow)?;
            let body = self.parse_block()?;

            arms.push(MatchArm {
                pattern,
                guard,
                body,
            });
        }

        self.expect_token(TokenKind::RightBrace)?;

        Ok(Statement::Match { expression, arms })
    }

    /// Parse a defer statement.
    fn parse_defer_statement(&mut self) -> CompilerResult<Statement> {
        let statement = Box::new(self.parse_statement()?);
        Ok(Statement::Defer { statement })
    }

    /// Parse an expression statement.
    fn parse_expression_statement(&mut self) -> CompilerResult<Statement> {
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Statement::Expression(expr))
    }

    /// Parse a block of statements.
    fn parse_block(&mut self) -> CompilerResult<Block> {
        self.expect_token(TokenKind::LeftBrace)?;
        let mut statements = Vec::new();

        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        self.expect_token(TokenKind::RightBrace)?;

        Ok(Block::new(statements))
    }

    /// Parse parameter list.
    fn parse_parameter_list(&mut self) -> CompilerResult<Vec<Parameter>> {
        let mut parameters = Vec::new();

        if !self.check_token(TokenKind::RightParen) {
            loop {
                let is_reference = self.match_keyword(Keyword::Ref);

                // Parse pattern for parameter (supports destructuring)
                let pattern = self.parse_pattern()?;

                self.expect_token(TokenKind::Colon)?;
                let param_type = self.parse_type()?;

                parameters.push(Parameter {
                    pattern,
                    param_type,
                    is_reference,
                });

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        Ok(parameters)
    }

    /// Parse a type annotation.
    fn parse_type(&mut self) -> CompilerResult<Type> {
        // Parse function types: fn(int, string) -> bool
        if self.match_keyword(Keyword::Fn) {
            self.expect_token(TokenKind::LeftParen)?;
            
            let mut param_types = Vec::new();
            if !self.check_token(TokenKind::RightParen) {
                param_types.push(self.parse_type()?);
                while self.match_token(TokenKind::Comma) {
                    param_types.push(self.parse_type()?);
                }
            }
            
            self.expect_token(TokenKind::RightParen)?;
            self.expect_token(TokenKind::Arrow)?;
            let return_type = Box::new(self.parse_type()?);
            
            return Ok(Type::Function {
                param_types,
                return_type,
            });
        }

        if let Some(keyword) = self.current_keyword() {
            if let Some(base_type) = Type::from_keyword(keyword) {
                self.advance();
                return Ok(base_type);
            }
        }

        // Parse tuple types: (int, string, bool)
        if self.match_token(TokenKind::LeftParen) {
            let mut element_types = Vec::new();

            // Empty tuple () is unit type
            if self.check_token(TokenKind::RightParen) {
                self.advance();
                return Ok(Type::Tuple { element_types });
            }

            // Parse first type
            element_types.push(self.parse_type()?);

            // Parse remaining types
            while self.match_token(TokenKind::Comma) {
                // Allow trailing comma
                if self.check_token(TokenKind::RightParen) {
                    break;
                }
                element_types.push(self.parse_type()?);
            }

            self.expect_token(TokenKind::RightParen)?;
            return Ok(Type::Tuple { element_types });
        }

        if self.match_token(TokenKind::LeftBracket) {
            let element_type = Box::new(self.parse_type()?);

            let size = if self.match_token(TokenKind::Semicolon) {
                if let TokenKind::IntLiteral = self.peek().kind {
                    let size_str = self.peek().lexeme.clone();
                    self.advance();
                    Some(
                        size_str
                            .parse::<usize>()
                            .map_err(|_| self.error("Invalid array size"))?,
                    )
                } else {
                    return Err(self.error("Expected array size"));
                }
            } else {
                None
            };

            self.expect_token(TokenKind::RightBracket)?;

            return Ok(Type::Array { element_type, size });
        }

        if self.match_operator(Operator::Ampersand) {
            let is_mutable = self.match_keyword(Keyword::Mut);
            let inner_type = Box::new(self.parse_type()?);
            return Ok(Type::Reference {
                inner_type,
                is_mutable,
            });
        }

        let name = self.consume_identifier()?;

        if self.match_operator(Operator::Less) {
            let mut type_params = Vec::new();
            type_params.push(self.parse_type()?);
            while self.match_token(TokenKind::Comma) {
                type_params.push(self.parse_type()?);
            }
            self.expect_operator(Operator::Greater)?;
            Ok(Type::Generic { name, type_params })
        } else {
            Ok(Type::Custom(name))
        }
    }

    /// Parse a pattern for match expressions.
    fn parse_pattern(&mut self) -> CompilerResult<Pattern> {
        let pattern = self.parse_pattern_base()?;

        // Check for or pattern: pattern | pattern | pattern
        if self.match_operator(Operator::BitOr) {
            let mut patterns = vec![pattern];
            patterns.push(self.parse_pattern_base()?);

            while self.match_operator(Operator::BitOr) {
                patterns.push(self.parse_pattern_base()?);
            }

            return Ok(Pattern::Or { patterns });
        }

        Ok(pattern)
    }

    /// Parse a base pattern (without or patterns).
    fn parse_pattern_base(&mut self) -> CompilerResult<Pattern> {
        if self.check_token(TokenKind::LeftParen) {
            // Tuple pattern: (x, y, z) or (1, _, x)
            self.advance();
            let mut patterns = Vec::new();

            if !self.check_token(TokenKind::RightParen) {
                patterns.push(self.parse_pattern()?);
                while self.match_token(TokenKind::Comma) {
                    if self.check_token(TokenKind::RightParen) {
                        break;
                    }
                    patterns.push(self.parse_pattern()?);
                }
            }

            self.expect_token(TokenKind::RightParen)?;
            Ok(Pattern::Tuple { patterns })
        } else if self.check_token(TokenKind::Identifier) {
            let name = self.consume_identifier()?;
            if name == "_" {
                Ok(Pattern::Wildcard)
            } else if self.match_token(TokenKind::ColonColon) {
                // Enum variant pattern: EnumName::VariantName or EnumName::VariantName(bindings)
                let variant_name = self.consume_identifier()?;
                let bindings = if self.match_token(TokenKind::LeftParen) {
                    let mut bindings = Vec::new();
                    if !self.check_token(TokenKind::RightParen) {
                        bindings.push(self.consume_identifier()?);
                        while self.match_token(TokenKind::Comma) {
                            bindings.push(self.consume_identifier()?);
                        }
                    }
                    self.expect_token(TokenKind::RightParen)?;
                    bindings
                } else {
                    Vec::new()
                };
                Ok(Pattern::EnumVariant {
                    enum_name: name,
                    variant_name,
                    bindings,
                })
            } else if self.match_token(TokenKind::LeftBrace) {
                // Struct pattern: StructName { field1, field2, .. }
                let struct_name = name;
                let mut fields = Vec::new();
                let mut partial = false;

                if !self.check_token(TokenKind::RightBrace) {
                    loop {
                        // Check for partial pattern (..)
                        if self.match_operator(Operator::DotDot) {
                            partial = true;
                            break;
                        }

                        // Parse field pattern: field or field: pattern
                        let field_name = self.consume_identifier()?;
                        let field_pattern = if self.match_token(TokenKind::Colon) {
                            self.parse_pattern()?
                        } else {
                            // Shorthand: field means field: field
                            Pattern::Identifier(field_name.clone())
                        };

                        fields.push((field_name, field_pattern));

                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }

                        // Allow trailing comma before }
                        if self.check_token(TokenKind::RightBrace) {
                            break;
                        }
                    }
                }

                self.expect_token(TokenKind::RightBrace)?;
                Ok(Pattern::Struct {
                    struct_name,
                    fields,
                    partial,
                })
            } else {
                Ok(Pattern::Identifier(name))
            }
        } else {
            // Parse literal or range pattern
            let expr = self.parse_primary()?;

            // Check if this is a range pattern
            if let Some(op) = self.match_operators(&[Operator::DotDot, Operator::DotDotEqual]) {
                let inclusive = op == Operator::DotDotEqual;
                let end = self.parse_primary()?;
                return Ok(Pattern::Range {
                    start: Box::new(expr),
                    end: Box::new(end),
                    inclusive,
                });
            }

            Ok(Pattern::Literal(expr))
        }
    }

    /// Parse an expression.
    fn parse_expression(&mut self) -> CompilerResult<Expression> {
        self.parse_assignment()
    }

    /// Parse assignment expression.
    fn parse_assignment(&mut self) -> CompilerResult<Expression> {
        let expr = self.parse_logical_or()?;

        if self.match_operator(Operator::Assign) {
            let value = Box::new(self.parse_assignment()?);
            return Ok(Expression::Assignment {
                target: Box::new(expr),
                value,
            });
        }

        Ok(expr)
    }

    /// Parse logical OR expression.
    fn parse_logical_or(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_logical_and()?;

        while self.match_operator(Operator::Or) {
            let operator = Operator::Or;
            let right = self.parse_logical_and()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse logical AND expression.
    fn parse_logical_and(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_bitwise_or()?;

        while self.match_operator(Operator::And) {
            let operator = Operator::And;
            let right = self.parse_bitwise_or()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse bitwise OR expression.
    fn parse_bitwise_or(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_bitwise_xor()?;

        while self.match_operator(Operator::BitOr) {
            let operator = Operator::BitOr;
            let right = self.parse_bitwise_xor()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse bitwise XOR expression.
    fn parse_bitwise_xor(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_bitwise_and()?;

        while self.match_operator(Operator::BitXor) {
            let operator = Operator::BitXor;
            let right = self.parse_bitwise_and()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse bitwise AND expression.
    fn parse_bitwise_and(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_equality()?;

        while self.match_operator(Operator::BitAnd) {
            let operator = Operator::BitAnd;
            let right = self.parse_equality()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse equality expression.
    fn parse_equality(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_comparison()?;

        while let Some(op) = self.match_operators(&[Operator::Equal, Operator::NotEqual]) {
            let right = self.parse_comparison()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse comparison expression.
    fn parse_comparison(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_range()?;

        while let Some(op) = self.match_operators(&[
            Operator::Less,
            Operator::LessEqual,
            Operator::Greater,
            Operator::GreaterEqual,
        ]) {
            let right = self.parse_range()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse range expression (.. or ..=).
    fn parse_range(&mut self) -> CompilerResult<Expression> {
        let left = self.parse_shift()?;

        if let Some(op) = self.match_operators(&[Operator::DotDot, Operator::DotDotEqual]) {
            let inclusive = op == Operator::DotDotEqual;
            let right = self.parse_shift()?;
            return Ok(Expression::Range {
                start: Box::new(left),
                end: Box::new(right),
                inclusive,
            });
        }

        Ok(left)
    }

    /// Parse shift expression (<<, >>).
    fn parse_shift(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_term()?;

        while let Some(op) = self.match_operators(&[Operator::LeftShift, Operator::RightShift]) {
            let right = self.parse_term()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse term expression (addition, subtraction).
    fn parse_term(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_factor()?;

        while let Some(op) = self.match_operators(&[Operator::Plus, Operator::Minus]) {
            let right = self.parse_factor()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse factor expression (multiplication, division, modulo).
    fn parse_factor(&mut self) -> CompilerResult<Expression> {
        let mut left = self.parse_unary()?;

        while let Some(op) =
            self.match_operators(&[Operator::Star, Operator::Slash, Operator::Percent])
        {
            let right = self.parse_unary()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse unary expression.
    fn parse_unary(&mut self) -> CompilerResult<Expression> {
        // Handle await expression
        if self.match_keyword(Keyword::Await) {
            let expression = Box::new(self.parse_unary()?);
            return Ok(Expression::Await { expression });
        }

        // Handle spawn expression
        if self.match_keyword(Keyword::Spawn) {
            // parse_block expects to consume the opening brace itself
            let body = self.parse_block()?;
            return Ok(Expression::Spawn { body });
        }

        if let Some(op) = self.match_operators(&[
            Operator::Not,
            Operator::Minus,
            Operator::BitNot,
            Operator::BitAnd,
            Operator::Star,
        ]) {
            let operand = Box::new(self.parse_unary()?);

            return Ok(match op {
                Operator::BitAnd => Expression::Reference {
                    expression: operand,
                },
                Operator::Star => Expression::Dereference {
                    expression: operand,
                },
                _ => Expression::Unary {
                    operator: op,
                    operand,
                },
            });
        }

        self.parse_postfix()
    }

    /// Parse postfix expression (calls, indexing, member access).
    fn parse_postfix(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_primary()?;

        loop {
            // Turbofish syntax: Identifier::<T>(...) - unambiguous
            if matches!(self.peek().kind, TokenKind::ColonColon) {
                if let Expression::Identifier(name) = &expr {
                    self.advance(); // Consume '::'
                    
                    if !matches!(self.peek().kind, TokenKind::Operator(Operator::Less)) {
                        return Err(self.error("Expected '<' after '::' in turbofish syntax"));
                    }
                    
                    self.advance(); // Consume '<'
                    
                    let mut type_args = Vec::new();
                    type_args.push(self.parse_type()?);
                    while self.match_token(TokenKind::Comma) {
                        type_args.push(self.parse_type()?);
                    }
                    self.expect_operator(Operator::Greater)?;

                    // Must be followed by '(' for function call or '{' for struct literal
                    if self.match_token(TokenKind::LeftParen) {
                        let mut arguments = Vec::new();
                        if !self.check_token(TokenKind::RightParen) {
                            arguments.push(self.parse_expression()?);
                            while self.match_token(TokenKind::Comma) {
                                arguments.push(self.parse_expression()?);
                            }
                        }
                        self.expect_token(TokenKind::RightParen)?;
                        expr = Expression::Call {
                            callee: Box::new(Expression::Identifier(name.clone())),
                            type_args: Some(type_args),
                            arguments,
                        };
                        continue;
                    } else if self.match_token(TokenKind::LeftBrace) {
                        let mut fields = Vec::new();
                        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
                            let field_name = self.consume_identifier()?;
                            self.expect_token(TokenKind::Colon)?;
                            let field_value = self.parse_expression()?;
                            fields.push((field_name, field_value));
                            if !self.match_token(TokenKind::Comma) {
                                break;
                            }
                        }
                        self.expect_token(TokenKind::RightBrace)?;
                        expr = Expression::StructLiteral {
                            name: name.clone(),
                            type_args: Some(type_args),
                            fields,
                        };
                        continue;
                    } else {
                        return Err(self.error("Expected '(' or '{' after turbofish type arguments"));
                    }
                }
            }
            
            // Generic call/struct literal syntax:
            //   Identifier<T>(...) or Identifier<T> { ... }
            // Only attempted when the next token is '<' and the base expression is an identifier.
            if matches!(self.peek().kind, TokenKind::Operator(Operator::Less)) {
                if let Expression::Identifier(name) = &expr {
                    let saved = self.current;

                    // Consume '<'
                    self.advance();

                    // Parse type argument list (speculatively). If anything fails, roll back.
                    let type_args = (|| {
                        let mut type_args = Vec::new();
                        type_args.push(self.parse_type()?);
                        while self.match_token(TokenKind::Comma) {
                            type_args.push(self.parse_type()?);
                        }
                        self.expect_operator(Operator::Greater)?;
                        Ok::<_, CompilerError>(type_args)
                    })();

                    let type_args = match type_args {
                        Ok(v) => v,
                        Err(_) => {
                            self.current = saved;
                            Vec::new()
                        }
                    };

                    if self.current != saved && self.check_token(TokenKind::LeftParen) {
                        self.advance();
                        let arguments = self.parse_argument_list()?;
                        self.expect_token(TokenKind::RightParen)?;
                        expr = Expression::Call {
                            callee: Box::new(Expression::Identifier(name.clone())),
                            type_args: Some(type_args),
                            arguments,
                        };
                        continue;
                    } else if self.current != saved && self.check_token(TokenKind::LeftBrace) {
                        self.advance(); // consume '{'
                        let mut fields = Vec::new();

                        while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
                            let field_name = self.consume_identifier()?;
                            self.expect_token(TokenKind::Colon)?;
                            let field_value = self.parse_expression()?;
                            fields.push((field_name, field_value));

                            if !self.match_token(TokenKind::Comma) {
                                break;
                            }
                        }

                        self.expect_token(TokenKind::RightBrace)?;
                        expr = Expression::StructLiteral {
                            name: name.clone(),
                            type_args: Some(type_args),
                            fields,
                        };
                        continue;
                    } else if self.current != saved {
                        // Parsed type args, but not followed by '(' or '{' => not a generic call/struct literal.
                        self.current = saved;
                    }
                }
            }

            if self.match_token(TokenKind::LeftParen) {
                let arguments = self.parse_argument_list()?;
                self.expect_token(TokenKind::RightParen)?;
                expr = Expression::Call {
                    callee: Box::new(expr),
                    type_args: None,
                    arguments,
                };
            } else if self.match_token(TokenKind::LeftBracket) {
                let start_or_index = self.parse_expression()?;

                // Check for slice syntax: x[start:end]
                if self.match_token(TokenKind::Colon) {
                    let end = self.parse_expression()?;
                    self.expect_token(TokenKind::RightBracket)?;
                    expr = Expression::Slice {
                        array: Box::new(expr),
                        start: Box::new(start_or_index),
                        end: Box::new(end),
                    };
                } else {
                    self.expect_token(TokenKind::RightBracket)?;
                    expr = Expression::Index {
                        array: Box::new(expr),
                        index: Box::new(start_or_index),
                    };
                }
            } else if self.match_token(TokenKind::Dot) {
                // Check if this is tuple indexing (.0, .1, etc) or member access
                if let TokenKind::IntLiteral = self.peek().kind {
                    let index_str = self.peek().lexeme.clone();
                    let index = index_str
                        .parse::<usize>()
                        .map_err(|_| self.error("Invalid tuple index"))?;
                    self.advance();
                    expr = Expression::TupleIndex {
                        tuple: Box::new(expr),
                        index,
                    };
                } else {
                    let member = self.consume_identifier()?;
                    expr = Expression::MemberAccess {
                        object: Box::new(expr),
                        member,
                    };
                }
            } else if self.match_token(TokenKind::ColonColon) {
                // Enum variant: EnumName::VariantName or EnumName::VariantName(payload)
                if let Expression::Identifier(enum_name) = expr {
                    let variant_name = self.consume_identifier()?;
                    let payload = if self.match_token(TokenKind::LeftParen) {
                        let args = self.parse_argument_list()?;
                        self.expect_token(TokenKind::RightParen)?;
                        Some(args)
                    } else {
                        None
                    };
                    expr = Expression::EnumVariant {
                        enum_name,
                        variant_name,
                        payload,
                    };
                } else {
                    return Err(CompilerError::parser_error(
                        SourceLocation::new(self.file_path.clone(), self.peek().line, 0),
                        "Expected identifier before '::'".to_string(),
                    ));
                }
            } else if self.check_token(TokenKind::LeftBrace) {
                // Struct literal: Identifier { field: value, ... }
                if let Expression::Identifier(struct_name) = expr {
                    self.advance(); // consume {
                    let mut fields = Vec::new();

                    while !self.check_token(TokenKind::RightBrace) && !self.is_at_end() {
                        let field_name = self.consume_identifier()?;
                        self.expect_token(TokenKind::Colon)?;
                        let field_value = self.parse_expression()?;
                        fields.push((field_name, field_value));

                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }

                    self.expect_token(TokenKind::RightBrace)?;
                    expr = Expression::StructLiteral {
                        name: struct_name,
                        type_args: None,
                        fields,
                    };
                } else {
                    break;
                }
            } else if self.match_token(TokenKind::Question) {
                // Try operator: expr?
                expr = Expression::Try {
                    expression: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse primary expression (literals, identifiers, parenthesized expressions).
    fn parse_primary(&mut self) -> CompilerResult<Expression> {
        let token = self.peek();

        match &token.kind {
            TokenKind::IntLiteral => {
                let value = token
                    .lexeme
                    .parse::<i64>()
                    .map_err(|_| self.error("Invalid integer literal"))?;
                self.advance();
                Ok(Expression::IntLiteral(value))
            }
            TokenKind::FloatLiteral => {
                let value = token
                    .lexeme
                    .parse::<f64>()
                    .map_err(|_| self.error("Invalid float literal"))?;
                self.advance();
                Ok(Expression::FloatLiteral(value))
            }
            TokenKind::StringLiteral => {
                let value = token.lexeme.clone();
                self.advance();
                Ok(Expression::StringLiteral(value))
            }
            TokenKind::BoolLiteral => {
                let value = token.lexeme == "true";
                self.advance();
                Ok(Expression::BoolLiteral(value))
            }
            TokenKind::Keyword(Keyword::Null) => {
                self.advance();
                Ok(Expression::NullLiteral)
            }
            TokenKind::Identifier => {
                let name = token.lexeme.clone();
                self.advance();
                Ok(Expression::Identifier(name))
            }
            TokenKind::LeftParen => {
                self.advance();

                // Empty tuple ()
                if self.check_token(TokenKind::RightParen) {
                    self.advance();
                    return Ok(Expression::Tuple {
                        elements: Vec::new(),
                    });
                }

                // Parse first expression
                let first_expr = self.parse_expression()?;

                // Check if this is a tuple (has comma) or just a parenthesized expression
                if self.match_token(TokenKind::Comma) {
                    // This is a tuple
                    let mut elements = vec![first_expr];

                    // Allow trailing comma
                    if !self.check_token(TokenKind::RightParen) {
                        elements.push(self.parse_expression()?);
                        while self.match_token(TokenKind::Comma) {
                            if self.check_token(TokenKind::RightParen) {
                                break;
                            }
                            elements.push(self.parse_expression()?);
                        }
                    }

                    self.expect_token(TokenKind::RightParen)?;
                    Ok(Expression::Tuple { elements })
                } else {
                    // Just a parenthesized expression
                    self.expect_token(TokenKind::RightParen)?;
                    Ok(first_expr)
                }
            }
            TokenKind::LeftBracket => {
                self.advance();
                let elements = self.parse_argument_list()?;
                self.expect_token(TokenKind::RightBracket)?;
                Ok(Expression::Array { elements })
            }
            TokenKind::Pipe => {
                // Closure: |params| expr or |params| { block }
                self.parse_closure()
            }
            TokenKind::Keyword(Keyword::Move) => {
                // Move closure: move |params| expr
                self.parse_closure()
            }
            _ => Err(self.error("Expected expression")),
        }
    }

    /// Parse argument list for function calls.
    fn parse_argument_list(&mut self) -> CompilerResult<Vec<Expression>> {
        let mut arguments = Vec::new();

        if !self.check_token(TokenKind::RightParen) && !self.check_token(TokenKind::RightBracket) {
            loop {
                arguments.push(self.parse_expression()?);

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        Ok(arguments)
    }

    /// Parse closure expression: |params| expr or move |params| { block }
    fn parse_closure(&mut self) -> CompilerResult<Expression> {
        // Check for move keyword
        let is_move = self.match_keyword(Keyword::Move);

        // Expect opening pipe
        self.expect_token(TokenKind::Pipe)?;

        // Parse parameters
        let mut parameters = Vec::new();
        if !self.check_token(TokenKind::Pipe) {
            loop {
                // Parse parameter pattern (identifier for now, can be extended)
                let param_name = self.expect_identifier()?;
                let pattern = Pattern::Identifier(param_name.clone());

                // Optional type annotation
                let param_type = if self.match_token(TokenKind::Colon) {
                    self.parse_type()?
                } else {
                    // Type will be inferred
                    Type::Custom("_infer".to_string())
                };

                parameters.push(Parameter {
                    pattern,
                    param_type,
                    is_reference: false,
                });

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        // Expect closing pipe
        self.expect_token(TokenKind::Pipe)?;

        // Optional return type annotation
        let return_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse body - either expression or block
        let body = if self.check_token(TokenKind::LeftBrace) {
            // Block body
            let block = self.parse_block()?;
            ClosureBody::Block(block)
        } else {
            // Expression body
            let expr = self.parse_expression()?;
            ClosureBody::Expression(Box::new(expr))
        };

        Ok(Expression::Closure {
            parameters,
            return_type,
            body,
            is_move,
        })
    }

    // Helper methods for token manipulation

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn check_token(&self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn check_keyword(&self, keyword: Keyword) -> bool {
        matches!(self.peek().kind, TokenKind::Keyword(k) if k == keyword)
    }

    fn current_keyword(&self) -> Option<Keyword> {
        if let TokenKind::Keyword(k) = self.peek().kind {
            Some(k)
        } else {
            None
        }
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check_token(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> bool {
        if self.check_keyword(keyword) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_operator(&mut self, operator: Operator) -> bool {
        if matches!(self.peek().kind, TokenKind::Operator(op) if op == operator) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_operators(&mut self, operators: &[Operator]) -> Option<Operator> {
        operators
            .iter()
            .find(|&&op| self.match_operator(op))
            .copied()
    }

    fn expect_token(&mut self, kind: TokenKind) -> CompilerResult<()> {
        if self.check_token(kind.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(&format!("Expected {kind:?}")))
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> CompilerResult<()> {
        if self.check_keyword(keyword) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(&format!("Expected keyword '{keyword}'")))
        }
    }

    fn expect_operator(&mut self, operator: Operator) -> CompilerResult<()> {
        if self.match_operator(operator) {
            Ok(())
        } else {
            Err(self.error(&format!("Expected operator '{operator}'")))
        }
    }

    fn expect_identifier(&mut self) -> CompilerResult<String> {
        if matches!(self.peek().kind, TokenKind::Identifier) {
            let name = self.peek().lexeme.clone();
            self.advance();
            Ok(name)
        } else {
            Err(self.error("Expected identifier"))
        }
    }

    fn consume_identifier(&mut self) -> CompilerResult<String> {
        if matches!(self.peek().kind, TokenKind::Identifier) {
            let name = self.peek().lexeme.clone();
            self.advance();
            Ok(name)
        } else {
            Err(self.error("Expected identifier"))
        }
    }

    fn consume_semicolon(&mut self) -> CompilerResult<()> {
        self.expect_token(TokenKind::Semicolon)
    }

    fn error(&self, message: &str) -> CompilerError {
        let token = self.peek();
        CompilerError::parser_error(
            SourceLocation::new(self.file_path.clone(), token.line, token.column),
            message,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenizer::Tokenizer;

    fn parse_source(source: &str) -> CompilerResult<Program> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.kr"));
        parser.parse()
    }

    #[test]
    fn test_parse_import_statement() {
        let program = parse_source("import foo.bar;").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(program.statements[0], Statement::Import { .. }));
    }

    #[test]
    fn test_parse_module_statement() {
        let program = parse_source("module foo.bar;").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(program.statements[0], Statement::Module { .. }));
    }

    #[test]
    fn test_parse_variable_declaration() {
        let program = parse_source("let x = 42;").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0],
            Statement::VariableDeclaration { .. }
        ));
    }

    #[test]
    fn test_parse_function_declaration() {
        let program =
            parse_source("fn add(a: int, b: int) -> int { return a + b; }").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0],
            Statement::FunctionDeclaration { .. }
        ));
    }

    #[test]
    fn test_parse_if_statement() {
        let program = parse_source("if (x > 0) { return x; }").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(program.statements[0], Statement::If { .. }));
    }

    #[test]
    fn test_parse_generic_struct_declaration() {
        let program = parse_source("struct Box<T> { value: T; }").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0],
            Statement::StructDeclaration { .. }
        ));
    }

    #[test]
    fn test_parse_generic_call_site() {
        let program =
            parse_source("fn main() -> int { return id<int>(1); }").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_generic_struct_literal_site() {
        let program =
            parse_source("fn main() -> int { let _b = Box<int> { value: 1 }; return 0; }")
                .expect("parse failed");
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_expression() {
        let program = parse_source("x + y * z;").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
    }
}
