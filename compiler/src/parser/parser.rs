use crate::error::{CompilerError, CompilerResult, SourceLocation};
use crate::lexer::token::{Token, TokenKind, Keyword, Operator};
use super::ast::*;
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
        } else if self.match_keyword(Keyword::Fn) {
            self.parse_function_declaration(false, false)
        } else if self.match_keyword(Keyword::Pub) {
            self.parse_public_declaration()
        } else if self.match_keyword(Keyword::Struct) {
            self.parse_struct_declaration(false)
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
        let name = self.consume_identifier()?;
        
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
            name,
            type_annotation,
            initializer,
            is_mutable,
        })
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
    fn parse_function_declaration(&mut self, is_async: bool, is_public: bool) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;
        
        self.expect_token(TokenKind::LeftParen)?;
        let parameters = self.parse_parameter_list()?;
        self.expect_token(TokenKind::RightParen)?;

        let return_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Statement::FunctionDeclaration {
            name,
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
        } else if self.match_keyword(Keyword::Class) {
            self.parse_class_declaration(true)
        } else {
            Err(self.error("Expected fn, struct, or class after pub"))
        }
    }

    /// Parse a struct declaration.
    fn parse_struct_declaration(&mut self, is_public: bool) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;
        
        self.expect_token(TokenKind::LeftBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(TokenKind::RightBrace)?;

        Ok(Statement::StructDeclaration {
            name,
            fields,
            is_public,
        })
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

    /// Parse a for statement.
    fn parse_for_statement(&mut self) -> CompilerResult<Statement> {
        self.expect_token(TokenKind::LeftParen)?;

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
            self.expect_token(TokenKind::Arrow)?;
            let body = self.parse_block()?;

            arms.push(MatchArm { pattern, body });
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
                let name = self.consume_identifier()?;
                self.expect_token(TokenKind::Colon)?;
                let param_type = self.parse_type()?;

                parameters.push(Parameter {
                    name,
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
        if let Some(keyword) = self.current_keyword() {
            if let Some(base_type) = Type::from_keyword(keyword) {
                self.advance();
                return Ok(base_type);
            }
        }

        if self.match_token(TokenKind::LeftBracket) {
            let element_type = Box::new(self.parse_type()?);
            
            let size = if self.match_token(TokenKind::Semicolon) {
                if let TokenKind::IntLiteral = self.peek().kind {
                    let size_str = self.peek().lexeme.clone();
                    self.advance();
                    Some(size_str.parse::<usize>().map_err(|_| {
                        self.error("Invalid array size")
                    })?)
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
            return Ok(Type::Reference { inner_type, is_mutable });
        }

        let name = self.consume_identifier()?;
        Ok(Type::Custom(name))
    }

    /// Parse a pattern for match expressions.
    fn parse_pattern(&mut self) -> CompilerResult<Pattern> {
        if self.check_token(TokenKind::Identifier) {
            let name = self.consume_identifier()?;
            if name == "_" {
                Ok(Pattern::Wildcard)
            } else {
                Ok(Pattern::Identifier(name))
            }
        } else {
            let expr = self.parse_primary()?;
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
        let mut left = self.parse_equality()?;

        while self.match_operator(Operator::And) {
            let operator = Operator::And;
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
        let mut left = self.parse_term()?;

        while let Some(op) = self.match_operators(&[
            Operator::Less,
            Operator::LessEqual,
            Operator::Greater,
            Operator::GreaterEqual,
        ]) {
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

        while let Some(op) = self.match_operators(&[Operator::Star, Operator::Slash, Operator::Percent]) {
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
        if let Some(op) = self.match_operators(&[Operator::Not, Operator::Minus, Operator::Ampersand, Operator::Star]) {
            let operand = Box::new(self.parse_unary()?);
            
            return Ok(match op {
                Operator::Ampersand => Expression::Reference { expression: operand },
                Operator::Star => Expression::Dereference { expression: operand },
                _ => Expression::Unary { operator: op, operand },
            });
        }

        self.parse_postfix()
    }

    /// Parse postfix expression (calls, indexing, member access).
    fn parse_postfix(&mut self) -> CompilerResult<Expression> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(TokenKind::LeftParen) {
                let arguments = self.parse_argument_list()?;
                self.expect_token(TokenKind::RightParen)?;
                expr = Expression::Call {
                    callee: Box::new(expr),
                    arguments,
                };
            } else if self.match_token(TokenKind::LeftBracket) {
                let index = self.parse_expression()?;
                self.expect_token(TokenKind::RightBracket)?;
                expr = Expression::Index {
                    array: Box::new(expr),
                    index: Box::new(index),
                };
            } else if self.match_token(TokenKind::Dot) {
                let member = self.consume_identifier()?;
                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    member,
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
                let value = token.lexeme.parse::<i64>().map_err(|_| {
                    self.error("Invalid integer literal")
                })?;
                self.advance();
                Ok(Expression::IntLiteral(value))
            }
            TokenKind::FloatLiteral => {
                let value = token.lexeme.parse::<f64>().map_err(|_| {
                    self.error("Invalid float literal")
                })?;
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
                let expr = self.parse_expression()?;
                self.expect_token(TokenKind::RightParen)?;
                Ok(expr)
            }
            TokenKind::LeftBracket => {
                self.advance();
                let elements = self.parse_argument_list()?;
                self.expect_token(TokenKind::RightBracket)?;
                Ok(Expression::Array { elements })
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
        operators.iter().find(|&&op| self.match_operator(op)).copied()
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
        let program = parse_source("fn add(a: int, b: int) -> int { return a + b; }").expect("parse failed");
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
    fn test_parse_expression() {
        let program = parse_source("x + y * z;").expect("parse failed");
        assert_eq!(program.statements.len(), 1);
    }
}
