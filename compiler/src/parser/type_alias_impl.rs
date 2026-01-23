// Type alias and impl block parsing functions
// This is a temporary file to add the parsing functions, will be integrated into parser.rs

use crate::error::{CompilerError, CompilerResult};
use crate::lexer::token::{Keyword, TokenKind};
use crate::parser::ast::{Statement, Type};
use crate::parser::parser::Parser;

impl Parser {
    /// Parse a type alias: type MyInt = int;
    pub fn parse_type_alias(&mut self, is_public: bool) -> CompilerResult<Statement> {
        let name = self.consume_identifier()?;
        
        // Parse optional generic parameters: type Result<T> = ...
        let generic_params = if self.match_token(TokenKind::Operator(crate::lexer::token::Operator::Lt)) {
            let mut params = Vec::new();
            params.push(self.consume_identifier()?);
            while self.match_token(TokenKind::Comma) {
                params.push(self.consume_identifier()?);
            }
            self.expect_token(TokenKind::Operator(crate::lexer::token::Operator::Gt))?;
            params
        } else {
            Vec::new()
        };
        
        self.expect_token(TokenKind::Operator(crate::lexer::token::Operator::Assign))?;
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
    pub fn parse_impl_block(&mut self) -> CompilerResult<Statement> {
        // Parse optional generic parameters: impl<T> Vec<T> { ... }
        let generic_params = if self.match_token(TokenKind::Operator(crate::lexer::token::Operator::Lt)) {
            let mut params = Vec::new();
            params.push(self.consume_identifier()?);
            while self.match_token(TokenKind::Comma) {
                params.push(self.consume_identifier()?);
            }
            self.expect_token(TokenKind::Operator(crate::lexer::token::Operator::Gt))?;
            params
        } else {
            Vec::new()
        };
        
        let type_name = self.consume_identifier()?;
        
        // Skip generic type arguments if present: impl Vec<T> { ... }
        if self.match_token(TokenKind::Operator(crate::lexer::token::Operator::Lt)) {
            // Skip until we find the matching >
            let mut depth = 1;
            while depth > 0 && !self.is_at_end() {
                if self.check_token(TokenKind::Operator(crate::lexer::token::Operator::Lt)) {
                    depth += 1;
                } else if self.check_token(TokenKind::Operator(crate::lexer::token::Operator::Gt)) {
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
}
