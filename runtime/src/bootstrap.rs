//! Bootstrap compiler infrastructure for Kraken.
//!
//! Provides foundational components for building a self-hosted compiler.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Token type for lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Keywords
    Fn,
    Let,
    If,
    Else,
    Return,
    Struct,
    Enum,
    Impl,
    Trait,
    Type,

    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),

    // Identifiers
    Identifier(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
    Colon,
    Arrow,

    // Special
    Eof,
}

/// Token with location information.
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

impl Token {
    /// Create a new token.
    pub fn new(token_type: TokenType, line: usize, column: usize) -> Self {
        Self {
            token_type,
            line,
            column,
        }
    }
}

/// Simple lexer for bootstrap compiler.
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    /// Create a new lexer.
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.is_at_end() {
            return Token::new(TokenType::Eof, self.line, self.column);
        }

        let ch = self.current_char();
        let line = self.line;
        let column = self.column;

        match ch {
            '+' => {
                self.advance();
                Token::new(TokenType::Plus, line, column)
            }
            '-' => {
                self.advance();
                if self.current_char() == '>' {
                    self.advance();
                    Token::new(TokenType::Arrow, line, column)
                } else {
                    Token::new(TokenType::Minus, line, column)
                }
            }
            '*' => {
                self.advance();
                Token::new(TokenType::Star, line, column)
            }
            '/' => {
                self.advance();
                Token::new(TokenType::Slash, line, column)
            }
            '%' => {
                self.advance();
                Token::new(TokenType::Percent, line, column)
            }
            '=' => {
                self.advance();
                if self.current_char() == '=' {
                    self.advance();
                    Token::new(TokenType::EqualEqual, line, column)
                } else {
                    Token::new(TokenType::Equal, line, column)
                }
            }
            '!' => {
                self.advance();
                if self.current_char() == '=' {
                    self.advance();
                    Token::new(TokenType::NotEqual, line, column)
                } else {
                    Token::new(TokenType::Eof, line, column) // Error case
                }
            }
            '<' => {
                self.advance();
                if self.current_char() == '=' {
                    self.advance();
                    Token::new(TokenType::LessEqual, line, column)
                } else {
                    Token::new(TokenType::Less, line, column)
                }
            }
            '>' => {
                self.advance();
                if self.current_char() == '=' {
                    self.advance();
                    Token::new(TokenType::GreaterEqual, line, column)
                } else {
                    Token::new(TokenType::Greater, line, column)
                }
            }
            '(' => {
                self.advance();
                Token::new(TokenType::LeftParen, line, column)
            }
            ')' => {
                self.advance();
                Token::new(TokenType::RightParen, line, column)
            }
            '{' => {
                self.advance();
                Token::new(TokenType::LeftBrace, line, column)
            }
            '}' => {
                self.advance();
                Token::new(TokenType::RightBrace, line, column)
            }
            '[' => {
                self.advance();
                Token::new(TokenType::LeftBracket, line, column)
            }
            ']' => {
                self.advance();
                Token::new(TokenType::RightBracket, line, column)
            }
            ',' => {
                self.advance();
                Token::new(TokenType::Comma, line, column)
            }
            ';' => {
                self.advance();
                Token::new(TokenType::Semicolon, line, column)
            }
            ':' => {
                self.advance();
                Token::new(TokenType::Colon, line, column)
            }
            '"' => self.read_string(),
            _ if ch.is_ascii_digit() => self.read_number(),
            _ if ch.is_alphabetic() || ch == '_' => self.read_identifier(),
            _ => {
                self.advance();
                Token::new(TokenType::Eof, line, column) // Error case
            }
        }
    }

    fn current_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            if self.input[self.position] == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.position += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() && self.current_char().is_whitespace() {
            self.advance();
        }
    }

    fn read_string(&mut self) -> Token {
        let line = self.line;
        let column = self.column;
        self.advance(); // Skip opening quote

        let mut value = String::new();
        while !self.is_at_end() && self.current_char() != '"' {
            value.push(self.current_char());
            self.advance();
        }

        if !self.is_at_end() {
            self.advance(); // Skip closing quote
        }

        Token::new(TokenType::String(value), line, column)
    }

    fn read_number(&mut self) -> Token {
        let line = self.line;
        let column = self.column;
        let mut value = String::new();

        while !self.is_at_end() && self.current_char().is_ascii_digit() {
            value.push(self.current_char());
            self.advance();
        }

        if !self.is_at_end() && self.current_char() == '.' {
            value.push(self.current_char());
            self.advance();

            while !self.is_at_end() && self.current_char().is_ascii_digit() {
                value.push(self.current_char());
                self.advance();
            }

            let float_val = value.parse::<f64>().unwrap_or(0.0);
            Token::new(TokenType::Float(float_val), line, column)
        } else {
            let int_val = value.parse::<i64>().unwrap_or(0);
            Token::new(TokenType::Integer(int_val), line, column)
        }
    }

    fn read_identifier(&mut self) -> Token {
        let line = self.line;
        let column = self.column;
        let mut value = String::new();

        while !self.is_at_end()
            && (self.current_char().is_alphanumeric() || self.current_char() == '_')
        {
            value.push(self.current_char());
            self.advance();
        }

        let token_type = match value.as_str() {
            "fn" => TokenType::Fn,
            "let" => TokenType::Let,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "struct" => TokenType::Struct,
            "enum" => TokenType::Enum,
            "impl" => TokenType::Impl,
            "trait" => TokenType::Trait,
            "type" => TokenType::Type,
            "true" => TokenType::Bool(true),
            "false" => TokenType::Bool(false),
            _ => TokenType::Identifier(value),
        };

        Token::new(token_type, line, column)
    }
}

/// AST node types for bootstrap compiler.
#[derive(Debug, Clone)]
pub enum AstNode {
    Program(Vec<AstNode>),
    Function {
        name: String,
        params: Vec<String>,
        body: Box<AstNode>,
    },
    Block(Vec<AstNode>),
    Return(Option<Box<AstNode>>),
    BinaryOp {
        op: String,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    Integer(i64),
    Identifier(String),
}

/// Simple parser for bootstrap compiler.
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    /// Create a new parser.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    /// Parse the token stream into an AST.
    pub fn parse(&mut self) -> Result<AstNode, String> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                break;
            }
        }

        Ok(AstNode::Program(statements))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        if self.match_token(&TokenType::Return) {
            self.parse_return()
        } else {
            self.parse_expression()
        }
    }

    fn parse_return(&mut self) -> Result<AstNode, String> {
        if self.match_token(&TokenType::Semicolon) {
            Ok(AstNode::Return(None))
        } else {
            let expr = self.parse_expression()?;
            self.expect_token(&TokenType::Semicolon)?;
            Ok(AstNode::Return(Some(Box::new(expr))))
        }
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_primary()?;

        while self.match_token(&TokenType::Plus) || self.match_token(&TokenType::Minus) {
            let op = if self.previous_token_is(&TokenType::Plus) {
                "+"
            } else {
                "-"
            };
            let right = self.parse_primary()?;
            left = AstNode::BinaryOp {
                op: op.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<AstNode, String> {
        if let Some(token) = self.current_token() {
            match &token.token_type {
                TokenType::Integer(val) => {
                    let val = *val;
                    self.advance();
                    Ok(AstNode::Integer(val))
                }
                TokenType::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    Ok(AstNode::Identifier(name))
                }
                _ => Err("Unexpected token".to_string()),
            }
        } else {
            Err("Unexpected end of input".to_string())
        }
    }

    fn current_token(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
            || matches!(
                self.current_token().map(|t| &t.token_type),
                Some(TokenType::Eof)
            )
    }

    fn match_token(&mut self, token_type: &TokenType) -> bool {
        if let Some(token) = self.current_token() {
            if std::mem::discriminant(&token.token_type) == std::mem::discriminant(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn previous_token_is(&self, token_type: &TokenType) -> bool {
        if self.position > 0 {
            let prev = &self.tokens[self.position - 1];
            std::mem::discriminant(&prev.token_type) == std::mem::discriminant(token_type)
        } else {
            false
        }
    }

    fn expect_token(&mut self, token_type: &TokenType) -> Result<(), String> {
        if self.match_token(token_type) {
            Ok(())
        } else {
            Err(format!("Expected token: {token_type:?}"))
        }
    }
}

/// Bootstrap compiler context.
pub struct BootstrapCompiler {
    modules: HashMap<PathBuf, AstNode>,
}

impl BootstrapCompiler {
    /// Create a new bootstrap compiler.
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Compile a source file.
    pub fn compile_file(&mut self, path: &Path, source: &str) -> Result<(), String> {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        let mut parser = Parser::new(tokens);
        let ast = parser.parse()?;

        self.modules.insert(path.to_path_buf(), ast);
        Ok(())
    }

    /// Get compiled modules.
    pub fn modules(&self) -> &HashMap<PathBuf, AstNode> {
        &self.modules
    }
}

impl Default for BootstrapCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_keywords() {
        let mut lexer = Lexer::new("fn let if else return");
        assert!(matches!(lexer.next_token().token_type, TokenType::Fn));
        assert!(matches!(lexer.next_token().token_type, TokenType::Let));
        assert!(matches!(lexer.next_token().token_type, TokenType::If));
        assert!(matches!(lexer.next_token().token_type, TokenType::Else));
        assert!(matches!(lexer.next_token().token_type, TokenType::Return));
    }

    #[test]
    fn test_lexer_integers() {
        let mut lexer = Lexer::new("42 123 0");
        assert!(matches!(
            lexer.next_token().token_type,
            TokenType::Integer(42)
        ));
        assert!(matches!(
            lexer.next_token().token_type,
            TokenType::Integer(123)
        ));
        assert!(matches!(
            lexer.next_token().token_type,
            TokenType::Integer(0)
        ));
    }

    #[test]
    fn test_lexer_operators() {
        let mut lexer = Lexer::new("+ - * / == != < >");
        assert!(matches!(lexer.next_token().token_type, TokenType::Plus));
        assert!(matches!(lexer.next_token().token_type, TokenType::Minus));
        assert!(matches!(lexer.next_token().token_type, TokenType::Star));
        assert!(matches!(lexer.next_token().token_type, TokenType::Slash));
        assert!(matches!(
            lexer.next_token().token_type,
            TokenType::EqualEqual
        ));
        assert!(matches!(lexer.next_token().token_type, TokenType::NotEqual));
        assert!(matches!(lexer.next_token().token_type, TokenType::Less));
        assert!(matches!(lexer.next_token().token_type, TokenType::Greater));
    }

    #[test]
    fn test_lexer_identifiers() {
        let mut lexer = Lexer::new("foo bar_baz test123");
        match lexer.next_token().token_type {
            TokenType::Identifier(s) => assert_eq!(s, "foo"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().token_type {
            TokenType::Identifier(s) => assert_eq!(s, "bar_baz"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().token_type {
            TokenType::Identifier(s) => assert_eq!(s, "test123"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_lexer_strings() {
        let mut lexer = Lexer::new(r#""hello" "world""#);
        match lexer.next_token().token_type {
            TokenType::String(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string"),
        }
        match lexer.next_token().token_type {
            TokenType::String(s) => assert_eq!(s, "world"),
            _ => panic!("Expected string"),
        }
    }

    #[test]
    fn test_parser_simple_expression() {
        let mut lexer = Lexer::new("42");
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        assert!(ast.is_ok());
    }

    #[test]
    fn test_parser_binary_expression() {
        let mut lexer = Lexer::new("1 + 2");
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        assert!(ast.is_ok());
    }

    #[test]
    fn test_bootstrap_compiler() {
        let mut compiler = BootstrapCompiler::new();
        let result = compiler.compile_file(Path::new("test.kr"), "42");
        assert!(result.is_ok());
        assert_eq!(compiler.modules().len(), 1);
    }
}
