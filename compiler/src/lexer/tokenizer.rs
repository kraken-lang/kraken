use std::path::Path;
use crate::error::{CompilerError, CompilerResult, SourceLocation};
use super::token::{Token, TokenKind, Keyword, Operator};

/// Tokenizer for Kraken source code.
/// 
/// Converts source code into a stream of tokens for parsing.
/// Supports both .kr and .krak file extensions.
pub struct Tokenizer {
    source: Vec<char>,
    current: usize,
    line: usize,
    column: usize,
    file_path: std::path::PathBuf,
}

impl Tokenizer {
    /// Create a new tokenizer for the given source code.
    /// 
    /// # Arguments
    /// * `source` - The source code to tokenize
    /// * `file_path` - Path to the source file
    pub fn new(source: String, file_path: std::path::PathBuf) -> Self {
        Self {
            source: source.chars().collect(),
            current: 0,
            line: 1,
            column: 1,
            file_path,
        }
    }

    /// Tokenize the entire source code.
    /// 
    /// # Returns
    /// A vector of tokens
    /// 
    /// # Errors
    /// Returns `CompilerError::LexerError` if tokenization fails
    pub fn tokenize(&mut self) -> CompilerResult<Vec<Token>> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();
            
            if self.is_at_end() {
                break;
            }

            let token = self.next_token()?;
            
            // Skip comments and newlines for now
            if !matches!(token.kind, TokenKind::Comment | TokenKind::Newline) {
                tokens.push(token);
            }
        }

        tokens.push(Token::new(
            TokenKind::Eof,
            String::new(),
            self.line,
            self.column,
        ));

        Ok(tokens)
    }

    /// Get the next token from the source.
    fn next_token(&mut self) -> CompilerResult<Token> {
        let start_line = self.line;
        let start_column = self.column;
        let ch = self.advance();

        let kind = match ch {
            // Single character tokens
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            ':' => TokenKind::Colon,
            '~' => TokenKind::Operator(Operator::BitNot),
            '?' => TokenKind::Operator(Operator::Question),

            // Operators that may be multi-character
            '+' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::PlusAssign)
                } else {
                    TokenKind::Operator(Operator::Plus)
                }
            }
            '-' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::MinusAssign)
                } else if self.match_char('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Operator(Operator::Minus)
                }
            }
            '*' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::StarAssign)
                } else {
                    TokenKind::Operator(Operator::Star)
                }
            }
            '/' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::SlashAssign)
                } else if self.match_char('/') {
                    return self.line_comment(start_line, start_column);
                } else if self.match_char('*') {
                    return self.block_comment(start_line, start_column);
                } else {
                    TokenKind::Operator(Operator::Slash)
                }
            }
            '%' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::PercentAssign)
                } else {
                    TokenKind::Operator(Operator::Percent)
                }
            }
            '=' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::Equal)
                } else {
                    TokenKind::Operator(Operator::Assign)
                }
            }
            '!' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::NotEqual)
                } else {
                    TokenKind::Operator(Operator::Not)
                }
            }
            '<' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::LessEqual)
                } else if self.match_char('<') {
                    TokenKind::Operator(Operator::LeftShift)
                } else {
                    TokenKind::Operator(Operator::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    TokenKind::Operator(Operator::GreaterEqual)
                } else if self.match_char('>') {
                    TokenKind::Operator(Operator::RightShift)
                } else {
                    TokenKind::Operator(Operator::Greater)
                }
            }
            '&' => {
                if self.match_char('&') {
                    TokenKind::Operator(Operator::And)
                } else {
                    TokenKind::Operator(Operator::Ampersand)
                }
            }
            '|' => {
                if self.match_char('|') {
                    TokenKind::Operator(Operator::Or)
                } else {
                    TokenKind::Operator(Operator::BitOr)
                }
            }
            '^' => TokenKind::Operator(Operator::BitXor),

            // String literals
            '"' => return self.string_literal(start_line, start_column),

            // Numbers
            '0'..='9' => return self.number_literal(ch, start_line, start_column),

            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => {
                return self.identifier_or_keyword(ch, start_line, start_column)
            }

            '\n' => {
                self.line += 1;
                self.column = 1;
                TokenKind::Newline
            }

            _ => {
                return Err(CompilerError::lexer_error(
                    SourceLocation::new(self.file_path.clone(), start_line, start_column),
                    format!("Unexpected character: '{ch}'"),
                ));
            }
        };

        let lexeme = self.source[self.current - 1..self.current]
            .iter()
            .collect();

        Ok(Token::new(kind, lexeme, start_line, start_column))
    }

    /// Parse a string literal.
    fn string_literal(&mut self, start_line: usize, start_column: usize) -> CompilerResult<Token> {
        let mut value = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            
            if self.peek() == '\\' {
                self.advance();
                if !self.is_at_end() {
                    let escaped = match self.advance() {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        ch => ch,
                    };
                    value.push(escaped);
                }
            } else {
                value.push(self.advance());
            }
        }

        if self.is_at_end() {
            return Err(CompilerError::lexer_error(
                SourceLocation::new(self.file_path.clone(), start_line, start_column),
                "Unterminated string literal",
            ));
        }

        self.advance(); // Closing "

        Ok(Token::new(
            TokenKind::StringLiteral,
            value,
            start_line,
            start_column,
        ))
    }

    /// Parse a number literal (integer or float).
    fn number_literal(
        &mut self,
        first: char,
        start_line: usize,
        start_column: usize,
    ) -> CompilerResult<Token> {
        let mut value = String::from(first);
        let mut is_float = false;

        while !self.is_at_end() && self.peek().is_ascii_digit() {
            value.push(self.advance());
        }

        if !self.is_at_end() && self.peek() == '.' && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            is_float = true;
            value.push(self.advance()); // .
            
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                value.push(self.advance());
            }
        }

        let kind = if is_float {
            TokenKind::FloatLiteral
        } else {
            TokenKind::IntLiteral
        };

        Ok(Token::new(kind, value, start_line, start_column))
    }

    /// Parse an identifier or keyword.
    fn identifier_or_keyword(
        &mut self,
        first: char,
        start_line: usize,
        start_column: usize,
    ) -> CompilerResult<Token> {
        let mut value = String::from(first);

        while !self.is_at_end() {
            let ch = self.peek();
            if ch.is_alphanumeric() || ch == '_' {
                value.push(self.advance());
            } else {
                break;
            }
        }

        let kind = if let Some(keyword) = Keyword::from_str(&value) {
            if keyword == Keyword::True || keyword == Keyword::False {
                TokenKind::BoolLiteral
            } else {
                TokenKind::Keyword(keyword)
            }
        } else {
            TokenKind::Identifier
        };

        Ok(Token::new(kind, value, start_line, start_column))
    }

    /// Parse a line comment.
    fn line_comment(&mut self, start_line: usize, start_column: usize) -> CompilerResult<Token> {
        let mut comment = String::from("//");

        while !self.is_at_end() && self.peek() != '\n' {
            comment.push(self.advance());
        }

        Ok(Token::new(
            TokenKind::Comment,
            comment,
            start_line,
            start_column,
        ))
    }

    /// Parse a block comment.
    fn block_comment(&mut self, start_line: usize, start_column: usize) -> CompilerResult<Token> {
        let mut comment = String::from("/*");

        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == Some('/') {
                comment.push(self.advance()); // *
                comment.push(self.advance()); // /
                break;
            }
            
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            
            comment.push(self.advance());
        }

        Ok(Token::new(
            TokenKind::Comment,
            comment,
            start_line,
            start_column,
        ))
    }

    /// Skip whitespace characters.
    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Check if we're at the end of the source.
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Peek at the current character without consuming it.
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    /// Peek at the next character without consuming it.
    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() {
            None
        } else {
            Some(self.source[self.current + 1])
        }
    }

    /// Advance to the next character.
    fn advance(&mut self) -> char {
        let ch = self.source[self.current];
        self.current += 1;
        self.column += 1;
        ch
    }

    /// Match and consume a character if it matches.
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }
}

/// Check if a file has a valid Kraken source extension (.kr or .krak).
pub fn is_kraken_source_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext == "kr" || ext == "krak")
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_is_kraken_source_file() {
        assert!(is_kraken_source_file(Path::new("test.kr")));
        assert!(is_kraken_source_file(Path::new("test.krak")));
        assert!(!is_kraken_source_file(Path::new("test.rs")));
        assert!(!is_kraken_source_file(Path::new("test")));
    }

    #[test]
    fn test_tokenize_simple() {
        let source = "let x = 42;".to_string();
        let mut tokenizer = Tokenizer::new(source, PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize().expect("tokenization failed");

        assert_eq!(tokens.len(), 6); // let, x, =, 42, ;, EOF
        assert!(matches!(tokens[0].kind, TokenKind::Keyword(Keyword::Let)));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier));
        assert!(matches!(tokens[2].kind, TokenKind::Operator(Operator::Assign)));
        assert!(matches!(tokens[3].kind, TokenKind::IntLiteral));
        assert!(matches!(tokens[4].kind, TokenKind::Semicolon));
        assert!(matches!(tokens[5].kind, TokenKind::Eof));
    }

    #[test]
    fn test_tokenize_operators() {
        let source = "+ - * / == != < > <= >=".to_string();
        let mut tokenizer = Tokenizer::new(source, PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize().expect("tokenization failed");

        assert!(matches!(tokens[0].kind, TokenKind::Operator(Operator::Plus)));
        assert!(matches!(tokens[1].kind, TokenKind::Operator(Operator::Minus)));
        assert!(matches!(tokens[2].kind, TokenKind::Operator(Operator::Star)));
        assert!(matches!(tokens[3].kind, TokenKind::Operator(Operator::Slash)));
        assert!(matches!(tokens[4].kind, TokenKind::Operator(Operator::Equal)));
        assert!(matches!(tokens[5].kind, TokenKind::Operator(Operator::NotEqual)));
    }

    #[test]
    fn test_tokenize_string_literal() {
        let source = r#""hello world""#.to_string();
        let mut tokenizer = Tokenizer::new(source, PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize().expect("tokenization failed");

        assert!(matches!(tokens[0].kind, TokenKind::StringLiteral));
        assert_eq!(tokens[0].lexeme, "hello world");
    }

    #[test]
    fn test_tokenize_float() {
        let source = "3.14".to_string();
        let mut tokenizer = Tokenizer::new(source, PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize().expect("tokenization failed");

        assert!(matches!(tokens[0].kind, TokenKind::FloatLiteral));
        assert_eq!(tokens[0].lexeme, "3.14");
    }

    #[test]
    fn test_tokenize_keywords() {
        let source = "fn if else while for".to_string();
        let mut tokenizer = Tokenizer::new(source, PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize().expect("tokenization failed");

        assert!(matches!(tokens[0].kind, TokenKind::Keyword(Keyword::Fn)));
        assert!(matches!(tokens[1].kind, TokenKind::Keyword(Keyword::If)));
        assert!(matches!(tokens[2].kind, TokenKind::Keyword(Keyword::Else)));
        assert!(matches!(tokens[3].kind, TokenKind::Keyword(Keyword::While)));
        assert!(matches!(tokens[4].kind, TokenKind::Keyword(Keyword::For)));
    }

    #[test]
    fn test_tokenize_comments() {
        let source = "// line comment\n/* block comment */".to_string();
        let mut tokenizer = Tokenizer::new(source, PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize().expect("tokenization failed");

        // Comments are filtered out
        assert_eq!(tokens.len(), 1); // Just EOF
    }
}
