use std::fmt;

/// Token representation with location information.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    /// Create a new token.
    pub fn new(kind: TokenKind, lexeme: String, line: usize, column: usize) -> Self {
        Self {
            kind,
            lexeme,
            line,
            column,
        }
    }
}

/// Token types in Kraken language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    BoolLiteral,
    
    // Identifiers and Keywords
    Identifier,
    Keyword(Keyword),
    
    // Operators
    Operator(Operator),
    
    // Delimiters
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]
    Semicolon,      // ;
    Comma,          // ,
    Dot,            // .
    Colon,          // :
    Arrow,          // ->
    
    // Special
    Eof,
    Newline,
    Comment,
}

/// Kraken language keywords.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keyword {
    // Control flow
    If,
    Else,
    Match,
    For,
    While,
    Do,
    Break,
    Continue,
    Return,
    
    // Types
    Int,
    Float,
    Bool,
    String,
    Void,
    
    // Declarations
    Fn,
    Let,
    Const,
    Struct,
    Class,
    Interface,
    Trait,
    Impl,
    
    // Modifiers
    Pub,
    Priv,
    Static,
    Async,
    Ref,
    Mut,
    
    // Special
    True,
    False,
    Null,
    Self_,
    Super,
    Defer,
    
    // Advanced
    Generic,
    Where,
    As,
    In,
}

impl Keyword {
    /// Parse a keyword from a string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "match" => Some(Self::Match),
            "for" => Some(Self::For),
            "while" => Some(Self::While),
            "do" => Some(Self::Do),
            "break" => Some(Self::Break),
            "continue" => Some(Self::Continue),
            "return" => Some(Self::Return),
            "int" => Some(Self::Int),
            "float" => Some(Self::Float),
            "bool" => Some(Self::Bool),
            "string" => Some(Self::String),
            "void" => Some(Self::Void),
            "fn" => Some(Self::Fn),
            "let" => Some(Self::Let),
            "const" => Some(Self::Const),
            "struct" => Some(Self::Struct),
            "class" => Some(Self::Class),
            "interface" => Some(Self::Interface),
            "trait" => Some(Self::Trait),
            "impl" => Some(Self::Impl),
            "pub" => Some(Self::Pub),
            "priv" => Some(Self::Priv),
            "static" => Some(Self::Static),
            "async" => Some(Self::Async),
            "ref" => Some(Self::Ref),
            "mut" => Some(Self::Mut),
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "null" => Some(Self::Null),
            "self" => Some(Self::Self_),
            "super" => Some(Self::Super),
            "defer" => Some(Self::Defer),
            "generic" => Some(Self::Generic),
            "where" => Some(Self::Where),
            "as" => Some(Self::As),
            "in" => Some(Self::In),
            _ => None,
        }
    }

    /// Get the string representation of a keyword.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::If => "if",
            Self::Else => "else",
            Self::Match => "match",
            Self::For => "for",
            Self::While => "while",
            Self::Do => "do",
            Self::Break => "break",
            Self::Continue => "continue",
            Self::Return => "return",
            Self::Int => "int",
            Self::Float => "float",
            Self::Bool => "bool",
            Self::String => "string",
            Self::Void => "void",
            Self::Fn => "fn",
            Self::Let => "let",
            Self::Const => "const",
            Self::Struct => "struct",
            Self::Class => "class",
            Self::Interface => "interface",
            Self::Trait => "trait",
            Self::Impl => "impl",
            Self::Pub => "pub",
            Self::Priv => "priv",
            Self::Static => "static",
            Self::Async => "async",
            Self::Ref => "ref",
            Self::Mut => "mut",
            Self::True => "true",
            Self::False => "false",
            Self::Null => "null",
            Self::Self_ => "self",
            Self::Super => "super",
            Self::Defer => "defer",
            Self::Generic => "generic",
            Self::Where => "where",
            Self::As => "as",
            Self::In => "in",
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Operators in Kraken language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operator {
    // Arithmetic
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    
    // Comparison
    Equal,          // ==
    NotEqual,       // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    
    // Logical
    And,            // &&
    Or,             // ||
    Not,            // !
    
    // Bitwise
    BitAnd,         // &
    BitOr,          // |
    BitXor,         // ^
    BitNot,         // ~
    LeftShift,      // <<
    RightShift,     // >>
    
    // Assignment
    Assign,         // =
    PlusAssign,     // +=
    MinusAssign,    // -=
    StarAssign,     // *=
    SlashAssign,    // /=
    PercentAssign,  // %=
    
    // Other
    Question,       // ?
    Ampersand,      // & (also used for references)
}

impl Operator {
    /// Get the string representation of an operator.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Percent => "%",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::And => "&&",
            Self::Or => "||",
            Self::Not => "!",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::BitNot => "~",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
            Self::Assign => "=",
            Self::PlusAssign => "+=",
            Self::MinusAssign => "-=",
            Self::StarAssign => "*=",
            Self::SlashAssign => "/=",
            Self::PercentAssign => "%=",
            Self::Question => "?",
            Self::Ampersand => "&",
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_from_str() {
        assert_eq!(Keyword::from_str("if"), Some(Keyword::If));
        assert_eq!(Keyword::from_str("fn"), Some(Keyword::Fn));
        assert_eq!(Keyword::from_str("true"), Some(Keyword::True));
        assert_eq!(Keyword::from_str("invalid"), None);
    }

    #[test]
    fn test_keyword_as_str() {
        assert_eq!(Keyword::If.as_str(), "if");
        assert_eq!(Keyword::Fn.as_str(), "fn");
        assert_eq!(Keyword::True.as_str(), "true");
    }

    #[test]
    fn test_operator_as_str() {
        assert_eq!(Operator::Plus.as_str(), "+");
        assert_eq!(Operator::Equal.as_str(), "==");
        assert_eq!(Operator::And.as_str(), "&&");
    }

    #[test]
    fn test_token_creation() {
        let token = Token::new(TokenKind::Identifier, "test".to_string(), 1, 5);
        assert_eq!(token.kind, TokenKind::Identifier);
        assert_eq!(token.lexeme, "test");
        assert_eq!(token.line, 1);
        assert_eq!(token.column, 5);
    }
}
