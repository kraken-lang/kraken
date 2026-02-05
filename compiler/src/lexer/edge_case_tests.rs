//! Comprehensive edge case tests for the lexer
//!
//! This module contains extensive edge case testing to ensure the lexer
//! handles all corner cases correctly.

#[cfg(test)]
mod tests {
    use crate::lexer::token::{Token, TokenKind};
    use crate::lexer::tokenizer::Tokenizer;
    use std::path::PathBuf;

    fn tokenize(source: &str) -> Vec<Token> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        tokenizer.tokenize().unwrap()
    }

    // Edge cases for numbers
    #[test]
    fn test_zero_integer() {
        let tokens = tokenize("0");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
        assert_eq!(tokens[0].lexeme, "0");
    }

    #[test]
    fn test_max_i64() {
        let tokens = tokenize("9223372036854775807");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
    }

    #[test]
    fn test_negative_integer() {
        let tokens = tokenize("-42");
        assert!(matches!(tokens[0].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[1].kind, TokenKind::IntLiteral));
    }

    #[test]
    fn test_float_with_exponent() {
        let tokens = tokenize("1.5e10");
        assert!(matches!(tokens[0].kind, TokenKind::FloatLiteral));
    }

    #[test]
    fn test_float_negative_exponent() {
        let tokens = tokenize("2.5e-3");
        assert!(matches!(tokens[0].kind, TokenKind::FloatLiteral));
    }

    #[test]
    fn test_float_zero() {
        let tokens = tokenize("0.0");
        assert!(matches!(tokens[0].kind, TokenKind::FloatLiteral));
    }

    // Edge cases for strings
    #[test]
    fn test_empty_string() {
        let tokens = tokenize(r#""""#);
        assert!(matches!(tokens[0].kind, TokenKind::StringLiteral));
        assert_eq!(tokens[0].lexeme, "");
    }

    #[test]
    fn test_string_with_newline_escape() {
        let tokens = tokenize(r#""hello\nworld""#);
        assert!(matches!(tokens[0].kind, TokenKind::StringLiteral));
    }

    #[test]
    fn test_string_with_tab_escape() {
        let tokens = tokenize(r#""hello\tworld""#);
        assert!(matches!(tokens[0].kind, TokenKind::StringLiteral));
    }

    #[test]
    fn test_string_with_quote_escape() {
        let tokens = tokenize(r#""say \"hello\"""#);
        assert!(matches!(tokens[0].kind, TokenKind::StringLiteral));
    }

    #[test]
    fn test_string_with_backslash() {
        let tokens = tokenize(r#""path\\to\\file""#);
        assert!(matches!(tokens[0].kind, TokenKind::StringLiteral));
    }

    #[test]
    fn test_multiline_string() {
        let tokens = tokenize("\"line1\nline2\nline3\"");
        assert!(matches!(tokens[0].kind, TokenKind::StringLiteral));
    }

    // Edge cases for identifiers
    #[test]
    fn test_single_char_identifier() {
        let tokens = tokenize("x");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[0].lexeme, "x");
    }

    #[test]
    fn test_identifier_with_numbers() {
        let tokens = tokenize("var123");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[0].lexeme, "var123");
    }

    #[test]
    fn test_identifier_with_underscores() {
        let tokens = tokenize("_private_var_");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[0].lexeme, "_private_var_");
    }

    #[test]
    fn test_identifier_all_caps() {
        let tokens = tokenize("CONSTANT");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[0].lexeme, "CONSTANT");
    }

    // Edge cases for operators
    #[test]
    fn test_double_equals() {
        let tokens = tokenize("==");
        assert!(matches!(tokens[0].kind, TokenKind::Operator(_)));
    }

    #[test]
    fn test_not_equals() {
        let tokens = tokenize("!=");
        assert!(matches!(tokens[0].kind, TokenKind::Operator(_)));
    }

    #[test]
    fn test_less_than_or_equal() {
        let tokens = tokenize("<=");
        assert!(matches!(tokens[0].kind, TokenKind::Operator(_)));
    }

    #[test]
    fn test_greater_than_or_equal() {
        let tokens = tokenize(">=");
        assert!(matches!(tokens[0].kind, TokenKind::Operator(_)));
    }

    #[test]
    fn test_logical_and() {
        let tokens = tokenize("&&");
        assert!(matches!(tokens[0].kind, TokenKind::Operator(_)));
    }

    #[test]
    fn test_logical_or() {
        let tokens = tokenize("||");
        assert!(matches!(tokens[0].kind, TokenKind::Operator(_)));
    }

    #[test]
    fn test_arrow() {
        let tokens = tokenize("->");
        assert_eq!(tokens[0].kind, TokenKind::Arrow);
    }

    #[test]
    fn test_double_colon() {
        let tokens = tokenize("::");
        assert_eq!(tokens[0].kind, TokenKind::ColonColon);
    }

    // Edge cases for comments
    #[test]
    fn test_single_line_comment() {
        let tokens = tokenize("// comment\n42");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
    }

    #[test]
    fn test_comment_at_end_of_file() {
        let tokens = tokenize("42 // comment");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
        assert_eq!(tokens.len(), 2); // number + EOF
    }

    #[test]
    fn test_empty_comment() {
        let tokens = tokenize("//\n42");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
    }

    // Edge cases for whitespace
    #[test]
    fn test_multiple_spaces() {
        let tokens = tokenize("1     +     2");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
        assert!(matches!(tokens[1].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[2].kind, TokenKind::IntLiteral));
    }

    #[test]
    fn test_tabs() {
        let tokens = tokenize("1\t+\t2");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
        assert!(matches!(tokens[1].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[2].kind, TokenKind::IntLiteral));
    }

    #[test]
    fn test_mixed_whitespace() {
        let tokens = tokenize("1 \t \n + \n\t 2");
        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral));
        assert!(matches!(tokens[1].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[2].kind, TokenKind::IntLiteral));
    }

    #[test]
    fn test_empty_input() {
        let tokens = tokenize("");
        assert_eq!(tokens.len(), 1); // Just EOF
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_only_whitespace() {
        let tokens = tokenize("   \n\t  \n  ");
        assert_eq!(tokens.len(), 1); // Just EOF
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    // Edge cases for keywords vs identifiers
    #[test]
    fn test_keyword_fn() {
        let tokens = tokenize("fn");
        assert!(matches!(tokens[0].kind, TokenKind::Keyword(_)));
    }

    #[test]
    fn test_identifier_fn_prefix() {
        let tokens = tokenize("function");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[0].lexeme, "function");
    }

    #[test]
    fn test_keyword_let() {
        let tokens = tokenize("let");
        assert!(matches!(tokens[0].kind, TokenKind::Keyword(_)));
    }

    #[test]
    fn test_identifier_let_suffix() {
        let tokens = tokenize("outlet");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[0].lexeme, "outlet");
    }

    // Edge cases for complex expressions
    #[test]
    fn test_chained_operators() {
        let tokens = tokenize("a + b * c - d / e");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert!(matches!(tokens[1].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[2].kind, TokenKind::Identifier));
        assert!(matches!(tokens[3].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[4].kind, TokenKind::Identifier));
        assert!(matches!(tokens[5].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[6].kind, TokenKind::Identifier));
        assert!(matches!(tokens[7].kind, TokenKind::Operator(_)));
        assert!(matches!(tokens[8].kind, TokenKind::Identifier));
    }

    #[test]
    fn test_nested_brackets() {
        let tokens = tokenize("((([[[{{{}}}}]]])))");
        assert_eq!(tokens[0].kind, TokenKind::LeftParen);
        assert_eq!(tokens[1].kind, TokenKind::LeftParen);
        assert_eq!(tokens[2].kind, TokenKind::LeftParen);
        assert_eq!(tokens[3].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[4].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[5].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[6].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[7].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[8].kind, TokenKind::LeftBrace);
    }

    #[test]
    fn test_method_chain() {
        let tokens = tokenize("obj.method1().method2().method3()");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[1].kind, TokenKind::Dot);
        assert!(matches!(tokens[2].kind, TokenKind::Identifier));
        assert_eq!(tokens[3].kind, TokenKind::LeftParen);
        assert_eq!(tokens[4].kind, TokenKind::RightParen);
        assert_eq!(tokens[5].kind, TokenKind::Dot);
    }

    // Edge cases for special characters
    #[test]
    fn test_semicolon() {
        let tokens = tokenize("let x = 5;");
        assert_eq!(tokens[4].kind, TokenKind::Semicolon);
    }

    #[test]
    fn test_comma_separated() {
        let tokens = tokenize("a, b, c");
        assert_eq!(tokens[1].kind, TokenKind::Comma);
        assert_eq!(tokens[3].kind, TokenKind::Comma);
    }

    #[test]
    fn test_question_mark() {
        let tokens = tokenize("result?");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier));
        assert_eq!(tokens[1].kind, TokenKind::Question);
    }

    // Edge cases for line/column tracking
    #[test]
    fn test_multiline_tracking() {
        let tokens = tokenize("line1\nline2\nline3");
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[2].line, 3);
    }

    #[test]
    fn test_column_tracking() {
        let tokens = tokenize("a b c");
        assert_eq!(tokens[0].column, 1);
        assert_eq!(tokens[1].column, 3);
        assert_eq!(tokens[2].column, 5);
    }
}
