#![no_main]

use libfuzzer_sys::fuzz_target;
use kraken_compiler::lexer::Lexer;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, ignoring invalid UTF-8
    if let Ok(input) = std::str::from_utf8(data) {
        // Try to lex the input
        let mut lexer = Lexer::new(input, "fuzz_input.kr");
        
        // Consume all tokens without panicking
        while let Ok(token) = lexer.next_token() {
            if token.kind == kraken_compiler::lexer::TokenKind::Eof {
                break;
            }
        }
    }
});
