#![no_main]

use libfuzzer_sys::fuzz_target;
use kraken_compiler::lexer::Lexer;
use kraken_compiler::parser::Parser;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, ignoring invalid UTF-8
    if let Ok(input) = std::str::from_utf8(data) {
        // Try to parse the input
        let lexer = Lexer::new(input, "fuzz_input.kr");
        let mut parser = Parser::new(lexer);
        
        // Attempt to parse without panicking
        let _ = parser.parse_program();
    }
});
