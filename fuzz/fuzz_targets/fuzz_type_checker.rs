#![no_main]

use libfuzzer_sys::fuzz_target;
use kraken_compiler::lexer::Lexer;
use kraken_compiler::parser::Parser;
use kraken_compiler::analyzer::type_checker::TypeChecker;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, ignoring invalid UTF-8
    if let Ok(input) = std::str::from_utf8(data) {
        // Try to parse and type check the input
        let lexer = Lexer::new(input, "fuzz_input.kr");
        let mut parser = Parser::new(lexer);
        
        if let Ok(ast) = parser.parse_program() {
            // Attempt type checking without panicking
            let mut type_checker = TypeChecker::new();
            let _ = type_checker.check_program(&ast);
        }
    }
});
