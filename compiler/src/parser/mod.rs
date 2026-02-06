/// Abstract syntax tree: all AST node types for the Kraken language.
pub mod ast;
#[allow(clippy::module_inception)]
/// Recursive descent parser: converts token streams into AST programs.
pub mod parser;
#[cfg(test)]
mod parser_tests;

pub use parser::Parser;
