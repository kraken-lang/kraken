/// Token types: keywords, operators, literals, and punctuation for the Kraken language.
pub mod token;
/// Tokenizer: converts source text into a stream of tokens with source locations.
pub mod tokenizer;

#[cfg(test)]
mod edge_case_tests;
