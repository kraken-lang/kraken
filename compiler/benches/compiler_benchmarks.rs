//! Compiler performance benchmarks
//!
//! Benchmarks for compiler hot paths to track performance regressions.
#![allow(clippy::result_large_err)]

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use kraken::analyzer::type_checker::TypeChecker;
use kraken::lexer::tokenizer::Tokenizer;
use kraken::parser::parser::Parser;
use std::path::PathBuf;

/// Benchmark lexer performance on various input sizes
fn bench_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");

    let small_input = "let x = 42;";
    let medium_input = r#"
        fn fibonacci(n: int) -> int {
            if n <= 1 {
                return n;
            }
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
    "#;
    let large_input = medium_input.repeat(100);

    group.bench_with_input(
        BenchmarkId::new("small", small_input.len()),
        &small_input,
        |b, input| {
            b.iter(|| {
                let mut tokenizer = Tokenizer::new(input.to_string(), PathBuf::from("bench.kr"));
                black_box(tokenizer.tokenize().unwrap())
            });
        },
    );

    group.bench_with_input(
        BenchmarkId::new("medium", medium_input.len()),
        &medium_input,
        |b, input| {
            b.iter(|| {
                let mut tokenizer = Tokenizer::new(input.to_string(), PathBuf::from("bench.kr"));
                black_box(tokenizer.tokenize().unwrap())
            });
        },
    );

    group.bench_with_input(
        BenchmarkId::new("large", large_input.len()),
        &large_input,
        |b, input| {
            b.iter(|| {
                let mut tokenizer = Tokenizer::new(input.to_string(), PathBuf::from("bench.kr"));
                black_box(tokenizer.tokenize().unwrap())
            });
        },
    );

    group.finish();
}

/// Benchmark parser performance
fn bench_parser(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    let simple_function = r#"
        fn add(a: int, b: int) -> int {
            return a + b;
        }
    "#;

    let complex_function = r#"
        fn complex(x: int, y: int) -> int {
            let mut result = 0;
            for i in 0..x {
                if i % 2 == 0 {
                    result = result + y;
                } else {
                    result = result - y;
                }
            }
            return result;
        }
    "#;

    group.bench_function("simple_function", |b| {
        b.iter(|| {
            let mut tokenizer =
                Tokenizer::new(simple_function.to_string(), PathBuf::from("bench.kr"));
            let tokens = tokenizer.tokenize().unwrap();
            let mut parser = Parser::new(tokens, PathBuf::from("bench.kr"));
            black_box(parser.parse().unwrap())
        });
    });

    group.bench_function("complex_function", |b| {
        b.iter(|| {
            let mut tokenizer =
                Tokenizer::new(complex_function.to_string(), PathBuf::from("bench.kr"));
            let tokens = tokenizer.tokenize().unwrap();
            let mut parser = Parser::new(tokens, PathBuf::from("bench.kr"));
            black_box(parser.parse().unwrap())
        });
    });

    group.finish();
}

/// Benchmark type checker performance
fn bench_type_checker(c: &mut Criterion) {
    let mut group = c.benchmark_group("type_checker");

    let simple_program = r#"
        fn main() {
            let x: int = 42;
            let y: int = x + 10;
        }
    "#;

    group.bench_function("simple_program", |b| {
        b.iter(|| {
            let mut tokenizer =
                Tokenizer::new(simple_program.to_string(), PathBuf::from("bench.kr"));
            let tokens = tokenizer.tokenize().unwrap();
            let mut parser = Parser::new(tokens, PathBuf::from("bench.kr"));
            let ast = parser.parse().unwrap();
            let mut type_checker = TypeChecker::new(PathBuf::from("bench.kr"));
            black_box(type_checker.check_program(&ast))
        });
    });

    group.finish();
}

/// Benchmark full compilation pipeline
fn bench_full_pipeline(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_pipeline");

    let program = r#"
        fn factorial(n: int) -> int {
            if n <= 1 {
                return 1;
            }
            return n * factorial(n - 1);
        }
        
        fn main() {
            let result = factorial(10);
        }
    "#;

    group.bench_function("factorial", |b| {
        b.iter(|| {
            // Lexer
            let mut tokenizer = Tokenizer::new(program.to_string(), PathBuf::from("bench.kr"));
            let tokens = tokenizer.tokenize().unwrap();

            // Parser
            let mut parser = Parser::new(tokens, PathBuf::from("bench.kr"));
            let ast = parser.parse().unwrap();

            // Type checker
            let mut type_checker = TypeChecker::new(PathBuf::from("bench.kr"));
            let _ = type_checker.check_program(&ast);

            black_box(ast)
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_lexer,
    bench_parser,
    bench_type_checker,
    bench_full_pipeline
);
criterion_main!(benches);
