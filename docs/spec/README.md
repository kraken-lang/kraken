<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE</sup></sub><br>Specification</h1>
</div>

This directory contains the authoritative language specification for Kraken **v0.9.3**.

## Chapters

| # | File | Title |
|---|------|-------|
| 00 | [`00_introduction.md`](00_introduction.md) | Introduction |
| 01 | [`01_basic_syntax.md`](01_basic_syntax.md) | Lexical Structure |
| 02 | [`02_types.md`](02_types.md) | Types |
| 03 | [`03_declarations.md`](03_declarations.md) | Declarations |
| 04 | [`04_expressions.md`](04_expressions.md) | Expressions |
| 05 | [`05_statements.md`](05_statements.md) | Statements & Control Flow |
| 06 | [`06_patterns.md`](06_patterns.md) | Pattern Matching |
| 07 | [`07_generics_traits.md`](07_generics_traits.md) | Generics & Traits |
| 08 | [`08_modules_visibility.md`](08_modules_visibility.md) | Modules & Visibility |
| 09 | [`09_memory_model.md`](09_memory_model.md) | Memory Model |
| 10 | [`10_error_handling.md`](10_error_handling.md) | Error Handling |
| 11 | [`11_concurrency.md`](11_concurrency.md) | Concurrency |
| 12 | [`12_ffi_abi.md`](12_ffi_abi.md) | FFI & ABI |
| 13 | [`13_macros.md`](13_macros.md) | Macros & Compile-Time |
| 14 | [`14_standard_library.md`](14_standard_library.md) | Standard Library |

## Formal Grammar

The complete EBNF grammar is in [`grammar.ebnf`](grammar.ebnf).

## Conventions

- Files are numbered `##_name.md` and ordered by dependency.
- `code spans` denote Kraken source text.
- **shall** / **must** = required (error if violated).
- **should** = recommended (warning if violated).
- Undefined behaviour is explicitly noted where it exists.
