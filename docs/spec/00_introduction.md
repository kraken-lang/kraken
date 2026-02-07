<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>00 — Introduction</h1>
</div>

## 1. Purpose

This document defines the Kraken programming language. It is the authoritative reference for language syntax, semantics, type rules, and runtime behaviour. Implementations that conform to this specification are valid Kraken compilers.

## 2. Design Goals

Kraken is a compiled, statically typed, systems-level language. Its design targets three properties simultaneously:

- **Familiar surface** — C-shaped syntax; developers productive within hours.
- **Low-level control** — manual memory, raw pointers, FFI to C, zero-cost abstractions.
- **Modern safety** — traits, generics, pattern matching, `defer`, optional bounds checks.

## 3. Influences

| Influence | What Kraken borrows |
|-----------|---------------------|
| C | Manual memory, pointer arithmetic, FFI story |
| Rust | Traits, enums with payloads, pattern matching, `match` exhaustiveness |
| Go | Simplicity, fast compilation, `defer` |
| Zig | Explicit allocation, comptime, no hidden control flow |

## 4. Notation

The formal grammar uses ISO/IEC 14977 EBNF notation. See [`grammar.ebnf`](grammar.ebnf) for the complete grammar.

In prose, `code spans` denote Kraken source text. *Italic* terms are defined elsewhere in this specification.

## 5. Conformance

A conforming implementation must:

1. Accept every program that is well-formed according to this specification.
2. Reject every program that violates a rule marked **shall** or **must**.
3. Produce diagnostics for rules marked **should** (warnings, not errors).
4. Behave as described for all defined operations; undefined behaviour is explicitly noted.

## 6. Versioning

This specification corresponds to Kraken **v0.9.2**. Language changes are governed by semantic versioning: breaking changes increment the minor version during the 0.x series and the major version at 1.0+.

## 7. Document Structure

| Chapter | Title | Covers |
|---------|-------|--------|
| 00 | Introduction | This chapter |
| 01 | Lexical Structure | Tokens, keywords, operators, literals, comments |
| 02 | Types | Primitives, composites, generics, type inference |
| 03 | Declarations | Variables, functions, structs, enums, unions, traits |
| 04 | Expressions | Operators, precedence, calls, closures, ranges |
| 05 | Statements & Control Flow | If, while, for, match, defer, break, continue |
| 06 | Pattern Matching | Patterns, exhaustiveness, guards, or-patterns |
| 07 | Generics & Traits | Generic parameters, where clauses, trait objects |
| 08 | Modules & Visibility | Module declarations, imports, `pub` semantics |
| 09 | Memory Model | Manual allocation, pointers, references, `unsafe` |
| 10 | Error Handling | Result, Option, `?` operator, panics |
| 11 | Concurrency | async/await, spawn, threads, channels, atomics |
| 12 | FFI & ABI | C interop, repr attributes, variadic functions |
| 13 | Macros & Compile-Time | Declarative macros, const functions, static_assert |
| 14 | Standard Library | Built-in functions, containers, string operations |
