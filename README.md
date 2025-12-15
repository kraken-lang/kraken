<div align="center">
    <img width="auto" height="118" alt="Iron Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1>Kraken Language</h1>
</div>

**Kraken** is an open-source, general-purpose programming language.

Current version: `v0.8.4`

## Workspace Layout

 - **compiler/**
 - **runtime/**
 - **examples/**
 - **tests/programs/**

## Prerequisites

 - **LLVM 18** (required to build the compiler via `llvm-sys`)
 - **Clang** (used for linking)

See `docs/platform.md` for macOS/Linux platform notes.

### macOS (Homebrew)

```bash
brew install llvm@18

# Point llvm-sys at the Homebrew LLVM install
export LLVM_SYS_180_PREFIX="$(brew --prefix llvm@18)"

# Make llvm-config available on PATH (recommended)
export PATH="${LLVM_SYS_180_PREFIX}/bin:${PATH}"

# Sanity check
llvm-config --version
```

## Build

```bash
cargo build -p kraken
```

## Lint & Tests (Strict)

```bash
cargo fmt --check
RUSTFLAGS="-D warnings" cargo test --workspace
RUSTFLAGS="-D warnings" cargo clippy --workspace --all-targets --all-features
```

## Run an Example

```bash
cargo run -p kraken -- build examples/hello.kr
./build/hello
```

## Standard Types v1 (0.8.4)

- **`string`**: currently lowered to an `i8*` and primarily used for **C-string** text at the libc/FFI boundary.
- **`bytes`**: currently lowered to an `i8*` and used for **raw buffers** and **opaque handles** (e.g. `malloc` pointers, `FILE*`-like values).
- **Indexing**:
  - `string[i]` returns an `int` in the range `0..255` (byte indexing).
  - `bytes[i]` returns an `int` in the range `0..255` (byte indexing).
- **C-string helpers**:
  - `cstr(string) -> bytes`: explicit boundary helper for passing text to APIs expecting an `i8*`.
  - `from_cstr(bytes) -> string`: explicit boundary helper for treating an `i8*` as a C-string (**traps on null**).

### Migration note

Many libc/stdlib signatures that previously accepted `string` for raw pointers/buffers were tightened to use `bytes` (e.g. `malloc/free/realloc`, `mem*`, and `FILE*`-style handles).





<!--// FOOTER
================================================= -->

<div align="center"><!--// COPYRIGHT  -->
    <br>
    <h2></h2>
    <sup>Copyright <small>&copy;</small> 2025 <strong></strong></sup>
</div>
<!-- ============================================ -->
