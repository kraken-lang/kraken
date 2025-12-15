<div align="center">
    <img width="auto" height="118" alt="Iron Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1>Kraken Language</h1>
</div>

**Kraken** is an open-source, general-purpose programming language.

## Workspace Layout

 - **compiler/**
 - **runtime/**
 - **examples/**
 - **tests/programs/**

## Prerequisites

 - **LLVM 18** (required to build the compiler via `llvm-sys`)
 - **Clang** (used for linking)

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
cargo fmt
cargo check
cargo test
cargo clippy --all-targets --all-features -- -D warnings
```

## Run an Example

```bash
cargo run -p kraken -- build examples/hello.kr
./build/hello
```





<!--// FOOTER
================================================= -->

<div align="center"><!--// COPYRIGHT  -->
    <br>
    <h2></h2>
    <sup>Copyright <small>&copy;</small> 2025 <strong></strong></sup>
</div>
<!-- ============================================ -->
