<div align="center">
    <img width="auto" height="118" alt="Iron Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1>Kraken Language</h1>
</div>

**Kraken** is an open-source, general-purpose programming language.

Current version: `v0.8.14`

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

### Using direnv (Recommended)

The project includes a `.envrc` file that automatically sets up LLVM environment variables. Install [direnv](https://direnv.net/) and allow the config:

```bash
# Install direnv
brew install direnv

# Add to your shell (e.g., ~/.zshrc)
eval "$(direnv hook zsh)"

# Allow the project's .envrc
cd /path/to/kraken
direnv allow
```

After setup, LLVM paths are loaded automatically when you enter the project directory.

**IDE Note**: Most IDEs don't load direnv automatically. If you see LLVM-related errors in your IDE, they can be ignored - building from terminal with `source .envrc && cargo build` works correctly.

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

## Containers v1 (0.8.5)

Kraken now includes built-in container types with heap-allocated storage:

| Type | Description |
|------|-------------|
| `VecInt` | Dynamic array of `int` |
| `VecString` | Dynamic array of `string` |
| `VecBytes` | Dynamic array of `bytes` |
| `MapStringInt` | String-keyed map with `int` values |
| `MapStringString` | String-keyed map with `string` values |

See `docs/CONTAINERS.md` for full API reference and `examples/vec_demo.kr`, `examples/map_demo.kr` for usage examples.

## Standard Types v1 (0.8.4)

- **`string`**: currently lowered to an `i8*` and primarily used for **C-string** text at the libc/FFI boundary.
- **`bytes`**: currently lowered to an `i8*` and used for **raw buffers** and **opaque handles** (e.g. `malloc` pointers, `FILE*`-like values).
- **Indexing**:
  - `string[i]` returns an `int` in the range `0..255` (byte indexing).
  - `bytes[i]` returns an `int` in the range `0..255` (byte indexing).
- **C-string helpers**:
  - `cstr(string) -> bytes`: explicit boundary helper for passing text to APIs expecting an `i8*`.
  - `from_cstr(bytes) -> string`: explicit boundary helper for treating an `i8*` as a C-string (**traps on null**).





<!--// FOOTER
================================================= -->

<div align="center"><!--// COPYRIGHT  -->
    <br>
    <h2></h2>
    <sup>Copyright <small>&copy;</small> 2025 <strong></strong></sup>
</div>
<!-- ============================================ -->
