# Platform Notes (macOS / Linux)

This document captures the platform-specific requirements and expectations for building and running the Kraken compiler.

## macOS

### Tooling

- **LLVM 18** is required (via `llvm-sys`).
- **Clang** is used as the system linker.

### Homebrew

- Install LLVM:

```bash
brew install llvm@18
```

- Ensure `llvm-sys` can find LLVM:

```bash
export LLVM_SYS_180_PREFIX="$(brew --prefix llvm@18)"
export PATH="${LLVM_SYS_180_PREFIX}/bin:${PATH}"
llvm-config --version
```

## Linux

### Tooling

- **LLVM 18** is required (via `llvm-sys`).
- **Clang** is used as the system linker.

### Packages

Exact package names differ by distribution; the build expects:

- `clang`
- `llvm-config` for LLVM 18

You must set `LLVM_SYS_180_PREFIX` to the LLVM 18 installation root so `llvm-sys` can locate it.

## CI notes

- Run the strict gate:

```bash
cargo fmt
RUSTFLAGS="-D warnings" cargo test -p kraken
RUSTFLAGS="-D warnings" cargo clippy -p kraken --all-targets --all-features
```

- If LLVM is not discovered, the build will fail with an `llvm-sys` error. Ensure `LLVM_SYS_180_PREFIX` is set and `llvm-config` is on `PATH`.
