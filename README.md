<div align="center">
    <img width="auto" height="118" alt="Iron Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1>Kraken Language</h1>
</div>

**Kraken** is an open-source, general-purpose programming language designed for performance, safety, and expressiveness.

Current version: `v0.8.20`

## Language Features

### Type System (0.8.14)
- **Tuples**: `(int, string, bool)` with destructuring and pattern matching
- **Enums**: Variant types with payloads (`Option<T>`, `Result<T, E>`)
- **Pattern Matching**: Comprehensive match expressions with literal, tuple, and enum patterns
- **Generics**: Generic functions and types with monomorphization

### Iteration & Ranges (0.8.15)
- **Range Types**: `0..10` (exclusive), `0..=10` (inclusive)
- **For-In Loops**: `for (x in 0..10) { ... }`
- **Range Patterns**: Match expressions with range patterns

### Advanced Patterns (0.8.16)
- **Or Patterns**: `1 | 2 | 3 -> { ... }`
- **Guard Clauses**: `pattern if condition -> { ... }`
- **Exhaustiveness Checking**: Compile-time validation of match coverage

### Struct Patterns & Advanced Destructuring (0.8.17)
- **Struct Patterns**: `Point { x, y } -> { ... }` in match expressions
- **Partial Patterns**: `Point { x, .. }` to ignore remaining fields
- **Let Destructuring**: `let Point { x, y } = point;`
- **Function Parameter Destructuring**: `fn foo((x, y): (int, int))` and `fn bar(Point { x, y }: Point)`
- **Enum Struct Payloads**: `enum Shape { Circle(int), Point { x: int, y: int } }`

### Operator Desugaring & Error Handling (0.8.18)
- **`?` Operator Infrastructure**: Full compiler support for try operator syntax
- **Error Propagation Patterns**: Result and Option enum-based error handling
- **AST Desugaring Framework**: Infrastructure for operator transformations

### Closures & Lambdas (0.8.19)
- **Closure Syntax**: `|x, y| x + y` and `|x| { ... }` with full type inference
- **Function Types**: `fn(int, int) -> int` for function pointers
- **Capture Analysis**: Automatic detection of captured variables
- **Move Closures**: `move |x| ...` for capture by value
- **Higher-Order Functions**: Functions accepting and returning closures
- **Comprehensive Tests**: 1,027 lines of tests covering all scenarios

### Type Aliases & Impl Blocks (0.8.20)
- **Type Aliases**: `type MyInt = int;` with generic support
- **Impl Blocks**: `impl TypeName { ... }` for associated functions
- **Generic Impl Blocks**: `impl<T> Vec<T> { ... }` syntax support
- **Visibility Control**: Public and private type aliases and methods
- **Infrastructure Complete**: Full parsing and compiler pass support

### Core Features
- **Static Typing**: Strong type system with type inference
- **LLVM Backend**: Efficient native code generation
- **C FFI**: Seamless interop with C libraries
- **Module System**: Organized code with imports and visibility control
- **Memory Safety**: Ownership-based memory management (in development)

## Workspace Layout

 - **compiler/** — Kraken compiler implementation
 - **runtime/** — Runtime library and C FFI bindings
 - **examples/** — Example programs
 - **tests/programs/** — Comprehensive test suite

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

## Quick Examples

### Tuples & Destructuring

```kraken
fn main() -> int {
    let point: (int, int) = (10, 20);
    let (x, y) = point;
    
    puts("Point coordinates:");
    puts(x);  // 10
    puts(y);  // 20
    
    0;
}
```

### Pattern Matching with Or Patterns & Guards

```kraken
fn classify(x: int) -> string {
    match (x) {
        0 -> { "zero" }
        1 | 2 | 3 -> { "one, two, or three" }
        _ if x < 0 -> { "negative" }
        _ if x < 10 -> { "single digit" }
        10..100 -> { "two digits" }
        _ -> { "large number" }
    }
}
```

### For-In Loops

```kraken
fn factorial(n: int) -> int {
    let result: int = 1;
    for (i in 1..=n) {
        result = result * i;
    }
    result;
}

fn main() -> int {
    puts(factorial(5));  // 120
    0;
}
```

### Enums (Option & Result)

```kraken
fn safe_divide(a: int, b: int) -> int {
    if (b == 0) {
        return 0;
    }
    a / b;
}

fn main() -> int {
    let result: int = safe_divide(10, 2);
    puts(result);  // 5
    0;
}
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
