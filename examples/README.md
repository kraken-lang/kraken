# Kraken Examples

This directory contains example programs demonstrating Kraken's features.

## Running Examples

```bash
# From the repo root
cargo run -p kraken -- build examples/<example>.kr

# Build everything in the examples directory.
# Note: files without a `main` entrypoint (module-only sources) are skipped.
cargo run -p kraken -- build examples/

# Executables are emitted to ./build/
./build/<example>
```

## Available Examples

### 1. hello.kr
**Basic "Hello World"**
- Demonstrates: Basic function, string output
- Features: `puts()`, `main()` function

### 2. showcase.kr
**Comprehensive Feature Showcase**
- Demonstrates: All major v0.8.2 features
- Features:
  - Fibonacci sequence with loops
  - Pattern matching with `match`
  - Bitwise operations (`&`, `|`, `^`, `<<`, `>>`, `~`)
  - Array operations and indexing
  - String operations (`strlen`, `strcmp`)
  - Math operations (`abs`, `rand`)
  - System integration (`setenv`, `getenv`)

### 3. calculator.kr
**Calculator with Structs**
- Demonstrates: Struct usage, function calls
- Features:
  - Custom struct types
  - Arithmetic operations
  - Bitwise operations
  - Match-based operation dispatch

### 4. modules_example_main.kr
**Multi-file Modules + Imports**
- Demonstrates: File-based modules via `module` declarations + `import`
- Files:
  - `modules_example_main.kr` (entrypoint)
  - `modules_example_util.kr` (imported module, no `main`)

## Feature Coverage

### Language Features
- ✅ Functions with parameters and return values
- ✅ Variables and assignments
- ✅ Control flow (`if`, `for`, `match`)
- ✅ Arrays and indexing
- ✅ Structs and member access
- ✅ Pattern matching
- ✅ Operators (arithmetic, logical, bitwise)

### Standard Library (80 functions)
- ✅ String operations (11 functions)
- ✅ Memory management (6 functions)
- ✅ Math operations (21 functions)
- ✅ File I/O (16 functions)
- ✅ System integration (5 functions)
- ✅ Character operations (8 functions)
- ✅ Type conversion (2 functions)
- ✅ Random & time (3 functions)
- ✅ Console I/O (5 functions)
- ✅ Error handling (1 function)
- ✅ Utilities (1 function)

### Bitwise Operations
- ✅ AND (`&`)
- ✅ OR (`|`)
- ✅ XOR (`^`)
- ✅ NOT (`~`)
- ✅ Left shift (`<<`)
- ✅ Right shift (`>>`)

## Building Your Own Programs

### Basic Structure
```kraken
fn main() -> int {
    puts("Hello, Kraken!");
    return 0;
}
```

### With Functions
```kraken
fn add(x: int, y: int) -> int {
    return x + y;
}

fn main() -> int {
    let result = add(10, 20);
    return result;
}
```

### With Structs
```kraken
struct Point {
    x: int;
    y: int;
}

fn main() -> int {
    let p = Point { x: 10, y: 20 };
    let sum = p.x + p.y;
    return sum;
}
```

### With Arrays
```kraken
fn main() -> int {
    let numbers = [1, 2, 3, 4, 5];
    let first = numbers[0];
    return first;
}
```

### With Pattern Matching
```kraken
fn main() -> int {
    let value = 42;
    
    match (value) {
        42 -> {
            puts("Found it!");
        }
        _ -> {
            puts("Not found");
        }
    }
    
    return 0;
}
```

## Tips

1. **All functions must have a return type** - Use `int`, `float`, `bool`, `string`, or `void`
2. **Variables are immutable by default** - Use `let` for declarations
3. **Struct fields use semicolons** - `field: type;`
4. **Arrays are stack-allocated** - Size determined at compile time
5. **Match arms use `->` syntax** - `pattern -> { body }`

## More Information

See the main [CHANGELOG.md](../CHANGELOG.md) for detailed feature documentation and version history.
