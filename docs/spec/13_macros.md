<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>13 — Macros & Compile-Time</h1>
</div>

## 1. Overview

Kraken provides three compile-time features: **declarative macros**, **const functions**, and **static assertions**. These allow code generation, compile-time evaluation, and build-time validation.

## 2. Declarative Macros

Macros are pattern-based code templates expanded at compile time.

### Declaration

```kraken
macro_rules! vec {
    ($elem) => {
        {
            let v = vec_int_new();
            vec_int_push(v, $elem);
            v
        }
    };
}
```

### Syntax

```
macro_rules! NAME { RULE; RULE; ... }
```

Each rule has the form:

```
(PATTERN) => { EXPANSION };
```

### Pattern Variables

- `$name` — binds a single token or expression
- `$($name)` — repetition pattern

### Hygiene

Macro expansion uses hygienic name generation to avoid variable capture. Generated names are suffixed with unique identifiers.

### Expansion

Macros are expanded after parsing, before type checking. The expanded code is subject to all normal compiler passes.

## 3. Const Functions

Functions declared with `const fn` can be evaluated at compile time.

```kraken
const fn factorial(n: int) -> int {
    if (n <= 1) { return 1; }
    return n * factorial(n - 1);
}
```

### Restrictions

Const functions may only use:
- Literal values
- Arithmetic, comparison, logical, and bitwise operators
- Calls to other const functions
- Recursion (with bounded depth)
- If/else control flow

Const functions may **not** use:
- Heap allocation
- I/O operations
- Mutable global state
- Loops (use recursion instead)

### Compile-Time Evaluation

The `ConstEvaluator` evaluates const expressions during compilation. Division by zero is detected and reported as a compile-time error.

## 4. Static Assertions

Static assertions validate conditions at compile time:

```kraken
static_assert!(1 + 1 == 2, "math is broken");
static_assert!(MAX_SIZE > 0, "MAX_SIZE must be positive");
```

If the condition evaluates to `false`, compilation fails with the provided message.

## 5. Attributes

Attributes annotate declarations with metadata:

### Function Attributes

```kraken
#[inline]
fn hot_path(x: int) -> int { return x * 2; }

#[no_mangle]
fn c_callable() -> void { }

#[test]
fn test_something() -> void { assert(1 + 1 == 2); }
```

### Type Attributes

```kraken
#[derive(Clone, Debug, PartialEq)]
struct Point {
    x: int;
    y: int;
}

#[repr(C)]
struct CCompatible {
    field: int;
}
```

### Supported Attributes

| Attribute | Target | Description |
|-----------|--------|-------------|
| `#[inline]` | Functions | Hint to inline the function |
| `#[no_mangle]` | Functions | Preserve symbol name for FFI |
| `#[test]` | Functions | Mark as a test function |
| `#[derive(...)]` | Structs/Enums | Auto-implement traits |
| `#[repr(C)]` | Structs | C-compatible layout |
| `#[repr(packed)]` | Structs | No padding |
| `#[repr(align(N))]` | Structs | Minimum alignment |

## 6. Derive Macros

The `#[derive(...)]` attribute generates trait implementations:

| Derive | Generated Method | Description |
|--------|-----------------|-------------|
| `Clone` | `clone(self) -> Self` | Deep copy of all fields |
| `Debug` | `debug(self) -> string` | Debug string representation |
| `PartialEq` | `eq(self, other) -> bool` | Field-by-field equality |
| `Eq` | (marker) | Total equality assertion |
| `PartialOrd` | `lt, le, gt, ge` | Field-by-field ordering |
| `Ord` | `cmp(self, other) -> int` | Total ordering |
| `Hash` | `hash(self) -> int` | Hash computation |
