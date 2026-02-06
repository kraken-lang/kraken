<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>10 — Error Handling</h1>
</div>

## 1. Overview

Kraken uses **value-based error handling** with `Result<T, E>` and `Option<T>` enums. There are no exceptions. Errors are explicit return values that must be handled by the caller.

## 2. Result Type

`Result<T, E>` represents either a success value or an error:

```kraken
enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

### Usage

```kraken
fn divide(a: int, b: int) -> Result<int, string> {
    if (b == 0) {
        return Result::Err("division by zero");
    }
    return Result::Ok(a / b);
}

fn main() -> int {
    match (divide(10, 3)) {
        Result::Ok(value) -> { printf("result: %d\n", value); },
        Result::Err(msg) -> { printf("error: %s\n", msg); },
    }
    return 0;
}
```

## 3. Option Type

`Option<T>` represents a value that may or may not be present:

```kraken
enum Option<T> {
    Some(T),
    None,
}
```

### Usage

```kraken
fn find(haystack: string, needle: string) -> Option<int> {
    let idx = str_index_of(haystack, needle);
    if (idx < 0) {
        return Option::None;
    }
    return Option::Some(idx);
}
```

## 4. Try Operator (`?`)

The `?` operator provides concise error propagation. When applied to a `Result` or `Option`:

- **`Result::Ok(v)`** — evaluates to `v`
- **`Result::Err(e)`** — returns `Err(e)` from the enclosing function
- **`Option::Some(v)`** — evaluates to `v`
- **`Option::None`** — returns `None` from the enclosing function

```kraken
fn process() -> Result<int, string> {
    let a = divide(10, 2)?;    // Ok(5) -> 5
    let b = divide(a, 0)?;     // Err("division by zero") -> early return
    return Result::Ok(b);
}
```

The enclosing function must return a compatible `Result` or `Option` type.

### Chained Try

```kraken
fn pipeline() -> Result<string, string> {
    let data = read_file("input.txt")?;
    let parsed = parse_json(data)?;
    let result = transform(parsed)?;
    return Result::Ok(result);
}
```

## 5. Panics and Traps

Some operations cause immediate program termination (trap/abort):

- **Null pointer dereference** in FFI boundaries
- **Out-of-bounds access** (when bounds checking is enabled)
- **`assert(false)`** — assertion failure
- **Division by zero** (in debug builds)
- **Stack overflow**

Traps produce a diagnostic message and exit with a non-zero code:

```
KRAKEN PANIC: index 5 out of bounds for length 3
  at main.kr:42
```

## 6. Assertions

```kraken
assert(x > 0);                    // aborts if false
assert_eq(a, b);                  // aborts if a != b
assert_ne(a, b);                  // aborts if a == b
```

Assertions are always checked (not stripped in release builds).

## 7. Error Handling Patterns

### Early Return with Match

```kraken
fn safe_divide(a: int, b: int) -> int {
    match (divide(a, b)) {
        Result::Ok(v) -> { return v; },
        Result::Err(msg) -> {
            printf("error: %s\n", msg);
            return 0;
        },
    }
}
```

### Default Values

```kraken
fn get_or_default(opt: Option<int>, default: int) -> int {
    match (opt) {
        Option::Some(v) -> { return v; },
        Option::None -> { return default; },
    }
}
```
