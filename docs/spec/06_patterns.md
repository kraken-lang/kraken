<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>06 — Pattern Matching</h1>
</div>

## 1. Overview

Patterns destructure and test values. They appear in `match` arms, `let` bindings, and function parameters. The compiler checks `match` expressions for exhaustiveness — every possible value must be covered.

## 2. Pattern Kinds

### Wildcard Pattern

`_` matches any value and binds nothing.

```kraken
match (x) {
    _ -> { printf("anything\n"); },
}
```

### Literal Patterns

Match against a specific constant value:

```kraken
match (code) {
    0 -> { printf("ok\n"); },
    404 -> { printf("not found\n"); },
    "error" -> { printf("error\n"); },
    true -> { printf("yes\n"); },
    _ -> { printf("other\n"); },
}
```

### Identifier Patterns

Bind the matched value to a new variable:

```kraken
match (x) {
    n -> { printf("got %d\n", n); },
}
```

### Enum Variant Patterns

Match against enum variants with optional payload bindings:

```kraken
match (option) {
    Option::Some(value) -> { printf("got %d\n", value); },
    Option::None -> { printf("nothing\n"); },
}

match (result) {
    Result::Ok(data) -> { process(data); },
    Result::Err(msg) -> { printf("error: %s\n", msg); },
}
```

### Tuple Patterns

Destructure tuples positionally:

```kraken
match (pair) {
    (0, 0) -> { printf("origin\n"); },
    (x, 0) -> { printf("on x-axis: %d\n", x); },
    (0, y) -> { printf("on y-axis: %d\n", y); },
    (x, y) -> { printf("point: %d, %d\n", x, y); },
}
```

### Struct Patterns

Destructure structs by field name:

```kraken
match (point) {
    Point { x, y } -> { printf("(%d, %d)\n", x, y); },
}

// Partial match with ..
match (point) {
    Point { x, .. } -> { printf("x = %d\n", x); },
}
```

### Range Patterns

Match against numeric ranges:

```kraken
match (score) {
    0..60 -> { printf("fail\n"); },
    60..80 -> { printf("pass\n"); },
    80..=100 -> { printf("excellent\n"); },
    _ -> { printf("invalid\n"); },
}
```

- `a..b` — exclusive: matches values where `a <= x < b`.
- `a..=b` — inclusive: matches values where `a <= x <= b`.

### Or Patterns

Combine multiple patterns with `|`:

```kraken
match (day) {
    1 | 7 -> { printf("weekend\n"); },
    2 | 3 | 4 | 5 | 6 -> { printf("weekday\n"); },
    _ -> { printf("invalid\n"); },
}
```

## 3. Guard Clauses

A guard adds an `if` condition to a pattern. The arm matches only if both the pattern and the guard are satisfied:

```kraken
match (x) {
    n if n > 0 -> { printf("positive: %d\n", n); },
    n if n < 0 -> { printf("negative: %d\n", n); },
    0 -> { printf("zero\n"); },
    _ -> {},
}
```

Or patterns with guards:

```kraken
match (x) {
    1 | 2 | 3 if x != 2 -> { printf("1 or 3\n"); },
    _ -> {},
}
```

## 4. Exhaustiveness Checking

The compiler verifies that `match` expressions cover all possible values. A match is exhaustive if:

- It contains a wildcard (`_`) or identifier pattern as a catch-all, **or**
- For enums: every variant is covered, **or**
- For integers/strings: a wildcard or catch-all is present.

Non-exhaustive matches produce a compile-time error:

```
error[KRA0042]: non-exhaustive match — missing variants: Blue
```

## 5. Nested Patterns

Patterns can be nested arbitrarily:

```kraken
match (value) {
    Option::Some((x, y)) -> { printf("point: %d, %d\n", x, y); },
    Option::None -> { printf("no point\n"); },
}
```

## 6. Let Destructuring

Patterns in `let` bindings destructure values:

```kraken
let (a, b, c) = (1, 2, 3);
let Point { x, y } = get_point();
```

## 7. Function Parameter Destructuring

Function parameters can use patterns:

```kraken
fn sum_pair((a, b): (int, int)) -> int {
    return a + b;
}

fn get_name(Point { x, .. }: Point) -> int {
    return x;
}
```
