<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>05 — Statements & Control Flow</h1>
</div>

## 1. Expression Statements

Any expression followed by a semicolon is an expression statement:

```kraken
printf("hello\n");
x + 1;
```

## 2. Variable & Constant Declarations

See [Chapter 03 — Declarations](03_declarations.md).

## 3. Return Statements

```kraken
return 42;
return;          // void return
```

A `return` statement exits the current function. If the function has a non-void return type, a value must be provided.

## 4. If Statements

Conditions are enclosed in parentheses. Braces are required for the body.

```kraken
if (x > 0) {
    printf("positive\n");
}

if (x > 0) {
    printf("positive\n");
} else {
    printf("non-positive\n");
}

if (x > 0) {
    printf("positive\n");
} else if (x == 0) {
    printf("zero\n");
} else {
    printf("negative\n");
}
```

The condition must have type `bool`.

## 5. While Loops

```kraken
while (x > 0) {
    x = x - 1;
}
```

The condition is evaluated before each iteration. The loop body executes while the condition is `true`.

## 6. For Loops (C-style)

C-style for loops have three parts: initializer, condition, and increment. All three are optional. Parentheses are required.

```kraken
for (let i = 0; i < 10; i = i + 1) {
    printf("%d\n", i);
}

// Infinite loop
for (;;) {
    break;
}
```

The initializer can be a variable declaration or expression statement. The condition must be `bool`. The increment is an expression evaluated after each iteration.

## 7. For-In Loops

Iterator-based loops over ranges and collections:

```kraken
for (i in 0..10) {
    printf("%d\n", i);
}

for (i in 0..=100) {
    // inclusive: 0 through 100
}
```

The loop variable is scoped to the loop body.

## 8. Match Statements

Pattern matching with exhaustiveness checking. Match arms use `->` (not `=>`).

```kraken
match (value) {
    0 -> { printf("zero\n"); },
    1 -> { printf("one\n"); },
    _ -> { printf("other\n"); },
}
```

See [Chapter 06 — Pattern Matching](06_patterns.md) for full pattern syntax.

## 9. Break & Continue

```kraken
while (true) {
    if (done) {
        break;
    }
    if (skip) {
        continue;
    }
    process();
}
```

- **`break`** — exits the innermost loop.
- **`continue`** — skips to the next iteration of the innermost loop.

Both can be used in `while`, `for`, and `for-in` loops.

## 10. Defer Statements

`defer` schedules a statement to execute when the enclosing scope exits, regardless of how it exits (normal return or early return). Multiple defers execute in LIFO (reverse) order.

```kraken
fn read_file(path: string) -> string {
    let f = fopen(path, "r");
    defer fclose(f);

    let buf = malloc(1024);
    defer free(buf);

    // buf and f are cleaned up automatically
    return process(buf);
}
```

## 11. Unsafe Blocks

`unsafe` blocks allow operations that the compiler cannot verify for safety:

```kraken
unsafe {
    let raw: *mut int = malloc(8);
    *raw = 42;
    let value = *raw;
    free(raw);
}
```

Operations requiring `unsafe`:
- Dereferencing raw pointers (`*const T`, `*mut T`)
- Calling unsafe functions
- Accessing union fields

## 12. Blocks

A block is a sequence of statements enclosed in braces. Blocks create a new scope.

```kraken
{
    let x = 42;
    printf("%d\n", x);
}
// x is not accessible here
```
