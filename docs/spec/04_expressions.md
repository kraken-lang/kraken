<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>04 — Expressions</h1>
</div>

## 1. Overview

Expressions produce values. Every expression has a type determined at compile time. Expressions can appear as statements (followed by `;`), as initializers, as function arguments, or nested inside other expressions.

## 2. Literal Expressions

```kraken
42              // int
3.14            // float
"hello"         // string
true            // bool
null            // null pointer
```

See [Chapter 01 — Lexical Structure](01_basic_syntax.md) for literal syntax.

## 3. Identifier Expressions

An identifier evaluates to the value of the variable or constant it names.

```kraken
let x = 10;
let y = x + 1;   // x is an identifier expression
```

## 4. Binary Expressions

Binary expressions apply an operator to two operands.

```kraken
let sum = a + b;
let eq = x == y;
let both = p && q;
```

### Arithmetic Operators

| Operator | Operation | Operand Types | Result |
|----------|-----------|---------------|--------|
| `+` | Addition | `int`, `float` | Same |
| `-` | Subtraction | `int`, `float` | Same |
| `*` | Multiplication | `int`, `float` | Same |
| `/` | Division | `int`, `float` | Same |
| `%` | Remainder | `int` | `int` |

Integer division truncates toward zero. Division by zero is undefined behaviour.

### Comparison Operators

| Operator | Description | Result |
|----------|-------------|--------|
| `==` | Equal | `bool` |
| `!=` | Not equal | `bool` |
| `<` | Less than | `bool` |
| `<=` | Less or equal | `bool` |
| `>` | Greater than | `bool` |
| `>=` | Greater or equal | `bool` |

### Logical Operators

| Operator | Description | Short-circuit |
|----------|-------------|---------------|
| `&&` | Logical AND | Yes — right side not evaluated if left is `false` |
| `\|\|` | Logical OR | Yes — right side not evaluated if left is `true` |

### Bitwise Operators

| Operator | Description |
|----------|-------------|
| `&` | Bitwise AND |
| `\|` | Bitwise OR |
| `^` | Bitwise XOR |
| `<<` | Left shift |
| `>>` | Arithmetic right shift |

## 5. Unary Expressions

| Operator | Description | Operand | Result |
|----------|-------------|---------|--------|
| `-` | Numeric negation | `int`, `float` | Same |
| `!` | Logical NOT | `bool` | `bool` |
| `~` | Bitwise NOT | `int` | `int` |
| `&` | Reference | any `T` | `&T` |
| `*` | Dereference | `&T`, `*T` | `T` |

```kraken
let neg = -x;
let not_flag = !flag;
let bits = ~mask;
let r = &value;
let v = *r;
```

## 6. Assignment Expressions

Assignment writes a value to a target (variable, field, index).

```kraken
x = 42;
point.x = 10;
arr[0] = 99;
```

### Compound Assignment

```kraken
x += 1;     // x = x + 1
x -= 2;     // x = x - 2
x *= 3;     // x = x * 3
x /= 4;     // x = x / 4
x %= 5;     // x = x % 5
```

## 7. Function Call Expressions

```kraken
let result = add(1, 2);
printf("value: %d\n", x);
```

### Turbofish Syntax

Explicit type arguments use `::< >` syntax:

```kraken
let id = identity::<int>(42);
```

## 8. Member Access Expressions

The `.` operator accesses struct fields:

```kraken
let x = point.x;
let name = person.name;
```

## 9. Index Expressions

The `[]` operator indexes into arrays, strings, and bytes:

```kraken
let first = arr[0];
let ch = name[0];     // byte value at index 0
```

## 10. Slice Expressions

The `[start:end]` syntax creates a substring or subslice:

```kraken
let sub = name[0:5];     // first 5 characters
```

## 11. Tuple Expressions

```kraken
let pair = (42, "hello");
let unit = ();
```

### Tuple Indexing

```kraken
let x = pair.0;      // 42
let s = pair.1;      // "hello"
```

## 12. Array Expressions

```kraken
let nums = [1, 2, 3, 4, 5];
let empty: [int] = [];
```

## 13. Struct Literal Expressions

```kraken
let p = Point { x: 10, y: 20 };
let b = Box::<int> { value: 42 };
```

## 14. Enum Variant Expressions

```kraken
let c = Color::Red;
let some = Option::Some(42);
let ok = Result::Ok("success");
```

## 15. Range Expressions

```kraken
let exclusive = 0..10;      // 0, 1, ..., 9
let inclusive = 0..=10;      // 0, 1, ..., 10
```

Ranges are used in `for-in` loops and match patterns.

## 16. Closure Expressions

Closures are anonymous functions that can capture variables from their enclosing scope.

```kraken
let add = |a: int, b: int| -> int { return a + b; };
let double = |x| x * 2;
let captured = move |x| x + offset;
```

- **Expression body**: `|params| expr`
- **Block body**: `|params| { statements }`
- **`move`**: captures variables by value instead of by reference.

## 17. Reference & Dereference Expressions

```kraken
let r = &x;          // create reference
let v = *r;          // dereference
```

## 18. Await Expressions

```kraken
let data = await fetch("https://example.com");
```

`await` suspends the current async function until the future completes.

## 19. Spawn Expressions

```kraken
let handle = spawn {
    heavy_computation();
};
```

`spawn` creates a new concurrent task.

## 20. Try Expressions

The `?` operator propagates errors from `Result` or `Option` types:

```kraken
let value = get_result()?;
```

If the expression is `Err(e)`, the function returns `Err(e)` early. If `Ok(v)`, evaluates to `v`.

## 21. Operator Precedence

See [Chapter 01 — Lexical Structure, Section 5](01_basic_syntax.md) for the complete precedence table.
