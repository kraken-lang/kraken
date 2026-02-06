<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>02 — Types</h1>
</div>

## 1. Overview

Kraken is statically typed. Every value has a type known at compile time. Types are divided into primitive types, composite types, container types, pointer/reference types, and special types.

## 2. Primitive Types

| Type | Size | Description | Default |
|------|------|-------------|---------|
| `int` | 64-bit | Signed integer (`i64`) | `0` |
| `float` | 64-bit | IEEE 754 double-precision | `0.0` |
| `bool` | 1 byte | Boolean value | `false` |
| `string` | pointer | Null-terminated C string (`i8*`) | `""` |
| `str` | pointer | Borrowed string view (read-only `i8*`) | — |
| `bytes` | pointer | Raw byte buffer (`i8*`) | `null` |
| `void` | 0 | No value; used for functions that return nothing | — |

### int

A 64-bit signed integer. Range: −2^63 to 2^63 − 1. Arithmetic overflow is undefined behaviour in release builds; debug builds may trap.

```kraken
let x: int = 42;
let y = -100;
let hex = 0xFF;
```

### float

A 64-bit IEEE 754 double-precision floating-point number.

```kraken
let pi: float = 3.14159;
let sci = 1.5e10;
```

### bool

A boolean value: `true` or `false`. Stored as 1 byte. Used in conditions and logical operations.

```kraken
let flag: bool = true;
if (flag) { printf("yes\n"); }
```

### string

A pointer to a null-terminated UTF-8 byte sequence. String literals produce `string` values. Strings are immutable by convention; mutation requires explicit buffer management.

```kraken
let name: string = "Kraken";
let len = strlen(name);
```

### str

A borrowed string view. Semantically identical to `string` in the current implementation (both are `i8*`), but signals intent: the callee does not own the data.

### bytes

A raw byte pointer (`i8*`). Used for buffers, opaque handles, and FFI boundaries where the data is not necessarily text.

```kraken
let buf: bytes = malloc(1024);
defer free(buf);
```

### void

The unit type. Functions that do not return a value have return type `void`. `void` cannot be used as a variable type.

## 3. Container Types

Kraken provides concrete container types as language built-ins. These are backed by heap-allocated C runtime structures.

| Type | Element | Description |
|------|---------|-------------|
| `VecInt` | `int` | Dynamic array of integers |
| `VecString` | `string` | Dynamic array of strings |
| `VecBytes` | `bytes` | Dynamic array of byte buffers |
| `MapStringInt` | `string` → `int` | Hash map with string keys and int values |
| `MapStringString` | `string` → `string` | Hash map with string keys and string values |

```kraken
let v: VecInt = vec_int_new();
vec_int_push(v, 42);
let n = vec_int_len(v);
vec_int_free(v);
```

## 4. Slice Types

Slices are borrowed views into contiguous data. They carry a pointer and a length.

| Type | Element | Description |
|------|---------|-------------|
| `SliceInt` | `int` | Read-only view of integers |
| `SliceString` | `string` | Read-only view of strings |
| `SliceBytes` | `bytes` | Read-only view of byte buffers |

## 5. Array Types

Fixed-size or unsized arrays of a single element type.

```kraken
let fixed: [int; 5] = [1, 2, 3, 4, 5];
let dynamic: [int] = [10, 20, 30];
```

- `[T; N]` — fixed-size array of `N` elements of type `T`.
- `[T]` — unsized array (pointer-sized in practice).

Array indexing: `arr[i]` returns the element at index `i`. Out-of-bounds access is undefined behaviour unless debug bounds checking is enabled (`KRAKEN_BOUNDS_CHECK=1`).

## 6. Tuple Types

A tuple is an ordered, heterogeneous, fixed-size collection of values.

```kraken
let pair: (int, string) = (42, "hello");
let x = pair.0;    // 42
let s = pair.1;    // "hello"
```

The empty tuple `()` serves as the unit value.

Tuples support destructuring:

```kraken
let (a, b) = pair;
```

## 7. Function Types

Function types describe callable values.

```kraken
let add: fn(int, int) -> int = some_function;
```

Syntax: `fn(P1, P2, ...) -> R` where `P1..Pn` are parameter types and `R` is the return type.

## 8. Reference & Pointer Types

| Syntax | Description |
|--------|-------------|
| `&T` | Immutable reference to `T` |
| `&mut T` | Mutable reference to `T` |
| `*const T` | Raw immutable pointer (unsafe) |
| `*mut T` | Raw mutable pointer (unsafe) |

References are safe pointers. Raw pointers require `unsafe` blocks for dereferencing.

```kraken
fn increment(x: &int) -> int {
    return *x + 1;
}

unsafe {
    let raw: *mut int = malloc(8);
    *raw = 42;
    free(raw);
}
```

## 9. Trait Object Types

A trait object is a dynamically-dispatched value that implements a given trait.

```kraken
let drawable: dyn Draw = get_shape();
let sendable: dyn Draw + Send = get_concurrent_shape();
```

Trait objects use fat pointers: a data pointer and a vtable pointer.

## 10. Generic Types

Generic types are parameterized by one or more type variables.

```kraken
struct Box<T> {
    value: T;
}

let b: Box<int> = Box::<int> { value: 42 };
```

Generic containers are lowered to concrete types during monomorphization:
- `Vec<int>` → `VecInt`
- `Vec<string>` → `VecString`
- `Map<string, int>` → `MapStringInt`

## 11. Custom Types

Any user-defined struct, enum, union, or type alias introduces a named type.

```kraken
struct Point { x: int; y: int; }
let p: Point = Point { x: 1, y: 2 };
```

## 12. Type Inference

Kraken infers types for local variables when an initializer is present:

```kraken
let x = 42;          // inferred: int
let s = "hello";     // inferred: string
let v = vec_int_new(); // inferred: VecInt
```

Function parameters and return types must always be explicitly annotated. Generic type arguments can be inferred from call-site context or specified explicitly via turbofish syntax:

```kraken
let result = identity::<int>(42);
```
