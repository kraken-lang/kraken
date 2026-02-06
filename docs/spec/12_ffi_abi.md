<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>12 — FFI & ABI</h1>
</div>

## 1. Overview

Kraken has first-class C interoperability. Functions from the C standard library and custom C libraries can be called directly. The compiler manages ABI-correct calling conventions via LLVM.

## 2. C ABI Types

Kraken types map to C ABI types at the FFI boundary:

| Kraken Type | C ABI Type | Size |
|-------------|-----------|------|
| `int` | `int64_t` / `long long` | 8 bytes |
| `float` | `double` | 8 bytes |
| `bool` | `i8` | 1 byte |
| `string` | `const char*` (`i8*`) | pointer |
| `str` | `const char*` (`i8*`) | pointer |
| `bytes` | `void*` / `char*` (`i8*`) | pointer |
| `void` | `void` | 0 |

### C Integer Widening

C functions returning `int` (32-bit) are widened to Kraken's 64-bit `int`:
- **Signed** functions: sign-extended (`sext i32 → i64`)
- **Unsigned** functions (e.g., `size_t` returns): zero-extended (`zext i32 → i64`)

## 3. Calling C Functions

C functions are called directly by name. The compiler's stdlib table maps function names to their ABI signatures:

```kraken
let len = strlen("hello");          // calls libc strlen
let result = strcmp("a", "b");      // calls libc strcmp
let ptr = malloc(1024);             // calls libc malloc
```

### Null Safety

FFI functions that return pointers check for null. A null return from `malloc`, `fopen`, etc. causes a trap (abort with message).

### Errno Convention

Some C functions set `errno` on failure. The stdlib table tracks which functions follow this convention.

## 4. String Boundary Rules

| Direction | Operation | Semantics |
|-----------|-----------|-----------|
| Kraken → C | Pass `string` to C function | Direct pointer pass (no copy) |
| C → Kraken | C returns `char*` | Treated as `string` (borrowed) |
| CStr helpers | `cstr(s)` | Explicit cast to `bytes` |
| CStr helpers | `from_cstr(ptr)` | Convert `bytes` to `string` (traps on null) |

```kraken
let c_str: bytes = cstr(my_string);
let kr_str: string = from_cstr(raw_ptr);
```

## 5. Struct Repr Attributes

Control struct memory layout for C interoperability:

### `#[repr(C)]`

C-compatible field ordering and padding:

```kraken
#[repr(C)]
struct CPoint {
    x: int;
    y: int;
}
```

### `#[repr(packed)]`

No padding between fields:

```kraken
#[repr(packed)]
struct PackedHeader {
    magic: int;
    version: int;
    flags: int;
}
```

### `#[repr(align(N))]`

Force minimum alignment:

```kraken
#[repr(align(16))]
struct SIMDData {
    values: [float; 4];
}
```

## 6. Variadic Functions

Kraken supports calling C variadic functions:

```kraken
fn printf(format: string, ...) -> int;
```

The `...` must appear after at least one named parameter. Variadic arguments are passed according to C ABI promotion rules.

## 7. Function Pointers

The `&fn_name` syntax creates a C-callable function pointer:

```kraken
fn callback(x: int) -> int {
    return x * 2;
}

let fp = &callback;
```

Function pointer types: `fn(int) -> int`

## 8. Union Types for FFI

Unions match C union semantics — all fields share the same memory:

```kraken
union Value {
    int_val: int;
    float_val: float;
    ptr_val: bytes;
}
```

Accessing a field other than the one last written is undefined behaviour (matching C semantics).

## 9. Platform-Aware Linking

The compiler applies platform-specific linking rules:

| Platform | Default Libraries | Notes |
|----------|-------------------|-------|
| macOS | libSystem (via clang) | Includes libc, libm |
| Linux | libc (default), libm (`-lm`) | `-lm` added automatically |
| Windows | MSVC CRT (via clang) | Uses `link.exe` backend |

## 10. Runtime Library

The Kraken runtime library (`libkraken_runtime.a`) provides C implementations of string operations, collection helpers, safety checks, and other runtime functions. It is compiled per-platform and linked automatically when present.

Key runtime modules:
- **kraken_string.c** — `str_split`, `str_join`, string utilities
- **kraken_stdlib.c** — file I/O wrappers, math, time, memory
- **kraken_safety.c** — bounds checking, leak detection, null checks
- **kraken_collections.c** — collection helpers, iterators
- **kraken_ffi_safety.c** — FFI boundary validation
- **kraken_struct_layout.c** — struct padding and alignment
- **kraken_union_runtime.c** — tagged union support
- **kraken_variadic.c** — variadic function wrappers
