<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>09 — Memory Model</h1>
</div>

## 1. Overview

Kraken uses **manual memory management** by default. The programmer controls allocation and deallocation explicitly. The language provides safety tools (`defer`, bounds checking, leak detection) but does not enforce ownership or lifetimes at compile time.

## 2. Stack Allocation

Local variables and function parameters are stack-allocated. They are automatically freed when their enclosing scope exits.

```kraken
fn example() -> void {
    let x = 42;           // stack-allocated
    let p = Point { x: 1, y: 2 };  // stack-allocated struct
}   // x and p freed here
```

## 3. Heap Allocation

Heap memory is allocated with `malloc` and freed with `free`. The programmer is responsible for matching every allocation with a deallocation.

```kraken
let buf: bytes = malloc(1024);
// ... use buf ...
free(buf);
```

### Allocation Functions

| Function | Description |
|----------|-------------|
| `malloc(size: int) -> bytes` | Allocate `size` bytes |
| `calloc(count: int, size: int) -> bytes` | Allocate zeroed memory |
| `realloc(ptr: bytes, new_size: int) -> bytes` | Resize allocation |
| `free(ptr: bytes) -> void` | Free allocation |
| `kraken_aligned_alloc(align: int, size: int) -> bytes` | Aligned allocation |

All allocation functions trap (abort) on failure (null return).

## 4. Defer for Cleanup

`defer` ensures cleanup code runs when the scope exits, preventing resource leaks:

```kraken
fn process_file(path: string) -> int {
    let f = fopen(path, "r");
    defer fclose(f);

    let buf = malloc(4096);
    defer free(buf);

    // Multiple defers execute in LIFO order:
    // 1. free(buf)
    // 2. fclose(f)
    return read_and_process(f, buf);
}
```

## 5. Pointers & References

### References (Safe)

References are safe pointers that cannot be null:

```kraken
let x = 42;
let r: &int = &x;        // immutable reference
let value = *r;           // dereference: 42
```

### Raw Pointers (Unsafe)

Raw pointers require `unsafe` blocks for dereferencing:

```kraken
unsafe {
    let raw: *mut int = malloc(8);
    *raw = 42;
    let v = *raw;
    free(raw);
}
```

| Type | Description |
|------|-------------|
| `&T` | Immutable reference (safe) |
| `&mut T` | Mutable reference (safe) |
| `*const T` | Raw immutable pointer (unsafe to deref) |
| `*mut T` | Raw mutable pointer (unsafe to deref) |

## 6. String Memory

String literals are statically allocated and live for the duration of the program. Strings created by concatenation or runtime operations are heap-allocated and must be freed.

```kraken
let static_str = "hello";           // static — do not free
let dynamic = str_concat("a", "b"); // heap — must free
defer free(dynamic);
```

## 7. Container Ownership

Containers (`VecInt`, `MapStringInt`, etc.) own their backing storage. The programmer must call the corresponding `*_free()` function:

```kraken
let v = vec_int_new();
vec_int_push(v, 1);
vec_int_push(v, 2);
// ... use v ...
vec_int_free(v);    // frees backing array
```

## 8. Debug Safety Tools

### Bounds Checking

Enable with `KRAKEN_BOUNDS_CHECK=1` environment variable. Array/string indexing is checked at runtime:

```kraken
let arr = [1, 2, 3];
let x = arr[5];    // traps with: "index 5 out of bounds for length 3"
```

### Leak Detection

Enable with `KRAKEN_LEAK_CHECK=1`. Reports unfreed allocations at program exit:

```
[LEAK] 1024 bytes allocated at main.kr:42 never freed
```

### Null Pointer Checks

All FFI boundary functions validate pointers. Null pointer access traps with a descriptive message.

## 9. Unsafe Code

The `unsafe` keyword marks code that bypasses safety checks:

```kraken
unsafe fn raw_access(ptr: *mut int) -> int {
    return *ptr;
}

unsafe {
    let p: *mut int = malloc(8);
    *p = 42;
    free(p);
}
```

Operations requiring `unsafe`:
- Dereferencing raw pointers
- Calling functions marked `unsafe`
- Accessing union fields
- Performing pointer arithmetic
