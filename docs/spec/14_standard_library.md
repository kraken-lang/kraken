<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>14 — Standard Library</h1>
</div>

## 1. Overview

The Kraken standard library is provided in two layers:

- **Runtime library** (`libkraken_runtime.a`) — C functions linked into every Kraken binary.
- **Compiler runtime** (Rust modules) — higher-level abstractions in the compiler's `stdlib/` directory.

All runtime functions use the `kraken_` prefix to avoid collisions with libc.

## 2. String Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_str_len` | `(s: string) -> int` | String length (bytes) |
| `kraken_str_concat` | `(a: string, b: string) -> string` | Concatenate two strings (heap) |
| `kraken_str_substring` | `(s: string, start: int, len: int) -> string` | Extract substring (heap) |
| `kraken_str_contains` | `(s: string, needle: string) -> bool` | Check substring presence |
| `kraken_str_starts_with` | `(s: string, prefix: string) -> bool` | Check prefix |
| `kraken_str_ends_with` | `(s: string, suffix: string) -> bool` | Check suffix |
| `kraken_str_to_upper` | `(s: string) -> string` | Uppercase (heap) |
| `kraken_str_to_lower` | `(s: string) -> string` | Lowercase (heap) |
| `kraken_str_trim` | `(s: string) -> string` | Trim whitespace (heap) |
| `kraken_str_replace` | `(s: string, old: string, new: string) -> string` | Replace occurrences (heap) |
| `kraken_str_split` | `(s: string, delim: string) -> VecString` | Split string into vector |
| `kraken_str_join` | `(v: VecString, sep: string) -> string` | Join vector with separator |

### String Utility Wrappers

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_sprintf` | `(fmt: string, ...) -> string` | Formatted string (heap) |
| `kraken_snprintf` | `(buf: bytes, size: int, fmt: string, ...) -> int` | Bounded format |
| `kraken_strtok` | `(s: string, delim: string) -> string` | Tokenize |
| `kraken_strdup` | `(s: string) -> string` | Duplicate (heap) |
| `kraken_strchr` | `(s: string, c: int) -> string` | Find first char |
| `kraken_strrchr` | `(s: string, c: int) -> string` | Find last char |

## 3. Memory Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `malloc` | `(size: int) -> bytes` | Allocate heap memory |
| `calloc` | `(count: int, size: int) -> bytes` | Allocate zeroed memory |
| `realloc` | `(ptr: bytes, size: int) -> bytes` | Resize allocation |
| `free` | `(ptr: bytes) -> void` | Free allocation |
| `kraken_aligned_alloc` | `(align: int, size: int) -> bytes` | Aligned allocation |
| `kraken_memcpy` | `(dst: bytes, src: bytes, n: int) -> bytes` | Copy memory |
| `kraken_memset` | `(ptr: bytes, val: int, n: int) -> bytes` | Fill memory |
| `kraken_memcmp` | `(a: bytes, b: bytes, n: int) -> int` | Compare memory |

## 4. I/O Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `printf` | `(fmt: string, ...) -> int` | Print formatted |
| `puts` | `(s: string) -> int` | Print string with newline |
| `kraken_fopen` | `(path: string, mode: string) -> bytes` | Open file |
| `kraken_fclose` | `(f: bytes) -> int` | Close file |
| `kraken_fread` | `(buf: bytes, size: int, count: int, f: bytes) -> int` | Read from file |
| `kraken_fwrite` | `(buf: bytes, size: int, count: int, f: bytes) -> int` | Write to file |
| `kraken_fseek` | `(f: bytes, offset: int, whence: int) -> int` | Seek in file |
| `kraken_ftell` | `(f: bytes) -> int` | Get file position |
| `kraken_rewind` | `(f: bytes) -> void` | Reset to start |
| `kraken_feof` | `(f: bytes) -> int` | Check end-of-file |
| `kraken_ferror` | `(f: bytes) -> int` | Check file error |

## 5. Math Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_sin` | `(x: float) -> float` | Sine |
| `kraken_cos` | `(x: float) -> float` | Cosine |
| `kraken_tan` | `(x: float) -> float` | Tangent |
| `kraken_asin` | `(x: float) -> float` | Arc sine |
| `kraken_acos` | `(x: float) -> float` | Arc cosine |
| `kraken_atan` | `(x: float) -> float` | Arc tangent |
| `kraken_atan2` | `(y: float, x: float) -> float` | Two-argument arc tangent |
| `kraken_sinh` | `(x: float) -> float` | Hyperbolic sine |
| `kraken_cosh` | `(x: float) -> float` | Hyperbolic cosine |
| `kraken_tanh` | `(x: float) -> float` | Hyperbolic tangent |
| `kraken_exp` | `(x: float) -> float` | Exponential (e^x) |
| `kraken_log` | `(x: float) -> float` | Natural logarithm |
| `kraken_log10` | `(x: float) -> float` | Base-10 logarithm |
| `kraken_log2` | `(x: float) -> float` | Base-2 logarithm |
| `kraken_pow` | `(base: float, exp: float) -> float` | Power |
| `kraken_sqrt` | `(x: float) -> float` | Square root |
| `kraken_cbrt` | `(x: float) -> float` | Cube root |
| `kraken_ceil` | `(x: float) -> float` | Ceiling |
| `kraken_floor` | `(x: float) -> float` | Floor |
| `kraken_round` | `(x: float) -> float` | Round |
| `kraken_trunc` | `(x: float) -> float` | Truncate |
| `kraken_fmod` | `(x: float, y: float) -> float` | Floating remainder |
| `kraken_fabs` | `(x: float) -> float` | Absolute value |
| `kraken_hypot` | `(x: float, y: float) -> float` | Hypotenuse |

## 6. Time Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_time` | `() -> int` | Current Unix timestamp |
| `kraken_clock` | `() -> int` | Clock ticks |
| `kraken_clocks_per_sec` | `() -> int` | CLOCKS_PER_SEC constant |
| `kraken_difftime` | `(end: int, start: int) -> float` | Time difference in seconds |

## 7. Safety Functions

### Bounds Checking

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_bounds_check` | `(index: int, length: int, file: string, line: int) -> void` | Validate index |
| `kraken_bounds_check_range` | `(start: int, end: int, length: int, file: string, line: int) -> void` | Validate range |
| `kraken_null_check` | `(ptr: bytes, msg: string) -> void` | Validate non-null |

### Leak Detection

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_malloc_tracked` | `(size: int, file: string, line: int) -> bytes` | Tracked malloc |
| `kraken_free_tracked` | `(ptr: bytes) -> void` | Tracked free |
| `kraken_get_allocation_count` | `() -> int` | Active allocations |
| `kraken_get_allocated_bytes` | `() -> int` | Total allocated bytes |
| `kraken_print_allocation_stats` | `() -> void` | Print leak report |

## 8. FFI Safety Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_ffi_check_null` | `(ptr: bytes, name: string) -> void` | Validate FFI pointer |
| `kraken_ffi_validate_ptr` | `(ptr: bytes) -> int` | Check pointer validity |
| `kraken_ffi_validate_string` | `(s: string) -> int` | Check string validity |
| `kraken_ffi_safe_malloc` | `(size: int) -> bytes` | Malloc with null check |
| `kraken_ffi_check_bounds` | `(offset: int, size: int, total: int) -> int` | Buffer bounds check |

## 9. Collection Helpers

| Function | Signature | Description |
|----------|-----------|-------------|
| `kraken_array_map` | `(arr: bytes, len: int, elem_size: int, fn: bytes) -> bytes` | Map over array |
| `kraken_array_filter` | `(arr: bytes, len: int, elem_size: int, fn: bytes) -> bytes` | Filter array |
| `kraken_array_fold` | `(arr: bytes, len: int, elem_size: int, init: int, fn: bytes) -> int` | Fold/reduce |
| `kraken_array_any` | `(arr: bytes, len: int, elem_size: int, fn: bytes) -> int` | Any predicate |
| `kraken_array_all` | `(arr: bytes, len: int, elem_size: int, fn: bytes) -> int` | All predicate |
| `kraken_array_find` | `(arr: bytes, len: int, elem_size: int, fn: bytes) -> int` | Find index |

## 10. Environment

Enable safety features via environment variables:

| Variable | Values | Description |
|----------|--------|-------------|
| `KRAKEN_BOUNDS_CHECK` | `0`, `1` | Enable runtime bounds checking |
| `KRAKEN_LEAK_CHECK` | `0`, `1` | Enable leak detection at exit |
