<h1 align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
        <br><sub><sup>KRAKEN LANGUAGE</sup></sub><br>
        STRINGS
</h1>

This guide provides a comprehensive overview of string handling in the Kraken programming language, including string types, operations, memory management, and best practices.

## Table of Contents

1. [String Types](#string-types)
2. [String Creation](#string-creation)
3. [String Operations](#string-operations)
4. [Memory Management](#memory-management)
5. [String Safety](#string-safety)
6. [Best Practices](#best-practices)
7. [Runtime Functions](#runtime-functions)

## String Types

Kraken provides two primary string types:

### `str` - String Slice (Borrowed)

A `str` is a borrowed reference to a string. It's a view into string data without owning it.

```kraken
fn greet(name: str) -> void {
    puts(name);
}
```

**Characteristics:**
- Immutable by default
- Does not own the underlying data
- Lightweight (pointer + length)
- Cannot be resized
- Ideal for function parameters

### `String` - Owned String

A `String` is an owned, heap-allocated string that can be modified.

```kraken
fn build_message() -> String {
    let mut msg = String::new();
    msg.push_str("Hello");
    msg.push_str(", World!");
    return msg;
}
```

**Characteristics:**
- Owns its data
- Can be modified (if mutable)
- Heap-allocated
- Automatically freed when out of scope
- Can grow dynamically

## String Creation

### Literal Strings

```kraken
let s1 = "Hello, World!";           // str literal
let s2 = "Multi-line\nstring";      // with escape sequences
let s3 = "Unicode: 🦀";             // UTF-8 support
```

### Empty Strings

```kraken
let empty = "";
let empty_string = String::new();
```

### From C Strings

```kraken
extern fn from_cstr(cstr: int) -> str;

let c_string = /* ... */;
let kraken_str = from_cstr(c_string);
```

## String Operations

### Concatenation

```kraken
extern fn kraken_str_concat(s1: str, s2: str) -> str;

let greeting = kraken_str_concat("Hello, ", "World!");
puts(greeting);
```

### Substring Extraction

```kraken
extern fn kraken_str_substring(s: str, start: int, end: int) -> str;

let text = "Hello, World!";
let hello = kraken_str_substring(text, 0, 5);  // "Hello"
puts(hello);
```

### String Length

```kraken
extern fn kraken_str_len(s: str) -> int;

let text = "Hello";
let len = kraken_str_len(text);  // 5
```

### String Searching

```kraken
extern fn kraken_str_contains(s: str, substr: str) -> int;
extern fn kraken_str_starts_with(s: str, prefix: str) -> int;
extern fn kraken_str_ends_with(s: str, suffix: str) -> int;

let text = "Hello, World!";

if (kraken_str_contains(text, "World") == 1) {
    puts("Found 'World'");
}

if (kraken_str_starts_with(text, "Hello") == 1) {
    puts("Starts with 'Hello'");
}

if (kraken_str_ends_with(text, "!") == 1) {
    puts("Ends with '!'");
}
```

### Case Conversion

```kraken
extern fn kraken_str_to_upper(s: str) -> str;
extern fn kraken_str_to_lower(s: str) -> str;

let text = "Hello, World!";
let upper = kraken_str_to_upper(text);  // "HELLO, WORLD!"
let lower = kraken_str_to_lower(text);  // "hello, world!"
```

### Trimming Whitespace

```kraken
extern fn kraken_str_trim(s: str) -> str;

let text = "  Hello, World!  ";
let trimmed = kraken_str_trim(text);  // "Hello, World!"
```

### String Replacement

```kraken
extern fn kraken_str_replace(s: str, old: str, new: str) -> str;

let text = "Hello, World!";
let replaced = kraken_str_replace(text, "World", "Kraken");
puts(replaced);  // "Hello, Kraken!"
```

### String Splitting

```kraken
extern fn kraken_str_split(s: str, delim: str) -> VecString;

let text = "apple,banana,cherry";
let parts = kraken_str_split(text, ",");
// parts[0] = "apple"
// parts[1] = "banana"
// parts[2] = "cherry"
```

### String Joining

```kraken
extern fn kraken_str_join(vec: VecString, sep: str) -> str;

let parts = /* ... */;
let joined = kraken_str_join(parts, ", ");
puts(joined);  // "apple, banana, cherry"
```

## Memory Management

### Automatic Memory Management

Kraken uses ownership-based memory management for strings:

```kraken
fn example() -> void {
    let s = "Hello";  // String literal (static)
    
    let owned = kraken_str_concat(s, ", World!");  // Heap-allocated
    puts(owned);
    
    // 'owned' is automatically freed when function returns
}
```

### Manual Memory Management (Advanced)

For low-level control, you can use manual memory management:

```kraken
extern fn malloc(size: int) -> int;
extern fn free(ptr: int) -> void;

fn manual_string() -> void {
    let size = 100;
    let buffer = malloc(size);
    
    // Use buffer...
    
    free(buffer);  // Must manually free
}
```

### Memory Leak Detection

Enable leak detection to track allocations:

```bash
export KRAKEN_LEAK_CHECK=1
./my_program
```

This will report any leaked allocations at program exit.

## String Safety

### Bounds Checking

Enable runtime bounds checking for string indexing:

```bash
export KRAKEN_BOUNDS_CHECK=1
./my_program
```

This will trap on out-of-bounds access with clear error messages:

```
===========================================
KRAKEN RUNTIME ERROR: Index Out of Bounds
===========================================
Index:    10
Length:   5
Location: example.kr:42
===========================================
```

### Null Safety

Always check for null pointers when working with C strings:

```kraken
extern fn kraken_null_check(ptr: int, file: str, line: int) -> void;

fn safe_string_op(ptr: int) -> void {
    kraken_null_check(ptr, "example.kr", 10);
    // Safe to use ptr here
}
```

### UTF-8 Safety

Kraken strings are UTF-8 encoded. Be careful when:
- Indexing by byte position (not character position)
- Slicing strings (may split multi-byte characters)

```kraken
let emoji = "🦀";
let len = kraken_str_len(emoji);  // 4 bytes, not 1 character!
```

## Best Practices

### 1. Use `str` for Function Parameters

```kraken
// Good: accepts both str and String
fn process(text: str) -> void {
    puts(text);
}

// Less flexible: only accepts String
fn process_owned(text: String) -> void {
    puts(text);
}
```

### 2. Return Owned Strings from Functions

```kraken
// Good: caller owns the result
fn build_greeting(name: str) -> str {
    return kraken_str_concat("Hello, ", name);
}
```

### 3. Avoid Unnecessary Allocations

```kraken
// Good: reuse existing string
let text = "Hello, World!";
if (kraken_str_contains(text, "World") == 1) {
    puts(text);
}

// Bad: unnecessary allocation
let text = "Hello, World!";
let copy = kraken_str_concat(text, "");  // Wasteful!
```

### 4. Use String Literals for Constants

```kraken
// Good: compile-time constant
const GREETING = "Hello, World!";

// Less efficient: runtime allocation
let greeting = kraken_str_concat("Hello", ", World!");
```

### 5. Enable Safety Checks During Development

```bash
# Development
export KRAKEN_BOUNDS_CHECK=1
export KRAKEN_LEAK_CHECK=1
./my_program

# Production (disable for performance)
unset KRAKEN_BOUNDS_CHECK
unset KRAKEN_LEAK_CHECK
./my_program
```

## Runtime Functions

### String Utilities

| Function | Description | Returns |
|----------|-------------|---------|
| `kraken_str_len(s)` | Get string length | `int` |
| `kraken_str_concat(s1, s2)` | Concatenate strings | `str` |
| `kraken_str_substring(s, start, end)` | Extract substring | `str` |
| `kraken_str_contains(s, substr)` | Check if contains substring | `int` (0/1) |
| `kraken_str_starts_with(s, prefix)` | Check if starts with prefix | `int` (0/1) |
| `kraken_str_ends_with(s, suffix)` | Check if ends with suffix | `int` (0/1) |
| `kraken_str_to_upper(s)` | Convert to uppercase | `str` |
| `kraken_str_to_lower(s)` | Convert to lowercase | `str` |
| `kraken_str_trim(s)` | Trim whitespace | `str` |
| `kraken_str_replace(s, old, new)` | Replace substring | `str` |
| `kraken_str_split(s, delim)` | Split into vector | `VecString` |
| `kraken_str_join(vec, sep)` | Join vector | `str` |

### Safety Functions

| Function | Description |
|----------|-------------|
| `kraken_bounds_check(index, length, file, line)` | Check array bounds |
| `kraken_bounds_check_range(start, end, length, file, line)` | Check range bounds |
| `kraken_null_check(ptr, file, line)` | Check for null pointer |

### Memory Tracking

| Function | Description | Returns |
|----------|-------------|---------|
| `kraken_malloc_tracked(size, file, line)` | Allocate with tracking | `int` (ptr) |
| `kraken_calloc_tracked(count, size, file, line)` | Allocate zeroed with tracking | `int` (ptr) |
| `kraken_realloc_tracked(ptr, size, file, line)` | Reallocate with tracking | `int` (ptr) |
| `kraken_free_tracked(ptr)` | Free with tracking | `void` |
| `kraken_get_allocation_count()` | Get active allocation count | `int` |
| `kraken_get_allocated_bytes()` | Get total allocated bytes | `int` |
| `kraken_print_allocation_stats()` | Print allocation statistics | `void` |

## Examples

See the following example files:
- `examples/string_processing.kr` - String manipulation examples
- `examples/safe_pointers.kr` - Safe pointer usage patterns
- `tests/programs/test_string_utilities.kr` - String utility tests
- `tests/programs/test_bounds_checking.kr` - Bounds checking tests
- `tests/programs/test_leak_detection.kr` - Memory leak detection tests

## Further Reading

- [Memory Safety Guide](MEMORY_SAFETY.md)
- [C FFI Guide](FFI.md)
- [Language Specification](SPEC.md)
