<div align="center">
    <img width="auto" height="118" alt="Iron Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1>Kraken Language</h1>
</div>


## FFI / C Boundary (Compiler-Enforced)

This section specifies the rules for calling C/stdlib APIs from Kraken via the compiler’s built-in stdlib/FFI surface.
The authoritative source of truth for per-function ABI metadata is `compiler/src/ffi/stdlib.rs`.

### Canonical ABI types (current)

- **Kraken `string`**
  - Modeled as an `i8*` at the C boundary.
  - Used for C `char*` and also as an opaque pointer carrier for `void*`/`FILE*` in the current implementation.
- **Kraken `int`**
  - Modeled as `i64` internally.
  - When calling C APIs, the compiler inserts explicit casts per stdlib signature to match the declared C ABI (`i32`/`i64`).

### Ownership

- **Borrowed**
  - Pointer is owned by the callee (C) or some other system component.
  - Kraken must not free it.
- **Owned**
  - Pointer ownership is transferred to Kraken.
  - Kraken is responsible for eventually releasing it with the appropriate API (e.g. `free`).

### Nullability + trap policy

- **Owned, fallible pointer returns**
  - If an FFI call is declared as returning an **owned pointer** and can fail by returning null (`errno: ReturnsNull`), Kraken enforces a **fail-fast trap**: the program aborts immediately when the call returns null.
  - This applies to APIs such as `malloc`, `realloc`, and `fopen`.

- **Borrowed pointer returns**
  - Some C APIs may return borrowed null on “not found”/“not available” conditions (e.g. `getenv`, `fgets`).
  - These are modeled as `Nullable` + `Borrowed` in the stdlib table.
  - Kraken currently does not have a dedicated nullable-pointer type; programs must treat such APIs as semantically fallible and avoid dereferencing/consuming a null result.

### Errno conventions

The stdlib signature table records a per-function error convention:

- `None`: no special error encoding.
- `ReturnsNegOne`: failure is indicated by returning `-1` (C `int`).
- `ReturnsNull`: failure is indicated by returning `NULL`.

### C `int` widening policy

- For stdlib functions that return a C `int` (`i32`) but are represented as Kraken `int` (`i64`), the compiler must widen the value.
- **Default policy:** signed widening (`sext i32 -> i64`).
- **Unsigned widening** is reserved for explicit per-function overrides when introduced.
- The compiler enforces that any stdlib signature with `c_abi_return = I32` and `kraken_return = Int` must declare `c_int_widening` explicitly.
