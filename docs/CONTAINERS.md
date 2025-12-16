# Kraken Containers v1 — Semantics Reference

This document defines the semantics for Kraken's v1 container types.

## Overview

Kraken 0.8.5 introduces concrete container types as compiler intrinsics:

| Type | Description |
|------|-------------|
| `VecInt` | Dynamic array of `int` values |
| `VecString` | Dynamic array of `string` values |
| `VecBytes` | Dynamic array of `bytes` values |
| `MapStringInt` | Hash map with `string` keys and `int` values |
| `MapStringString` | Hash map with `string` keys and `string` values |

Generic containers (`Vec<T>`, `Map<K,V>`) are deferred to 0.9.x when generics are implemented.

---

## Type Strategy

**Concrete types only.** Each container type is a distinct type with its own set of intrinsic operations. There is no shared interface or trait system in v1.

---

## Error Semantics

**v1 Implementation:** For simplicity and performance, v1 does **not** include bounds checking. Out-of-bounds access and pop-on-empty are **undefined behavior** (like C arrays). Trap semantics are planned for 0.8.6.

| Operation | v1 Behavior | Planned (0.8.6) |
|-----------|-------------|------------------|
| `vec_*_get(v, i)` | UB if OOB | trap if `i < 0` or `i >= len` |
| `vec_*_set(v, i, val)` | UB if OOB | trap if `i < 0` or `i >= len` |
| `vec_*_pop(v)` | UB if empty | trap if `len == 0` |
| `map_*_get(m, key)` | stub (returns 0) | trap if key not found |

**Rationale:** Trap semantics provide fail-fast behavior during development. Optional/Result-based APIs are deferred until Kraken has a proper error-handling story.

---

## Ownership Model

**Containers own their backing storage.**

- Memory is allocated via `malloc` when the container is created or grows.
- The caller is responsible for freeing the container with `vec_*_free()` or `map_*_free()`.
- Use `defer` for automatic cleanup:
  ```kraken
  let v = vec_int_new();
  defer vec_int_free(v);
  ```

**Pointer validity:**
- Pointers obtained via `data()` are valid only while the container is not mutated.
- Any push/pop/set operation may invalidate previously obtained pointers.

---

## Iteration Strategy

**v1 uses index-based iteration.**

```kraken
let v = vec_int_new();
vec_int_push(v, 10);
vec_int_push(v, 20);
vec_int_push(v, 30);

let i = 0;
while i < vec_int_len(v) {
    let val = vec_int_get(v, i);
    // use val
    i = i + 1;
}
```

Iterator protocol and `for x in container` syntax are deferred until IR/traits are ready.

---

## Clone Semantics

**Containers are NOT trivially copyable.**

- Assignment copies the container handle (pointer), not the contents.
- For deep copy, use explicit `clone()` (deferred to 0.9.x).
- Sharing a container handle between multiple owners requires manual lifetime management.

---

## Vec Semantics

### Runtime Representation

```
VecInt:    { ptr: *mut i64,     len: usize, cap: usize }  // 24 bytes
VecString: { ptr: *mut *mut i8, len: usize, cap: usize }  // 24 bytes (array of string pointers)
VecBytes:  { ptr: *mut i8,      len: usize, cap: usize }  // 24 bytes (byte array)
```

### Growth Policy

- Initial capacity: 4 elements
- Growth factor: 2x when capacity is exceeded
- Shrink policy: none (capacity never decreases automatically)

### Operations

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `new` | `() -> Vec*` | Create empty vector |
| `push` | `(v, val) -> void` | Append element (may grow) |
| `pop` | `(v) -> T` | Remove and return last element (traps if empty) |
| `len` | `(v) -> int` | Return number of elements |
| `get` | `(v, i) -> T` | Return element at index (traps if OOB) |
| `set` | `(v, i, val) -> void` | Set element at index (traps if OOB) |
| `clear` | `(v) -> void` | Remove all elements (capacity unchanged) |
| `free` | `(v) -> void` | Free backing storage |

### Deferred Operations (0.9.x)

- `with_capacity(n)` — preallocate capacity
- `capacity()` — return current capacity
- `reserve(n)` — ensure capacity for n more elements
- `shrink_to_fit()` — reduce capacity to len
- `insert(i, val)` — insert at index
- `remove(i)` — remove at index
- `swap_remove(i)` — O(1) remove by swapping with last
- `clone()` — deep copy
- `data()` — raw pointer access

---

## Map Semantics

### Runtime Representation

```
MapStringInt:    { keys: *mut *mut i8, values: *mut i64,     len: usize, cap: usize }  // 32 bytes
MapStringString: { keys: *mut *mut i8, values: *mut *mut i8, len: usize, cap: usize }  // 32 bytes
```

**v1 Note:** The v1 implementation uses parallel arrays for keys and values. Hash-based lookup is deferred; v1 uses linear search.

### Hash Function

**FNV-1a** (Fowler–Noll–Vo) hash function.

- Simple, fast, good distribution for general use.
- DoS-resistant hashing (SipHash) deferred to 0.9.x.
- Seed: fixed (deterministic within process).

### Collision Strategy

**Open addressing with linear probing.**

- On collision, probe sequentially until an empty slot is found.
- Tombstones allow deletion without breaking probe chains.

### Load Factor

- Maximum load factor: 0.75
- Resize threshold: when `len / cap > 0.75`
- Resize factor: 2x
- Initial capacity: 8 slots

### Iteration Order

**Unspecified.** Do not rely on iteration order. It may change between operations, resizes, or Kraken versions.

### Operations

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `new` | `() -> Map*` | Create empty map |
| `set` | `(m, key, val) -> void` | Insert or update key-value pair |
| `get` | `(m, key) -> T` | Return value for key (traps if not found) |
| `has` | `(m, key) -> int` | Check if key exists (returns 0 or 1) |
| `remove` | `(m, key) -> void` | Remove key-value pair |
| `len` | `(m) -> int` | Return number of entries |
| `clear` | `(m) -> void` | Remove all entries |
| `free` | `(m) -> void` | Free backing storage |

### Deferred Operations (0.9.x)

- `capacity()` — return current capacity
- `reserve(n)` — ensure capacity
- `keys()` — return array/slice of keys
- `values()` — return array/slice of values
- `clone()` — deep copy

---

## FFI Interop

### Vec FFI

```kraken
// Get raw pointer to backing array (read-only)
let ptr = vec_int_data(v);  // deferred to 0.9.x
```

**Validity:** Pointer is valid until next mutation. Do not store across push/pop/set calls.

### Map FFI

No direct FFI interop for maps in v1. Use iteration to extract data.

---

## Deferred Features (0.9.x+)

| Feature | Target |
|---------|--------|
| Generic containers (`Vec<T>`, `Map<K,V>`) | 0.9.x |
| Capacity management APIs | 0.9.x |
| Advanced mutation (insert/remove/swap_remove) | 0.9.x |
| Deep clone (`clone()`) | 0.9.x |
| Iteration APIs (`keys()`, `values()`) | 0.8.8 |
| DoS-resistant hashing (SipHash) | 0.9.x |
| Equality operators (`==`/`!=`) | 0.9.x |
| Debug/fmt output | 0.9.x |

---

## Examples

### VecInt Usage

```kraken
fn main() -> int {
    let v = vec_int_new();
    defer vec_int_free(v);
    
    vec_int_push(v, 10);
    vec_int_push(v, 20);
    vec_int_push(v, 30);
    
    let sum = 0;
    let i = 0;
    while i < vec_int_len(v) {
        sum = sum + vec_int_get(v, i);
        i = i + 1;
    }
    
    return sum;  // 60
}
```

### MapStringInt Usage

**v1 Note:** `set`, `get`, `has`, `delete` are stubs in v1 (return 0). Full implementation requires loop codegen fixes, planned for 0.8.6.

```kraken
// v1: Only new/len/clear/free are functional
fn main() -> int {
    let m = map_string_int_new();
    let len = map_string_int_len(m);  // 0
    map_string_int_free(m);
    return len;
}

// Planned for 0.8.6:
// fn main() -> int {
//     let m = map_string_int_new();
//     defer map_string_int_free(m);
//     map_string_int_set(m, "one", 1);
//     map_string_int_set(m, "two", 2);
//     if (map_string_int_has(m, "two") == 1) {
//         return map_string_int_get(m, "two");  // 2
//     }
//     return 0;
// }
```
