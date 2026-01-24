// FFI safety utilities for null pointer checking and validation
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Null pointer check with trap
void* kraken_ffi_check_null(void* ptr, const char* func_name) {
    if (ptr == NULL) {
        fprintf(stderr, "FFI Error: Null pointer in %s\n", func_name);
        abort();
    }
    return ptr;
}

// Validate pointer is non-null before FFI call
void* kraken_ffi_validate_ptr(void* ptr, const char* operation) {
    if (ptr == NULL) {
        fprintf(stderr, "FFI Error: Null pointer for %s\n", operation);
        abort();
    }
    return ptr;
}

// Check buffer bounds for FFI operations
int kraken_ffi_check_bounds(size_t index, size_t size, const char* operation) {
    if (index >= size) {
        fprintf(stderr, "FFI Error: Index %zu out of bounds (size: %zu) in %s\n", 
                index, size, operation);
        abort();
    }
    return 1;
}

// Validate string pointer for FFI
const char* kraken_ffi_validate_string(const char* str, const char* func_name) {
    if (str == NULL) {
        fprintf(stderr, "FFI Error: Null string in %s\n", func_name);
        abort();
    }
    return str;
}

// Safe malloc with null check
void* kraken_ffi_safe_malloc(size_t size) {
    void* ptr = malloc(size);
    if (ptr == NULL && size > 0) {
        fprintf(stderr, "FFI Error: malloc failed for size %zu\n", size);
        abort();
    }
    return ptr;
}

// Safe realloc with null check
void* kraken_ffi_safe_realloc(void* ptr, size_t size) {
    void* new_ptr = realloc(ptr, size);
    if (new_ptr == NULL && size > 0) {
        fprintf(stderr, "FFI Error: realloc failed for size %zu\n", size);
        abort();
    }
    return new_ptr;
}

// Type validation for FFI boundaries
int kraken_ffi_validate_type_size(size_t actual, size_t expected, const char* type_name) {
    if (actual != expected) {
        fprintf(stderr, "FFI Error: Type size mismatch for %s (actual: %zu, expected: %zu)\n",
                type_name, actual, expected);
        abort();
    }
    return 1;
}

// Callback validation
typedef void (*kraken_callback_t)(void);

kraken_callback_t kraken_ffi_validate_callback(kraken_callback_t callback, const char* name) {
    if (callback == NULL) {
        fprintf(stderr, "FFI Error: Null callback for %s\n", name);
        abort();
    }
    return callback;
}
