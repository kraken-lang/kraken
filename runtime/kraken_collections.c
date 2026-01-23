#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// ============================================================================
// VEC HELPER FUNCTIONS
// ============================================================================

// Vec map helper - applies function to each element
// Note: In Kraken, this will be called with a closure pointer
void* kraken_vec_map(void* vec, void* (*fn)(void*), size_t elem_size) {
    // This is a C helper that will be wrapped by Kraken closures
    // Implementation would iterate and apply function
    return vec;
}

// Vec filter helper - keeps elements matching predicate
void* kraken_vec_filter(void* vec, int (*predicate)(void*), size_t elem_size) {
    // This is a C helper that will be wrapped by Kraken closures
    return vec;
}

// Vec fold helper - reduces to single value
void* kraken_vec_fold(void* vec, void* init, void* (*fn)(void*, void*), size_t elem_size) {
    // This is a C helper that will be wrapped by Kraken closures
    return init;
}

// Vec for_each helper - applies function for side effects
void kraken_vec_for_each(void* vec, void (*fn)(void*), size_t elem_size) {
    // This is a C helper that will be wrapped by Kraken closures
}

// ============================================================================
// OPTION HELPER FUNCTIONS
// ============================================================================

// Option map helper
void* kraken_option_map(void* option, void* (*fn)(void*)) {
    // This is a C helper that will be wrapped by Kraken closures
    return option;
}

// Option and_then helper
void* kraken_option_and_then(void* option, void* (*fn)(void*)) {
    // This is a C helper that will be wrapped by Kraken closures
    return option;
}

// Option or_else helper
void* kraken_option_or_else(void* option, void* (*fn)(void)) {
    // This is a C helper that will be wrapped by Kraken closures
    return option;
}

// Option filter helper
void* kraken_option_filter(void* option, int (*predicate)(void*)) {
    // This is a C helper that will be wrapped by Kraken closures
    return option;
}

// ============================================================================
// RESULT HELPER FUNCTIONS
// ============================================================================

// Result map helper
void* kraken_result_map(void* result, void* (*fn)(void*)) {
    // This is a C helper that will be wrapped by Kraken closures
    return result;
}

// Result map_err helper
void* kraken_result_map_err(void* result, void* (*fn)(void*)) {
    // This is a C helper that will be wrapped by Kraken closures
    return result;
}

// Result and_then helper
void* kraken_result_and_then(void* result, void* (*fn)(void*)) {
    // This is a C helper that will be wrapped by Kraken closures
    return result;
}

// Result or_else helper
void* kraken_result_or_else(void* result, void* (*fn)(void*)) {
    // This is a C helper that will be wrapped by Kraken closures
    return result;
}

// ============================================================================
// CONVERSION HELPER FUNCTIONS
// ============================================================================

// Generic conversion helper
void* kraken_convert(void* value, void* (*converter)(void*)) {
    if (converter == NULL) {
        return NULL;
    }
    return converter(value);
}

// Try conversion helper (returns 0 on success, 1 on failure)
int kraken_try_convert(void* value, void* (*converter)(void*, int*), void** result) {
    int error = 0;
    if (converter == NULL) {
        return 1;
    }
    *result = converter(value, &error);
    return error;
}

// ============================================================================
// ITERATOR HELPER FUNCTIONS
// ============================================================================

// Iterator next helper
void* kraken_iterator_next(void* iterator, void* (*next_fn)(void*)) {
    if (next_fn == NULL) {
        return NULL;
    }
    return next_fn(iterator);
}

// Iterator collect helper
void* kraken_iterator_collect(void* iterator, void* (*next_fn)(void*), void* (*collector)(void*, void*), void* init) {
    if (next_fn == NULL || collector == NULL) {
        return init;
    }
    
    void* result = init;
    void* item;
    
    while ((item = next_fn(iterator)) != NULL) {
        result = collector(result, item);
    }
    
    return result;
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

// Clone helper (shallow copy)
void* kraken_clone(void* value, size_t size) {
    if (value == NULL || size == 0) {
        return NULL;
    }
    
    void* copy = malloc(size);
    if (copy == NULL) {
        return NULL;
    }
    
    memcpy(copy, value, size);
    return copy;
}

// Compare helper (returns -1, 0, or 1)
int kraken_compare(void* a, void* b, int (*cmp_fn)(void*, void*)) {
    if (cmp_fn == NULL) {
        return 0;
    }
    return cmp_fn(a, b);
}

// Hash helper
uint64_t kraken_hash(void* value, uint64_t (*hash_fn)(void*)) {
    if (hash_fn == NULL) {
        return 0;
    }
    return hash_fn(value);
}

// Default value helper
void* kraken_default(void* (*default_fn)(void)) {
    if (default_fn == NULL) {
        return NULL;
    }
    return default_fn();
}

// ============================================================================
// RANGE HELPER FUNCTIONS
// ============================================================================

// Range iterator state
typedef struct {
    int64_t current;
    int64_t end;
    int64_t step;
} RangeIterator;

// Create range iterator
RangeIterator* kraken_range_iter(int64_t start, int64_t end, int64_t step) {
    RangeIterator* iter = (RangeIterator*)malloc(sizeof(RangeIterator));
    if (iter == NULL) {
        return NULL;
    }
    
    iter->current = start;
    iter->end = end;
    iter->step = step;
    
    return iter;
}

// Range iterator next
int kraken_range_next(RangeIterator* iter, int64_t* value) {
    if (iter == NULL || value == NULL) {
        return 0;
    }
    
    if ((iter->step > 0 && iter->current >= iter->end) ||
        (iter->step < 0 && iter->current <= iter->end)) {
        return 0;  // End of iteration
    }
    
    *value = iter->current;
    iter->current += iter->step;
    return 1;  // Has value
}

// Free range iterator
void kraken_range_free(RangeIterator* iter) {
    if (iter != NULL) {
        free(iter);
    }
}

// ============================================================================
// ARRAY HELPER FUNCTIONS
// ============================================================================

// Array map (creates new array)
void* kraken_array_map(void* arr, size_t len, size_t elem_size, void* (*fn)(void*)) {
    if (arr == NULL || fn == NULL || len == 0 || elem_size == 0) {
        return NULL;
    }
    
    void* result = malloc(len * elem_size);
    if (result == NULL) {
        return NULL;
    }
    
    for (size_t i = 0; i < len; i++) {
        void* elem = (char*)arr + (i * elem_size);
        void* mapped = fn(elem);
        if (mapped != NULL) {
            memcpy((char*)result + (i * elem_size), mapped, elem_size);
        }
    }
    
    return result;
}

// Array filter (creates new array, returns new length)
void* kraken_array_filter(void* arr, size_t len, size_t elem_size, int (*predicate)(void*), size_t* new_len) {
    if (arr == NULL || predicate == NULL || len == 0 || elem_size == 0 || new_len == NULL) {
        *new_len = 0;
        return NULL;
    }
    
    // First pass: count matching elements
    size_t count = 0;
    for (size_t i = 0; i < len; i++) {
        void* elem = (char*)arr + (i * elem_size);
        if (predicate(elem)) {
            count++;
        }
    }
    
    if (count == 0) {
        *new_len = 0;
        return NULL;
    }
    
    // Second pass: copy matching elements
    void* result = malloc(count * elem_size);
    if (result == NULL) {
        *new_len = 0;
        return NULL;
    }
    
    size_t j = 0;
    for (size_t i = 0; i < len; i++) {
        void* elem = (char*)arr + (i * elem_size);
        if (predicate(elem)) {
            memcpy((char*)result + (j * elem_size), elem, elem_size);
            j++;
        }
    }
    
    *new_len = count;
    return result;
}

// Array fold
void* kraken_array_fold(void* arr, size_t len, size_t elem_size, void* init, void* (*fn)(void*, void*)) {
    if (arr == NULL || fn == NULL || len == 0 || elem_size == 0) {
        return init;
    }
    
    void* accumulator = init;
    
    for (size_t i = 0; i < len; i++) {
        void* elem = (char*)arr + (i * elem_size);
        accumulator = fn(accumulator, elem);
    }
    
    return accumulator;
}

// Array any (returns 1 if any element matches)
int kraken_array_any(void* arr, size_t len, size_t elem_size, int (*predicate)(void*)) {
    if (arr == NULL || predicate == NULL || len == 0 || elem_size == 0) {
        return 0;
    }
    
    for (size_t i = 0; i < len; i++) {
        void* elem = (char*)arr + (i * elem_size);
        if (predicate(elem)) {
            return 1;
        }
    }
    
    return 0;
}

// Array all (returns 1 if all elements match)
int kraken_array_all(void* arr, size_t len, size_t elem_size, int (*predicate)(void*)) {
    if (arr == NULL || predicate == NULL || len == 0 || elem_size == 0) {
        return 0;
    }
    
    for (size_t i = 0; i < len; i++) {
        void* elem = (char*)arr + (i * elem_size);
        if (!predicate(elem)) {
            return 0;
        }
    }
    
    return 1;
}

// Array find (returns index or -1)
int64_t kraken_array_find(void* arr, size_t len, size_t elem_size, int (*predicate)(void*)) {
    if (arr == NULL || predicate == NULL || len == 0 || elem_size == 0) {
        return -1;
    }
    
    for (size_t i = 0; i < len; i++) {
        void* elem = (char*)arr + (i * elem_size);
        if (predicate(elem)) {
            return (int64_t)i;
        }
    }
    
    return -1;
}
