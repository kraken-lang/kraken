#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// VecString structure (matches Kraken's layout)
typedef struct {
    char** data;
    int64_t len;
    int64_t cap;
} VecString;

// Create a new VecString
VecString* vec_string_new_runtime() {
    VecString* vec = (VecString*)malloc(sizeof(VecString));
    vec->cap = 4;
    vec->len = 0;
    vec->data = (char**)malloc(vec->cap * sizeof(char*));
    return vec;
}

// Push a string to VecString
void vec_string_push_runtime(VecString* vec, const char* str) {
    if (vec->len >= vec->cap) {
        vec->cap *= 2;
        vec->data = (char**)realloc(vec->data, vec->cap * sizeof(char*));
    }
    vec->data[vec->len++] = (char*)str;
}

// str_split implementation
VecString* kraken_str_split(const char* s, const char* delim) {
    if (!s || !delim) {
        return NULL;
    }

    VecString* result = vec_string_new_runtime();
    if (!result) {
        return NULL;
    }
    
    size_t delim_len = strlen(delim);
    
    // Empty delimiter: return vector with whole string
    if (delim_len == 0) {
        size_t s_len = strlen(s);
        char* copy = (char*)malloc(s_len + 1);
        if (!copy) return NULL;
        memcpy(copy, s, s_len);
        copy[s_len] = '\0';
        vec_string_push_runtime(result, copy);
        return result;
    }
    
    const char* start = s;
    const char* found;
    
    while ((found = strstr(start, delim)) != NULL) {
        // Calculate length of part before delimiter
        size_t part_len = found - start;
        
        // Allocate and copy the part
        char* part = (char*)malloc(part_len + 1);
        if (!part) return NULL;
        memcpy(part, start, part_len);
        part[part_len] = '\0';
        
        vec_string_push_runtime(result, part);
        
        // Move past the delimiter
        start = found + delim_len;
    }
    
    // Add the final part (everything after last delimiter)
    size_t final_len = strlen(start);
    char* final_part = (char*)malloc(final_len + 1);
    if (!final_part) return NULL;
    memcpy(final_part, start, final_len);
    final_part[final_len] = '\0';
    vec_string_push_runtime(result, final_part);
    
    return result;
}

// str_join implementation
char* kraken_str_join(VecString* vec, const char* sep) {
    if (!vec || !sep) {
        return NULL;
    }
    
    // Empty vector: return empty string
    if (vec->len == 0) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    // Calculate total size needed
    size_t total_size = 0;
    size_t sep_len = strlen(sep);
    
    for (int64_t i = 0; i < vec->len; i++) {
        total_size += strlen(vec->data[i]);
        if (i < vec->len - 1) {
            total_size += sep_len;
        }
    }
    
    // Allocate result buffer
    char* result = (char*)malloc(total_size + 1);
    result[0] = '\0';
    
    // Build result string
    for (int64_t i = 0; i < vec->len; i++) {
        strcat(result, vec->data[i]);
        if (i < vec->len - 1) {
            strcat(result, sep);
        }
    }
    
    return result;
}
