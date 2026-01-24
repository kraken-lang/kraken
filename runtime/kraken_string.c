#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>

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

// String length (safe wrapper)
int64_t kraken_str_len(const char* s) {
    if (!s) return 0;
    return (int64_t)strlen(s);
}

// String concatenation
char* kraken_str_concat(const char* s1, const char* s2) {
    if (!s1 || !s2) return NULL;
    
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char* result = (char*)malloc(len1 + len2 + 1);
    if (!result) return NULL;
    
    memcpy(result, s1, len1);
    memcpy(result + len1, s2, len2);
    result[len1 + len2] = '\0';
    
    return result;
}

// String substring
char* kraken_str_substring(const char* s, int64_t start, int64_t end) {
    if (!s) return NULL;
    
    int64_t len = (int64_t)strlen(s);
    if (start < 0) start = 0;
    if (end > len) end = len;
    if (start >= end) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    int64_t substr_len = end - start;
    char* result = (char*)malloc(substr_len + 1);
    if (!result) return NULL;
    
    memcpy(result, s + start, substr_len);
    result[substr_len] = '\0';
    
    return result;
}

// String contains
int64_t kraken_str_contains(const char* s, const char* substr) {
    if (!s || !substr) return 0;
    return strstr(s, substr) != NULL ? 1 : 0;
}

// String starts_with
int64_t kraken_str_starts_with(const char* s, const char* prefix) {
    if (!s || !prefix) return 0;
    
    size_t s_len = strlen(s);
    size_t prefix_len = strlen(prefix);
    
    if (prefix_len > s_len) return 0;
    return memcmp(s, prefix, prefix_len) == 0 ? 1 : 0;
}

// String ends_with
int64_t kraken_str_ends_with(const char* s, const char* suffix) {
    if (!s || !suffix) return 0;
    
    size_t s_len = strlen(s);
    size_t suffix_len = strlen(suffix);
    
    if (suffix_len > s_len) return 0;
    return memcmp(s + s_len - suffix_len, suffix, suffix_len) == 0 ? 1 : 0;
}

// String to uppercase
char* kraken_str_to_upper(const char* s) {
    if (!s) return NULL;
    
    size_t len = strlen(s);
    char* result = (char*)malloc(len + 1);
    if (!result) return NULL;
    
    for (size_t i = 0; i < len; i++) {
        result[i] = (s[i] >= 'a' && s[i] <= 'z') ? s[i] - 32 : s[i];
    }
    result[len] = '\0';
    
    return result;
}

// String to lowercase
char* kraken_str_to_lower(const char* s) {
    if (!s) return NULL;
    
    size_t len = strlen(s);
    char* result = (char*)malloc(len + 1);
    if (!result) return NULL;
    
    for (size_t i = 0; i < len; i++) {
        result[i] = (s[i] >= 'A' && s[i] <= 'Z') ? s[i] + 32 : s[i];
    }
    result[len] = '\0';
    
    return result;
}

// String trim (remove leading/trailing whitespace)
char* kraken_str_trim(const char* s) {
    if (!s) return NULL;
    
    // Find first non-whitespace
    const char* start = s;
    while (*start && (*start == ' ' || *start == '\t' || *start == '\n' || *start == '\r')) {
        start++;
    }
    
    // Find last non-whitespace
    const char* end = s + strlen(s) - 1;
    while (end > start && (*end == ' ' || *end == '\t' || *end == '\n' || *end == '\r')) {
        end--;
    }
    
    size_t len = end - start + 1;
    char* result = (char*)malloc(len + 1);
    if (!result) return NULL;
    
    memcpy(result, start, len);
    result[len] = '\0';
    
    return result;
}

// String replace
char* kraken_str_replace(const char* s, const char* old_str, const char* new_str) {
    if (!s || !old_str || !new_str) return NULL;
    
    size_t old_len = strlen(old_str);
    size_t new_len = strlen(new_str);
    
    if (old_len == 0) {
        size_t s_len = strlen(s);
        char* result = (char*)malloc(s_len + 1);
        if (!result) return NULL;
        memcpy(result, s, s_len + 1);
        return result;
    }
    
    // Count occurrences
    int64_t count = 0;
    const char* tmp = s;
    while ((tmp = strstr(tmp, old_str)) != NULL) {
        count++;
        tmp += old_len;
    }
    
    if (count == 0) {
        size_t s_len = strlen(s);
        char* result = (char*)malloc(s_len + 1);
        if (!result) return NULL;
        memcpy(result, s, s_len + 1);
        return result;
    }
    
    // Allocate result buffer
    size_t result_len = strlen(s) + count * (new_len - old_len);
    char* result = (char*)malloc(result_len + 1);
    if (!result) return NULL;
    
    // Build result
    char* dst = result;
    const char* src = s;
    const char* found;
    
    while ((found = strstr(src, old_str)) != NULL) {
        size_t part_len = found - src;
        memcpy(dst, src, part_len);
        dst += part_len;
        memcpy(dst, new_str, new_len);
        dst += new_len;
        src = found + old_len;
    }
    
    strcpy(dst, src);
    
    return result;
}

// sprintf wrapper - formatted string output
int64_t kraken_sprintf(char* buffer, const char* format, ...) {
    if (!buffer || !format) return -1;
    
    va_list args;
    va_start(args, format);
    int result = vsprintf(buffer, format, args);
    va_end(args);
    
    return (int64_t)result;
}

// snprintf wrapper - bounded formatted string output
int64_t kraken_snprintf(char* buffer, int64_t size, const char* format, ...) {
    if (!buffer || !format || size <= 0) return -1;
    
    va_list args;
    va_start(args, format);
    int result = vsnprintf(buffer, (size_t)size, format, args);
    va_end(args);
    
    return (int64_t)result;
}

// strtok wrapper - string tokenization
char* kraken_strtok(char* str, const char* delim) {
    return strtok(str, delim);
}

// strdup wrapper - string duplication
char* kraken_strdup(const char* s) {
    if (!s) return NULL;
    
    size_t len = strlen(s);
    char* result = (char*)malloc(len + 1);
    if (!result) return NULL;
    
    memcpy(result, s, len + 1);
    return result;
}

// strchr wrapper - find first occurrence of character
char* kraken_strchr(const char* s, int c) {
    if (!s) return NULL;
    return strchr(s, c);
}

// strrchr wrapper - find last occurrence of character
char* kraken_strrchr(const char* s, int c) {
    if (!s) return NULL;
    return strrchr(s, c);
}
