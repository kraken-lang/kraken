// Runtime union tag checking and validation
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Union tag structure for runtime type checking
typedef struct {
    void* data;
    int tag;
    size_t size;
} kraken_tagged_union_t;

// Create a tagged union
kraken_tagged_union_t* kraken_union_create(size_t size, int initial_tag) {
    kraken_tagged_union_t* u = (kraken_tagged_union_t*)malloc(sizeof(kraken_tagged_union_t));
    if (u == NULL) {
        fprintf(stderr, "Union Error: Failed to allocate tagged union\n");
        abort();
    }
    u->data = malloc(size);
    if (u->data == NULL) {
        fprintf(stderr, "Union Error: Failed to allocate union data\n");
        free(u);
        abort();
    }
    u->tag = initial_tag;
    u->size = size;
    return u;
}

// Set union tag when field is assigned
void kraken_union_set_tag(kraken_tagged_union_t* u, int tag) {
    if (u == NULL) {
        fprintf(stderr, "Union Error: Null union pointer\n");
        abort();
    }
    u->tag = tag;
}

// Check union tag before field access
int kraken_union_check_tag(kraken_tagged_union_t* u, int expected_tag, const char* field_name) {
    if (u == NULL) {
        fprintf(stderr, "Union Error: Null union pointer when accessing %s\n", field_name);
        abort();
    }
    if (u->tag != expected_tag) {
        fprintf(stderr, "Union Error: Tag mismatch accessing %s (expected: %d, actual: %d)\n",
                field_name, expected_tag, u->tag);
        abort();
    }
    return 1;
}

// Get current union tag
int kraken_union_get_tag(kraken_tagged_union_t* u) {
    if (u == NULL) {
        fprintf(stderr, "Union Error: Null union pointer\n");
        abort();
    }
    return u->tag;
}

// Free tagged union
void kraken_union_free(kraken_tagged_union_t* u) {
    if (u != NULL) {
        if (u->data != NULL) {
            free(u->data);
        }
        free(u);
    }
}

// Get union data pointer
void* kraken_union_get_data(kraken_tagged_union_t* u) {
    if (u == NULL) {
        fprintf(stderr, "Union Error: Null union pointer\n");
        abort();
    }
    return u->data;
}

// Validate union size
int kraken_union_validate_size(kraken_tagged_union_t* u, size_t expected_size) {
    if (u == NULL) {
        fprintf(stderr, "Union Error: Null union pointer\n");
        abort();
    }
    if (u->size != expected_size) {
        fprintf(stderr, "Union Error: Size mismatch (expected: %zu, actual: %zu)\n",
                expected_size, u->size);
        abort();
    }
    return 1;
}
