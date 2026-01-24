// Advanced struct padding and alignment calculations
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

// Calculate alignment for a type
size_t kraken_type_alignment(size_t type_size) {
    // Standard alignment rules: align to type size up to pointer size
    if (type_size >= sizeof(void*)) {
        return sizeof(void*);
    }
    // Power of 2 alignment
    if (type_size == 1) return 1;
    if (type_size == 2) return 2;
    if (type_size <= 4) return 4;
    return sizeof(void*);
}

// Calculate padding needed to align offset
size_t kraken_calculate_padding(size_t offset, size_t alignment) {
    size_t remainder = offset % alignment;
    if (remainder == 0) {
        return 0;
    }
    return alignment - remainder;
}

// Align offset to alignment boundary
size_t kraken_align_offset(size_t offset, size_t alignment) {
    size_t padding = kraken_calculate_padding(offset, alignment);
    return offset + padding;
}

// Calculate struct size with padding
typedef struct {
    size_t field_size;
    size_t field_alignment;
} kraken_field_info_t;

size_t kraken_calculate_struct_size(kraken_field_info_t* fields, size_t field_count, int is_packed) {
    if (fields == NULL || field_count == 0) {
        return 0;
    }
    
    size_t offset = 0;
    size_t max_alignment = 1;
    
    for (size_t i = 0; i < field_count; i++) {
        size_t field_size = fields[i].field_size;
        size_t field_alignment = is_packed ? 1 : fields[i].field_alignment;
        
        if (field_alignment > max_alignment) {
            max_alignment = field_alignment;
        }
        
        // Align field offset
        if (!is_packed) {
            offset = kraken_align_offset(offset, field_alignment);
        }
        
        offset += field_size;
    }
    
    // Add trailing padding to align struct size to max alignment
    if (!is_packed) {
        offset = kraken_align_offset(offset, max_alignment);
    }
    
    return offset;
}

// Calculate field offset in struct
size_t kraken_calculate_field_offset(kraken_field_info_t* fields, size_t field_count, 
                                      size_t field_index, int is_packed) {
    if (fields == NULL || field_index >= field_count) {
        fprintf(stderr, "Struct Error: Invalid field index %zu (count: %zu)\n", 
                field_index, field_count);
        abort();
    }
    
    size_t offset = 0;
    
    for (size_t i = 0; i < field_index; i++) {
        size_t field_size = fields[i].field_size;
        size_t field_alignment = is_packed ? 1 : fields[i].field_alignment;
        
        // Align field offset
        if (!is_packed) {
            offset = kraken_align_offset(offset, field_alignment);
        }
        
        offset += field_size;
    }
    
    // Align target field offset
    if (!is_packed) {
        size_t field_alignment = fields[field_index].field_alignment;
        offset = kraken_align_offset(offset, field_alignment);
    }
    
    return offset;
}

// Validate struct alignment
int kraken_validate_struct_alignment(void* struct_ptr, size_t required_alignment) {
    uintptr_t addr = (uintptr_t)struct_ptr;
    if (addr % required_alignment != 0) {
        fprintf(stderr, "Struct Error: Misaligned struct at %p (required: %zu-byte alignment)\n",
                struct_ptr, required_alignment);
        abort();
    }
    return 1;
}

// Get maximum alignment for struct
size_t kraken_get_max_alignment(kraken_field_info_t* fields, size_t field_count) {
    if (fields == NULL || field_count == 0) {
        return 1;
    }
    
    size_t max_alignment = 1;
    for (size_t i = 0; i < field_count; i++) {
        if (fields[i].field_alignment > max_alignment) {
            max_alignment = fields[i].field_alignment;
        }
    }
    
    return max_alignment;
}

// Calculate padding bytes in struct
size_t kraken_calculate_struct_padding(kraken_field_info_t* fields, size_t field_count, int is_packed) {
    if (is_packed) {
        return 0;
    }
    
    size_t total_field_size = 0;
    for (size_t i = 0; i < field_count; i++) {
        total_field_size += fields[i].field_size;
    }
    
    size_t actual_size = kraken_calculate_struct_size(fields, field_count, is_packed);
    return actual_size - total_field_size;
}
