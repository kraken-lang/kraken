#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// ============================================================================
// BOUNDS CHECKING INFRASTRUCTURE
// ============================================================================

// Environment variable to enable bounds checking
static int bounds_check_enabled = -1;

// Check if bounds checking is enabled
static int is_bounds_check_enabled() {
    if (bounds_check_enabled == -1) {
        const char* env = getenv("KRAKEN_BOUNDS_CHECK");
        bounds_check_enabled = (env != NULL && strcmp(env, "1") == 0) ? 1 : 0;
    }
    return bounds_check_enabled;
}

// Bounds check for array/slice access
void kraken_bounds_check(int64_t index, int64_t length, const char* file, int line) {
    if (!is_bounds_check_enabled()) {
        return;
    }
    
    if (index < 0 || index >= length) {
        fprintf(stderr, "\n");
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "KRAKEN RUNTIME ERROR: Index Out of Bounds\n");
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "Index:    %lld\n", (long long)index);
        fprintf(stderr, "Length:   %lld\n", (long long)length);
        if (file) {
            fprintf(stderr, "Location: %s:%d\n", file, line);
        }
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "\n");
        abort();
    }
}

// Bounds check for range access
void kraken_bounds_check_range(int64_t start, int64_t end, int64_t length, const char* file, int line) {
    if (!is_bounds_check_enabled()) {
        return;
    }
    
    if (start < 0 || start > length || end < 0 || end > length || start > end) {
        fprintf(stderr, "\n");
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "KRAKEN RUNTIME ERROR: Range Out of Bounds\n");
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "Range:    [%lld..%lld]\n", (long long)start, (long long)end);
        fprintf(stderr, "Length:   %lld\n", (long long)length);
        if (file) {
            fprintf(stderr, "Location: %s:%d\n", file, line);
        }
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "\n");
        abort();
    }
}

// ============================================================================
// MEMORY LEAK DETECTION
// ============================================================================

#define MAX_ALLOCATIONS 10000

typedef struct {
    void* ptr;
    size_t size;
    const char* file;
    int line;
    int active;
} Allocation;

static Allocation allocations[MAX_ALLOCATIONS];
static int allocation_count = 0;
static int leak_check_enabled = -1;
static int leak_check_initialized = 0;

// Check if leak checking is enabled
static int is_leak_check_enabled() {
    if (leak_check_enabled == -1) {
        const char* env = getenv("KRAKEN_LEAK_CHECK");
        leak_check_enabled = (env != NULL && strcmp(env, "1") == 0) ? 1 : 0;
    }
    return leak_check_enabled;
}

// Report leaked allocations at exit
static void report_leaks() {
    if (!is_leak_check_enabled()) {
        return;
    }
    
    int leak_count = 0;
    size_t total_leaked = 0;
    
    for (int i = 0; i < allocation_count; i++) {
        if (allocations[i].active) {
            if (leak_count == 0) {
                fprintf(stderr, "\n");
                fprintf(stderr, "===========================================\n");
                fprintf(stderr, "KRAKEN MEMORY LEAK REPORT\n");
                fprintf(stderr, "===========================================\n");
            }
            
            leak_count++;
            total_leaked += allocations[i].size;
            
            fprintf(stderr, "Leak #%d: %zu bytes at %p\n", 
                    leak_count, allocations[i].size, allocations[i].ptr);
            if (allocations[i].file) {
                fprintf(stderr, "  Allocated at: %s:%d\n", 
                        allocations[i].file, allocations[i].line);
            }
        }
    }
    
    if (leak_count > 0) {
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "Total: %d leaks, %zu bytes\n", leak_count, total_leaked);
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "\n");
    }
}

// Initialize leak checking
static void init_leak_check() {
    if (!leak_check_initialized && is_leak_check_enabled()) {
        atexit(report_leaks);
        leak_check_initialized = 1;
        memset(allocations, 0, sizeof(allocations));
    }
}

// Track allocation
static void track_allocation(void* ptr, size_t size, const char* file, int line) {
    if (!is_leak_check_enabled()) {
        return;
    }
    
    init_leak_check();
    
    if (allocation_count >= MAX_ALLOCATIONS) {
        fprintf(stderr, "Warning: Allocation tracking limit reached\n");
        return;
    }
    
    allocations[allocation_count].ptr = ptr;
    allocations[allocation_count].size = size;
    allocations[allocation_count].file = file;
    allocations[allocation_count].line = line;
    allocations[allocation_count].active = 1;
    allocation_count++;
}

// Untrack allocation
static void untrack_allocation(void* ptr) {
    if (!is_leak_check_enabled()) {
        return;
    }
    
    for (int i = 0; i < allocation_count; i++) {
        if (allocations[i].active && allocations[i].ptr == ptr) {
            allocations[i].active = 0;
            return;
        }
    }
}

// ============================================================================
// SAFE MEMORY ALLOCATION WRAPPERS
// ============================================================================

// Safe malloc with leak tracking
void* kraken_malloc_tracked(size_t size, const char* file, int line) {
    void* ptr = malloc(size);
    if (ptr) {
        track_allocation(ptr, size, file, line);
    }
    return ptr;
}

// Safe calloc with leak tracking
void* kraken_calloc_tracked(size_t count, size_t size, const char* file, int line) {
    void* ptr = calloc(count, size);
    if (ptr) {
        track_allocation(ptr, count * size, file, line);
    }
    return ptr;
}

// Safe realloc with leak tracking
void* kraken_realloc_tracked(void* ptr, size_t new_size, const char* file, int line) {
    if (ptr) {
        untrack_allocation(ptr);
    }
    
    void* new_ptr = realloc(ptr, new_size);
    if (new_ptr) {
        track_allocation(new_ptr, new_size, file, line);
    }
    
    return new_ptr;
}

// Safe free with leak tracking
void kraken_free_tracked(void* ptr) {
    if (ptr) {
        untrack_allocation(ptr);
        free(ptr);
    }
}

// ============================================================================
// NULL POINTER CHECKING
// ============================================================================

// Check for null pointer
void kraken_null_check(void* ptr, const char* file, int line) {
    if (ptr == NULL) {
        fprintf(stderr, "\n");
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "KRAKEN RUNTIME ERROR: Null Pointer Access\n");
        fprintf(stderr, "===========================================\n");
        if (file) {
            fprintf(stderr, "Location: %s:%d\n", file, line);
        }
        fprintf(stderr, "===========================================\n");
        fprintf(stderr, "\n");
        abort();
    }
}

// ============================================================================
// MEMORY DEBUGGING UTILITIES
// ============================================================================

// Get allocation count
int64_t kraken_get_allocation_count() {
    if (!is_leak_check_enabled()) {
        return -1;
    }
    
    int count = 0;
    for (int i = 0; i < allocation_count; i++) {
        if (allocations[i].active) {
            count++;
        }
    }
    return count;
}

// Get total allocated bytes
int64_t kraken_get_allocated_bytes() {
    if (!is_leak_check_enabled()) {
        return -1;
    }
    
    size_t total = 0;
    for (int i = 0; i < allocation_count; i++) {
        if (allocations[i].active) {
            total += allocations[i].size;
        }
    }
    return (int64_t)total;
}

// Print allocation statistics
void kraken_print_allocation_stats() {
    if (!is_leak_check_enabled()) {
        fprintf(stderr, "Leak checking is not enabled. Set KRAKEN_LEAK_CHECK=1\n");
        return;
    }
    
    int active_count = 0;
    size_t total_bytes = 0;
    
    for (int i = 0; i < allocation_count; i++) {
        if (allocations[i].active) {
            active_count++;
            total_bytes += allocations[i].size;
        }
    }
    
    fprintf(stderr, "\n");
    fprintf(stderr, "===========================================\n");
    fprintf(stderr, "KRAKEN ALLOCATION STATISTICS\n");
    fprintf(stderr, "===========================================\n");
    fprintf(stderr, "Active allocations: %d\n", active_count);
    fprintf(stderr, "Total bytes:        %zu\n", total_bytes);
    fprintf(stderr, "Total tracked:      %d\n", allocation_count);
    fprintf(stderr, "===========================================\n");
    fprintf(stderr, "\n");
}
