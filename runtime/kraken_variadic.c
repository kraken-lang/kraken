// Variadic function runtime support (va_list, va_start, va_end)
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

// Wrapper for variadic printf-style functions
int kraken_vprintf(const char* format, ...) {
    if (format == NULL) {
        fprintf(stderr, "Variadic Error: Null format string\n");
        abort();
    }
    
    va_list args;
    va_start(args, format);
    int result = vprintf(format, args);
    va_end(args);
    
    return result;
}

// Wrapper for variadic sprintf
int kraken_vsprintf(char* buffer, const char* format, ...) {
    if (buffer == NULL || format == NULL) {
        fprintf(stderr, "Variadic Error: Null pointer in vsprintf\n");
        abort();
    }
    
    va_list args;
    va_start(args, format);
    int result = vsprintf(buffer, format, args);
    va_end(args);
    
    return result;
}

// Wrapper for variadic snprintf
int kraken_vsnprintf(char* buffer, size_t size, const char* format, ...) {
    if (buffer == NULL || format == NULL) {
        fprintf(stderr, "Variadic Error: Null pointer in vsnprintf\n");
        abort();
    }
    
    va_list args;
    va_start(args, format);
    int result = vsnprintf(buffer, size, format, args);
    va_end(args);
    
    return result;
}

// Wrapper for variadic fprintf
int kraken_vfprintf(FILE* stream, const char* format, ...) {
    if (stream == NULL || format == NULL) {
        fprintf(stderr, "Variadic Error: Null pointer in vfprintf\n");
        abort();
    }
    
    va_list args;
    va_start(args, format);
    int result = vfprintf(stream, format, args);
    va_end(args);
    
    return result;
}

// Generic variadic argument accessor
typedef struct {
    va_list args;
    int count;
    int current;
} kraken_va_context_t;

// Initialize variadic context
kraken_va_context_t* kraken_va_init(int arg_count) {
    kraken_va_context_t* ctx = (kraken_va_context_t*)malloc(sizeof(kraken_va_context_t));
    if (ctx == NULL) {
        fprintf(stderr, "Variadic Error: Failed to allocate va context\n");
        abort();
    }
    ctx->count = arg_count;
    ctx->current = 0;
    return ctx;
}

// Get next int argument
int kraken_va_get_int(kraken_va_context_t* ctx) {
    if (ctx == NULL) {
        fprintf(stderr, "Variadic Error: Null va context\n");
        abort();
    }
    if (ctx->current >= ctx->count) {
        fprintf(stderr, "Variadic Error: No more arguments (accessed %d of %d)\n",
                ctx->current, ctx->count);
        abort();
    }
    ctx->current++;
    return va_arg(ctx->args, int);
}

// Get next double argument
double kraken_va_get_double(kraken_va_context_t* ctx) {
    if (ctx == NULL) {
        fprintf(stderr, "Variadic Error: Null va context\n");
        abort();
    }
    if (ctx->current >= ctx->count) {
        fprintf(stderr, "Variadic Error: No more arguments (accessed %d of %d)\n",
                ctx->current, ctx->count);
        abort();
    }
    ctx->current++;
    return va_arg(ctx->args, double);
}

// Get next string argument
const char* kraken_va_get_string(kraken_va_context_t* ctx) {
    if (ctx == NULL) {
        fprintf(stderr, "Variadic Error: Null va context\n");
        abort();
    }
    if (ctx->current >= ctx->count) {
        fprintf(stderr, "Variadic Error: No more arguments (accessed %d of %d)\n",
                ctx->current, ctx->count);
        abort();
    }
    ctx->current++;
    const char* str = va_arg(ctx->args, const char*);
    if (str == NULL) {
        fprintf(stderr, "Variadic Error: Null string argument\n");
        abort();
    }
    return str;
}

// Get next pointer argument
void* kraken_va_get_pointer(kraken_va_context_t* ctx) {
    if (ctx == NULL) {
        fprintf(stderr, "Variadic Error: Null va context\n");
        abort();
    }
    if (ctx->current >= ctx->count) {
        fprintf(stderr, "Variadic Error: No more arguments (accessed %d of %d)\n",
                ctx->current, ctx->count);
        abort();
    }
    ctx->current++;
    return va_arg(ctx->args, void*);
}

// Clean up variadic context
void kraken_va_cleanup(kraken_va_context_t* ctx) {
    if (ctx != NULL) {
        va_end(ctx->args);
        free(ctx);
    }
}

// Check if more arguments available
int kraken_va_has_more(kraken_va_context_t* ctx) {
    if (ctx == NULL) {
        fprintf(stderr, "Variadic Error: Null va context\n");
        abort();
    }
    return ctx->current < ctx->count;
}

// Get remaining argument count
int kraken_va_remaining(kraken_va_context_t* ctx) {
    if (ctx == NULL) {
        fprintf(stderr, "Variadic Error: Null va context\n");
        abort();
    }
    return ctx->count - ctx->current;
}
