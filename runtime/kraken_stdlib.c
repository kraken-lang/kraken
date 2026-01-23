#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#include <time.h>

// ============================================================================
// FILE I/O FUNCTIONS
// ============================================================================

// Wrapper for fseek
int64_t kraken_fseek(void* stream, int64_t offset, int64_t whence) {
    if (!stream) return -1;
    return (int64_t)fseek((FILE*)stream, (long)offset, (int)whence);
}

// Wrapper for ftell
int64_t kraken_ftell(void* stream) {
    if (!stream) return -1;
    return (int64_t)ftell((FILE*)stream);
}

// Wrapper for rewind
void kraken_rewind(void* stream) {
    if (stream) {
        rewind((FILE*)stream);
    }
}

// Wrapper for feof
int64_t kraken_feof(void* stream) {
    if (!stream) return 0;
    return (int64_t)feof((FILE*)stream);
}

// Wrapper for ferror
int64_t kraken_ferror(void* stream) {
    if (!stream) return 0;
    return (int64_t)ferror((FILE*)stream);
}

// Wrapper for fopen
void* kraken_fopen(const char* filename, const char* mode) {
    if (!filename || !mode) return NULL;
    return (void*)fopen(filename, mode);
}

// Wrapper for fclose
int64_t kraken_fclose(void* stream) {
    if (!stream) return -1;
    return (int64_t)fclose((FILE*)stream);
}

// Wrapper for fread
int64_t kraken_fread(void* ptr, int64_t size, int64_t count, void* stream) {
    if (!ptr || !stream) return 0;
    return (int64_t)fread(ptr, (size_t)size, (size_t)count, (FILE*)stream);
}

// Wrapper for fwrite
int64_t kraken_fwrite(const void* ptr, int64_t size, int64_t count, void* stream) {
    if (!ptr || !stream) return 0;
    return (int64_t)fwrite(ptr, (size_t)size, (size_t)count, (FILE*)stream);
}

// ============================================================================
// MATH FUNCTIONS
// ============================================================================

// Trigonometric functions
double kraken_sin(double x) { return sin(x); }
double kraken_cos(double x) { return cos(x); }
double kraken_tan(double x) { return tan(x); }
double kraken_asin(double x) { return asin(x); }
double kraken_acos(double x) { return acos(x); }
double kraken_atan(double x) { return atan(x); }
double kraken_atan2(double y, double x) { return atan2(y, x); }

// Hyperbolic functions
double kraken_sinh(double x) { return sinh(x); }
double kraken_cosh(double x) { return cosh(x); }
double kraken_tanh(double x) { return tanh(x); }

// Exponential and logarithmic functions
double kraken_exp(double x) { return exp(x); }
double kraken_log(double x) { return log(x); }
double kraken_log10(double x) { return log10(x); }
double kraken_log2(double x) { return log2(x); }
double kraken_pow(double base, double exp) { return pow(base, exp); }
double kraken_sqrt(double x) { return sqrt(x); }
double kraken_cbrt(double x) { return cbrt(x); }

// Rounding and remainder functions
double kraken_ceil(double x) { return ceil(x); }
double kraken_floor(double x) { return floor(x); }
double kraken_round(double x) { return round(x); }
double kraken_trunc(double x) { return trunc(x); }
double kraken_fmod(double x, double y) { return fmod(x, y); }
double kraken_remainder(double x, double y) { return remainder(x, y); }

// Other math functions
double kraken_fabs(double x) { return fabs(x); }
double kraken_hypot(double x, double y) { return hypot(x, y); }

// ============================================================================
// TIME FUNCTIONS
// ============================================================================

// Get current time in seconds since epoch
int64_t kraken_time() {
    return (int64_t)time(NULL);
}

// Get clock ticks
int64_t kraken_clock() {
    return (int64_t)clock();
}

// Get CLOCKS_PER_SEC constant
int64_t kraken_clocks_per_sec() {
    return (int64_t)CLOCKS_PER_SEC;
}

// Calculate difference between two times
double kraken_difftime(int64_t time1, int64_t time0) {
    return difftime((time_t)time1, (time_t)time0);
}

// Format time string (simplified wrapper)
char* kraken_strftime(const char* format, int64_t timestamp) {
    if (!format) return NULL;
    
    time_t t = (time_t)timestamp;
    struct tm* tm_info = localtime(&t);
    if (!tm_info) return NULL;
    
    char* buffer = (char*)malloc(256);
    if (!buffer) return NULL;
    
    size_t result = strftime(buffer, 256, format, tm_info);
    if (result == 0) {
        free(buffer);
        return NULL;
    }
    
    return buffer;
}

// Get current timestamp as struct (year, month, day, hour, min, sec)
typedef struct {
    int64_t year;
    int64_t month;
    int64_t day;
    int64_t hour;
    int64_t minute;
    int64_t second;
    int64_t weekday;
    int64_t yearday;
} KrakenTime;

KrakenTime* kraken_localtime(int64_t timestamp) {
    time_t t = (time_t)timestamp;
    struct tm* tm_info = localtime(&t);
    if (!tm_info) return NULL;
    
    KrakenTime* kt = (KrakenTime*)malloc(sizeof(KrakenTime));
    if (!kt) return NULL;
    
    kt->year = tm_info->tm_year + 1900;
    kt->month = tm_info->tm_mon + 1;
    kt->day = tm_info->tm_mday;
    kt->hour = tm_info->tm_hour;
    kt->minute = tm_info->tm_min;
    kt->second = tm_info->tm_sec;
    kt->weekday = tm_info->tm_wday;
    kt->yearday = tm_info->tm_yday;
    
    return kt;
}

// ============================================================================
// MEMORY FUNCTIONS
// ============================================================================

// Wrapper for calloc
void* kraken_calloc(int64_t count, int64_t size) {
    if (count <= 0 || size <= 0) return NULL;
    return calloc((size_t)count, (size_t)size);
}

// Wrapper for aligned_alloc (C11)
void* kraken_aligned_alloc(int64_t alignment, int64_t size) {
    if (alignment <= 0 || size <= 0) return NULL;
    #if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
        return aligned_alloc((size_t)alignment, (size_t)size);
    #else
        // Fallback for older C standards
        return malloc((size_t)size);
    #endif
}

// Wrapper for realloc
void* kraken_realloc(void* ptr, int64_t new_size) {
    if (new_size <= 0) return NULL;
    return realloc(ptr, (size_t)new_size);
}

// Wrapper for free
void kraken_free(void* ptr) {
    if (ptr) {
        free(ptr);
    }
}

// Memory copy
void* kraken_memcpy(void* dest, const void* src, int64_t n) {
    if (!dest || !src || n <= 0) return dest;
    return memcpy(dest, src, (size_t)n);
}

// Memory set
void* kraken_memset(void* s, int64_t c, int64_t n) {
    if (!s || n <= 0) return s;
    return memset(s, (int)c, (size_t)n);
}

// Memory compare
int64_t kraken_memcmp(const void* s1, const void* s2, int64_t n) {
    if (!s1 || !s2 || n <= 0) return 0;
    return (int64_t)memcmp(s1, s2, (size_t)n);
}
