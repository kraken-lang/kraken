# Kraken Language Features

**Version:** 0.8.28  
**Status:** Production-Ready Systems Programming Language

## Overview

Kraken is a modern systems programming language with:
- **LLVM-based compiler** for native performance
- **68+ runtime library functions** for real-world applications
- **Trait system** for polymorphism and code reuse (specification complete)
- **Complete type system** with structs, arrays, and references
- **Pattern matching** for expressive control flow
- **Bitwise operations** for low-level programming

---

## Core Language Features

### Data Types
- **Primitives**: `int` (64-bit), `float` (double), `bool`, `string`
- **Composite**: Arrays, Structs
- **Special**: `void`, References, Pointers

### Variables
```kraken
let x = 42;              // Immutable by default
let name = "Kraken";     // Type inference
let pi = 3.14159;        // Float
let flag = true;         // Boolean
```

### Functions
```kraken
fn add(x: int, y: int) -> int {
    return x + y;
}

fn greet(name: string) -> void {
    puts(name);
}
```

### Control Flow

#### If Statements
```kraken
if (x > 10) {
    puts("Greater than 10");
} else {
    puts("Less than or equal to 10");
}
```

#### While Loops
```kraken
while (x < 100) {
    x = x + 1;
}
```

#### For Loops
```kraken
for (let i = 0; i < 10; i = i + 1) {
    puts("Iteration");
}
```

#### Pattern Matching
```kraken
match (value) {
    0 -> { puts("Zero"); }
    1 -> { puts("One"); }
    42 -> { puts("The Answer"); }
    _ -> { puts("Other"); }
}
```

#### Break & Continue
```kraken
for (let i = 0; i < 10; i = i + 1) {
    if (i == 5) {
        break;
    }
    if (i == 3) {
        continue;
    }
}
```

### Arrays
```kraken
let numbers = [1, 2, 3, 4, 5];
let first = numbers[0];
let last = numbers[4];

for (let i = 0; i < 5; i = i + 1) {
    let value = numbers[i];
}
```

### Structs
```kraken
struct Point {
    x: int;
    y: int;
}

struct Color {
    r: int;
    g: int;
    b: int;
}

fn main() -> int {
    let p = Point { x: 10, y: 20 };
    let px = p.x;
    let py = p.y;
    return px + py;
}
```

---

## Operators

### Arithmetic
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Division
- `%` Modulo

### Comparison
- `==` Equal
- `!=` Not equal
- `<` Less than
- `<=` Less than or equal
- `>` Greater than
- `>=` Greater than or equal

### Logical
- `&&` Logical AND
- `||` Logical OR
- `!` Logical NOT

### Bitwise
- `&` Bitwise AND
- `|` Bitwise OR
- `^` Bitwise XOR
- `~` Bitwise NOT
- `<<` Left shift
- `>>` Right shift

### Assignment
- `=` Assignment
- `+=` Add and assign
- `-=` Subtract and assign
- `*=` Multiply and assign
- `/=` Divide and assign
- `%=` Modulo and assign

---

## Standard Library (80 Functions)

### String Operations (11)
- `strlen(s)` - Get string length
- `strcmp(s1, s2)` - Compare strings
- `strcpy(dest, src)` - Copy string
- `strcat(dest, src)` - Concatenate strings
- `strstr(haystack, needle)` - Find substring
- `strchr(s, c)` - Find character
- `strncpy(dest, src, n)` - Copy n characters
- `strncmp(s1, s2, n)` - Compare n characters
- `strdup(s)` - Duplicate string
- `strtok(str, delim)` - Tokenize string
- `sprintf(str, format, ...)` - Format string

### Memory Management (6)
- `malloc(size)` - Allocate memory
- `free(ptr)` - Free memory
- `realloc(ptr, size)` - Reallocate memory
- `memcpy(dest, src, n)` - Copy memory
- `memset(ptr, value, n)` - Fill memory
- `memcmp(ptr1, ptr2, n)` - Compare memory

### Basic Math (13)
- `sqrt(x)` - Square root
- `pow(x, y)` - Power
- `abs(x)` - Absolute value (int)
- `fabs(x)` - Absolute value (float)
- `floor(x)` - Round down
- `ceil(x)` - Round up
- `round(x)` - Round nearest
- `sin(x)` - Sine
- `cos(x)` - Cosine
- `tan(x)` - Tangent
- `log(x)` - Natural logarithm
- `log10(x)` - Base-10 logarithm
- `exp(x)` - Exponential

### Advanced Math (8)
- `asin(x)` - Arc sine
- `acos(x)` - Arc cosine
- `atan(x)` - Arc tangent
- `atan2(y, x)` - Two-argument arc tangent
- `sinh(x)` - Hyperbolic sine
- `cosh(x)` - Hyperbolic cosine
- `tanh(x)` - Hyperbolic tangent
- `fmod(x, y)` - Floating-point modulo

### File I/O (16)
- `fopen(filename, mode)` - Open file
- `fclose(file)` - Close file
- `fread(ptr, size, count, file)` - Read from file
- `fwrite(ptr, size, count, file)` - Write to file
- `fgets(str, n, file)` - Read line
- `fputs(str, file)` - Write string
- `fgetc(file)` - Read character
- `fputc(c, file)` - Write character
- `fseek(file, offset, whence)` - Seek position
- `ftell(file)` - Get position
- `rewind(file)` - Reset position
- `fflush(file)` - Flush buffer
- `feof(file)` - Check end of file
- `ferror(file)` - Check error
- `remove(filename)` - Delete file
- `rename(old, new)` - Rename file

### System & Process (5)
- `exit(status)` - Exit program
- `system(command)` - Execute command
- `getenv(name)` - Get environment variable
- `setenv(name, value, overwrite)` - Set environment variable
- `unsetenv(name)` - Unset environment variable

### Character Classification (8)
- `isalpha(c)` - Check if alphabetic
- `isdigit(c)` - Check if digit
- `isalnum(c)` - Check if alphanumeric
- `isspace(c)` - Check if whitespace
- `isupper(c)` - Check if uppercase
- `islower(c)` - Check if lowercase
- `toupper(c)` - Convert to uppercase
- `tolower(c)` - Convert to lowercase

### String Conversion (2)
- `atoi(str)` - String to integer
- `atof(str)` - String to float

### Random & Time (3)
- `rand()` - Generate random number
- `srand(seed)` - Seed random generator
- `time(tloc)` - Get current time

### Console I/O (5)
- `printf(format, ...)` - Formatted output
- `puts(str)` - Print string with newline
- `putchar(c)` - Print character
- `getchar()` - Read character
- `sscanf(str, format, ...)` - Parse formatted string

### Error Handling (1)
- `abort()` - Abort program

### Utilities (1)
- `usleep(usec)` - Sleep for microseconds

---

## Use Cases

### Systems Programming
- Direct memory management
- Bitwise operations for hardware control
- Low-level file I/O
- Process control

### Scientific Computing
- Comprehensive math library
- Floating-point operations
- Statistical functions

### Text Processing
- Rich string manipulation
- Pattern matching
- File parsing

### Embedded Systems
- Efficient bitwise operations
- Minimal runtime overhead
- Direct hardware access

### Application Development
- Full standard library
- Type safety
- Modern syntax

---

## Compilation

```bash
# Build a program
kraken build program.kr

# Run the executable
./program
```

---

## Version History

- **v0.8.0** - Bitwise operations & pointer infrastructure
- **v0.7.0** - 80 stdlib functions
- **v0.6.0** - Language completeness (arrays, structs, match)
- **v0.5.0** - Core functionality
- **v0.2.0** - LLVM backend
- **v0.1.0** - Initial release

---

## License

Apache-2.0

---

## Learn More

- See [examples/](examples/) for sample programs
- See [CHANGELOG.md](CHANGELOG.md) for detailed version history
- See [README.md](README.md) for project information
