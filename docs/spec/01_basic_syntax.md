<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>01 — Lexical Structure</h1>
</div>

## 1. Source Encoding

Kraken source files are UTF-8 encoded text. The file extension is `.kr`.

## 2. Tokens

A Kraken program is a sequence of tokens separated by whitespace and comments. The lexer produces the following token categories:

- **Literals** — `IntLiteral`, `FloatLiteral`, `StringLiteral`, `BoolLiteral`
- **Identifiers** — user-defined names
- **Keywords** — reserved words with special meaning
- **Operators** — arithmetic, comparison, logical, bitwise, assignment
- **Delimiters** — `( ) { } [ ] ; , . : :: -> ? | #`
- **Special** — `Eof`, `Newline`, `Comment`

## 3. Comments

```kraken
// Line comment — ignored by the compiler
/// Doc comment — attached to the following declaration
//! Module doc comment — describes the enclosing module
```

Comments extend to the end of the line. There are no block comments.

## 4. Keywords

Keywords are reserved and cannot be used as identifiers.

### Control Flow
`if`, `else`, `match`, `for`, `while`, `do`, `break`, `continue`, `return`

### Primitive Types
`int`, `float`, `bool`, `string`, `str`, `bytes`, `void`

### Declarations
`fn`, `let`, `const`, `module`, `import`, `struct`, `enum`, `trait`, `class`, `interface`, `type`, `impl`, `union`

### Modifiers
`pub`, `priv`, `static`, `async`, `await`, `spawn`, `ref`, `mut`, `move`, `unsafe`

### Literals & Special
`true`, `false`, `null`, `self`, `super`, `defer`

### Advanced
`generic`, `where`, `as`, `in`, `dyn`

### Macros & Compile-Time
`macro`, `macro_rules`, `derive`, `inline`, `no_mangle`, `test`, `static_assert`

### Container Types
`VecInt`, `VecString`, `VecBytes`, `MapStringInt`, `MapStringString`, `SliceInt`, `SliceString`, `SliceBytes`

## 5. Operators

Operators are listed by precedence (highest to lowest):

| Precedence | Operators | Associativity | Description |
|------------|-----------|---------------|-------------|
| 14 | `.` `::` `?` | Left | Member access, path, try |
| 13 | `()` `[]` | Left | Call, index |
| 12 | `-` `!` `~` `&` `*` | Right (unary) | Negate, not, bitwise not, ref, deref |
| 11 | `*` `/` `%` | Left | Multiplication, division, remainder |
| 10 | `+` `-` | Left | Addition, subtraction |
| 9 | `<<` `>>` | Left | Bit shifts |
| 8 | `&` | Left | Bitwise AND |
| 7 | `^` | Left | Bitwise XOR |
| 6 | `\|` | Left | Bitwise OR |
| 5 | `==` `!=` | Left | Equality |
| 4 | `<` `<=` `>` `>=` | Left | Comparison |
| 3 | `&&` | Left | Logical AND |
| 2 | `\|\|` | Left | Logical OR |
| 1 | `=` `+=` `-=` `*=` `/=` `%=` | Right | Assignment |

### Range Operators

- `..` — exclusive range: `0..10` produces values 0 through 9
- `..=` — inclusive range: `0..=10` produces values 0 through 10

## 6. Literals

### Integer Literals

```kraken
42          // decimal
0xFF        // hexadecimal
0b1010      // binary
0o77        // octal
```

Integer literals have type `int` (64-bit signed).

### Float Literals

```kraken
3.14
2.0e10
1.5E-3
```

Float literals have type `float` (64-bit IEEE 754 double).

### String Literals

```kraken
"hello, world"
"line one\nline two"
"tab\there"
"null byte: \0"
"hex escape: \x41"   // 'A'
```

String literals have type `string` (null-terminated C string pointer). Supported escape sequences: `\\`, `\"`, `\n`, `\r`, `\t`, `\0`, `\xHH`.

### Boolean Literals

```kraken
true
false
```

### Null Literal

```kraken
null
```

Represents a null pointer value.

## 7. Identifiers

An identifier starts with a letter or underscore, followed by zero or more letters, digits, or underscores. Identifiers are case-sensitive.

```
identifier = [a-zA-Z_][a-zA-Z0-9_]*
```

## 8. Semicolons

Kraken uses explicit semicolons to terminate statements. Struct fields also use semicolons:

```kraken
let x = 42;
struct Point {
    x: int;
    y: int;
}
```

## 9. Delimiters

| Token | Name | Usage |
|-------|------|-------|
| `(` `)` | Parentheses | Grouping, function calls, conditions |
| `{` `}` | Braces | Blocks, struct/enum bodies |
| `[` `]` | Brackets | Arrays, indexing, slicing |
| `;` | Semicolon | Statement terminator, field separator |
| `,` | Comma | Parameter/argument separator |
| `.` | Dot | Member access, tuple indexing |
| `:` | Colon | Type annotations |
| `::` | Double colon | Path separator, turbofish |
| `->` | Arrow | Return type, match arm body |
| `?` | Question mark | Try operator |
| `\|` | Pipe | Closure parameters, or-patterns |
| `#` | Hash | Attribute prefix |
