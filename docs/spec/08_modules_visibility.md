<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>08 — Modules & Visibility</h1>
</div>

## 1. Module Declarations

Every Kraken source file can declare its module path:

```kraken
module math.geometry;
```

Module paths are dot-separated. The module declaration must be the first non-comment statement in the file.

## 2. Imports

```kraken
import math.geometry;
import utils.string_helpers;
```

An `import` makes all public symbols from the named module available in the current file.

## 3. File-Based Resolution

Modules map to files using dot-to-path conversion relative to the project root:

| Module Path | File Path |
|-------------|-----------|
| `math.geometry` | `math/geometry.kr` |
| `utils.string_helpers` | `utils/string_helpers.kr` |

## 4. Visibility

By default, all declarations are **private** to their module. The `pub` keyword makes a declaration visible to importing modules.

```kraken
pub fn public_api() -> int { return 42; }
fn private_helper() -> int { return 0; }

pub struct PublicType {
    pub x: int;     // public field
    y: int;         // private field
}
```

### Visibility Rules

| Declaration | Default | With `pub` |
|-------------|---------|------------|
| Functions | Module-private | Visible to importers |
| Structs | Module-private | Visible to importers |
| Struct fields | Private | Visible to importers |
| Enums | Module-private | Visible to importers |
| Traits | Module-private | Visible to importers |
| Constants | Module-private | Visible to importers |
| Type aliases | Module-private | Visible to importers |

## 5. Symbol Resolution Order

When resolving a name, the compiler searches in order:

1. Local scope (block, function)
2. Module scope (current file)
3. Imported modules (in declaration order)
4. Built-in/stdlib symbols

## 6. Circular Import Detection

Circular imports are detected at compile time and produce an error:

```
error[KRA0051]: circular import detected: a -> b -> a
```

## 7. Duplicate Symbol Detection

If two imported modules export the same symbol name, the compiler produces an error:

```
error[KRA0052]: duplicate symbol 'process' imported from both 'module_a' and 'module_b'
```
