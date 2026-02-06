<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>07 — Generics & Traits</h1>
</div>

## 1. Generic Parameters

Functions, structs, enums, traits, and impl blocks can be parameterized by type variables.

```kraken
fn identity<T>(x: T) -> T {
    return x;
}

struct Pair<A, B> {
    first: A;
    second: B;
}

enum Option<T> {
    Some(T),
    None,
}
```

Type parameters are uppercase by convention.

## 2. Monomorphization

Kraken uses monomorphization: each unique combination of type arguments produces a specialized copy at compile time. There is no runtime polymorphism cost for generics.

```kraken
identity::<int>(42);       // generates identity_int
identity::<string>("hi");  // generates identity_string
```

Generic containers are lowered to concrete types:
- `Vec<int>` → `VecInt`
- `Vec<string>` → `VecString`
- `Map<string, int>` → `MapStringInt`

## 3. Type Inference

The compiler infers generic type arguments from call-site context when unambiguous:

```kraken
let x = identity(42);     // T inferred as int
```

When inference is ambiguous, use turbofish syntax:

```kraken
let x = identity::<int>(42);
```

## 4. Where Clauses

Where clauses constrain type parameters to types that implement specific traits:

```kraken
fn print_all<T>(items: Vec<T>) -> void where T: Display {
    // T must implement Display
}

fn max<T>(a: T, b: T) -> T where T: PartialOrd {
    if (a > b) { return a; }
    return b;
}
```

Multiple constraints use `+`:

```kraken
fn process<T>(x: T) -> void where T: Clone + Display + Send {
    // ...
}
```

## 5. Trait Declarations

Traits define a set of methods that types can implement.

```kraken
trait Drawable {
    fn draw(self) -> void;
    fn area(self) -> float;
}
```

### Required Methods

Methods without a body must be implemented by every type that implements the trait.

### Provided Methods (Defaults)

Methods with a body provide a default implementation that can be overridden:

```kraken
trait Printable {
    fn to_string(self) -> string;

    fn print(self) -> void {
        printf("%s\n", self.to_string());
    }
}
```

### Async Trait Methods

```kraken
trait AsyncReader {
    async fn read(self) -> bytes;
}
```

### Generic Traits

```kraken
trait Converter<T> {
    fn convert(self) -> T;
}
```

## 6. Super Traits

A trait can require that implementing types also implement other traits:

```kraken
trait Shape: Drawable + PartialEq {
    fn perimeter(self) -> float;
}
```

Any type implementing `Shape` must also implement `Drawable` and `PartialEq`.

## 7. Associated Types

Traits can declare associated types that implementors must define:

```kraken
trait Iterator {
    type Item;
    fn next(self) -> Option<Self::Item>;
}

trait IntoIterator {
    type Item;
    type IntoIter: Iterator;
    fn into_iter(self) -> Self::IntoIter;
}
```

## 8. Trait Implementations

```kraken
impl Drawable for Circle {
    fn draw(self) -> void {
        printf("Drawing circle with radius %f\n", self.radius);
    }

    fn area(self) -> float {
        return 3.14159 * self.radius * self.radius;
    }
}
```

### Generic Trait Implementations

```kraken
impl<T> Clone for Box<T> where T: Clone {
    fn clone(self) -> Box<T> {
        return Box::<T> { value: self.value };
    }
}
```

## 9. Trait Objects (Dynamic Dispatch)

Trait objects enable runtime polymorphism through dynamic dispatch:

```kraken
let shape: dyn Drawable = get_shape();
```

Trait objects with multiple bounds:

```kraken
let obj: dyn Drawable + Send + Sync = get_concurrent_shape();
```

Trait objects are represented as fat pointers: `{ data_ptr, vtable_ptr }`. Method calls go through the vtable for dynamic dispatch.

## 10. Standard Traits

Kraken defines standard traits that provide common behaviour:

| Trait | Methods | Purpose |
|-------|---------|---------|
| `Clone` | `clone(self) -> Self` | Deep copy |
| `Copy` | (marker) | Bitwise copy (implicit) |
| `Drop` | `drop(self)` | Destructor (RAII cleanup) |
| `Default` | `default() -> Self` | Default value constructor |
| `Display` | `display(self) -> string` | Human-readable formatting |
| `Debug` | `debug(self) -> string` | Debug formatting |
| `Hash` | `hash(self) -> int` | Hash value computation |
| `PartialEq` | `eq(self, other) -> bool` | Equality comparison |
| `Eq` | (marker) | Total equality |
| `PartialOrd` | `lt, le, gt, ge` | Partial ordering |
| `Ord` | `cmp(self, other) -> int` | Total ordering |
| `Send` | (marker) | Safe to send between threads |
| `Sync` | (marker) | Safe to share between threads |

### Operator Traits

| Trait | Operator | Method |
|-------|----------|--------|
| `Add` | `+` | `add(self, rhs) -> Self` |
| `Sub` | `-` | `sub(self, rhs) -> Self` |
| `Mul` | `*` | `mul(self, rhs) -> Self` |
| `Div` | `/` | `div(self, rhs) -> Self` |
| `Rem` | `%` | `rem(self, rhs) -> Self` |
| `Neg` | `-` (unary) | `neg(self) -> Self` |
| `Not` | `!` | `not(self) -> Self` |
| `BitAnd` | `&` | `bitand(self, rhs) -> Self` |
| `BitOr` | `\|` | `bitor(self, rhs) -> Self` |
| `BitXor` | `^` | `bitxor(self, rhs) -> Self` |
| `Shl` | `<<` | `shl(self, rhs) -> Self` |
| `Shr` | `>>` | `shr(self, rhs) -> Self` |

### Conversion Traits

| Trait | Method | Purpose |
|-------|--------|---------|
| `From<T>` | `from(T) -> Self` | Infallible conversion |
| `Into<T>` | `into(self) -> T` | Reciprocal of From |
| `TryFrom<T>` | `try_from(T) -> Result<Self, E>` | Fallible conversion |
| `TryInto<T>` | `try_into(self) -> Result<T, E>` | Reciprocal of TryFrom |

## 11. Derive Macros

Common traits can be automatically implemented with `#[derive(...)]`:

```kraken
#[derive(Clone, Debug, PartialEq)]
struct Point {
    x: int;
    y: int;
}
```

Supported derive traits: `Clone`, `Debug`, `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`.
