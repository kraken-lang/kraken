<div align="center">
    <img width="auto" height="90" alt="Kraken Language" src="https://raw.githubusercontent.com/kraken-lang/.github/refs/heads/main/images/kraken-logo.png">
    <h1><sub><sup>KRAKEN LANGUAGE SPECIFICATION</sup></sub><br>03 — Declarations</h1>
</div>

## 1. Variable Declarations

Variables are declared with `let`. They are reassignable by default (no `mut` keyword required).

```kraken
let x = 42;
let name: string = "Kraken";
let uninitialized: int;
```

### Destructuring

Variables can be destructured from tuples and structs:

```kraken
let (a, b) = (1, 2);
let Point { x, y } = some_point;
```

## 2. Constant Declarations

Constants are declared with `const` and must have an initializer. They cannot be reassigned.

```kraken
const MAX_SIZE = 1024;
const PI: float = 3.14159;
```

## 3. Function Declarations

```kraken
fn add(a: int, b: int) -> int {
    return a + b;
}
```

### Modifiers

- **`pub`** — makes the function visible outside its module.
- **`async`** — marks the function as asynchronous (returns a future).
- **`unsafe`** — allows unsafe operations inside the function body.

```kraken
pub async fn fetch_data(url: string) -> string {
    // ...
}

unsafe fn raw_access(ptr: *mut int) -> int {
    return *ptr;
}
```

### Variadic Functions

Functions can accept a variable number of arguments using `...` after the last named parameter:

```kraken
fn printf(format: string, ...) -> int;
```

### Generic Functions

```kraken
fn identity<T>(x: T) -> T {
    return x;
}

fn max<T>(a: T, b: T) -> T where T: PartialOrd {
    if (a > b) { return a; }
    return b;
}
```

### Parameter Destructuring

Function parameters support pattern destructuring:

```kraken
fn distance((x1, y1): (int, int), (x2, y2): (int, int)) -> float {
    // ...
}

fn get_x(Point { x, .. }: Point) -> int {
    return x;
}
```

### Reference Parameters

Parameters can be passed by reference:

```kraken
fn increment(x: &int) -> int {
    return *x + 1;
}
```

## 4. Struct Declarations

Structs are product types with named fields. Fields are terminated by semicolons.

```kraken
struct Point {
    x: int;
    y: int;
}

pub struct Color {
    pub r: int;
    pub g: int;
    pub b: int;
}
```

### Generic Structs

```kraken
struct Pair<A, B> {
    first: A;
    second: B;
}
```

### Repr Attributes

Struct layout can be controlled with `#[repr(...)]` attributes for FFI compatibility:

```kraken
#[repr(C)]
struct CPoint {
    x: int;
    y: int;
}

#[repr(packed)]
struct PackedData {
    flag: bool;
    value: int;
}

#[repr(align(16))]
struct Aligned {
    data: int;
}
```

### Struct Literals

```kraken
let p = Point { x: 10, y: 20 };
let pair = Pair::<int, string> { first: 1, second: "hello" };
```

## 5. Enum Declarations

Enums are sum types (tagged unions) with named variants. Variants can optionally carry payloads.

```kraken
enum Color {
    Red,
    Green,
    Blue,
}

enum Option<T> {
    Some(T),
    None,
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}

enum Shape {
    Circle { radius: float; },
    Rectangle { width: float; height: float; },
}
```

### Enum Variant Construction

```kraken
let c = Color::Red;
let x = Option::Some(42);
let r = Result::Ok("success");
```

## 6. Union Declarations

Unions are untagged; all fields share the same memory. Accessing the wrong field is undefined behaviour.

```kraken
union Data {
    int_val: int;
    float_val: float;
    ptr_val: bytes;
}
```

The union is sized to its largest field. Field access uses bitcast to the appropriate type.

## 7. Trait Declarations

Traits define shared behaviour through method signatures.

```kraken
trait Drawable {
    fn draw(self) -> void;
    fn area(self) -> float;
}

pub trait Clone {
    fn clone(self) -> Self;
}
```

### Required vs. Provided Methods

Methods without a body are required; methods with a body provide a default implementation:

```kraken
trait Printable {
    fn to_string(self) -> string;   // required

    fn print(self) -> void {        // provided (default)
        printf("%s\n", self.to_string());
    }
}
```

### Super Traits

```kraken
trait OrderedDrawable: Drawable + PartialOrd {
    fn z_index(self) -> int;
}
```

### Associated Types

```kraken
trait Iterator {
    type Item;
    fn next(self) -> Option<Self::Item>;
}
```

### Trait Implementations

```kraken
impl Drawable for Circle {
    fn draw(self) -> void { /* ... */ }
    fn area(self) -> float { return 3.14 * self.radius * self.radius; }
}
```

## 8. Impl Blocks

Impl blocks attach methods to a type without a trait:

```kraken
impl Point {
    fn new(x: int, y: int) -> Point {
        return Point { x: x, y: y };
    }

    fn distance(self, other: Point) -> float {
        // ...
    }
}
```

## 9. Type Aliases

```kraken
type Coordinate = int;
type StringMap = MapStringString;
pub type Result<T> = Result<T, string>;
```

## 10. Interface Declarations

Interfaces define method signatures that implementing types must provide:

```kraken
interface Serializable {
    fn serialize(self) -> string;
    fn deserialize(data: string) -> Self;
}
```

## 11. Class Declarations

Classes combine fields and methods in a single declaration:

```kraken
pub class Animal {
    name: string;
    age: int;

    fn speak(self) -> string {
        return self.name;
    }
}
```

## 12. Module & Import Declarations

See [Chapter 08 — Modules & Visibility](08_modules_visibility.md).
