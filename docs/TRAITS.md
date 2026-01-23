# Traits in Kraken

This document provides a comprehensive specification of the trait system in Kraken, including standard traits, operator overloading traits, and trait-based patterns.

## Table of Contents

1. [Overview](#overview)
2. [Trait Syntax](#trait-syntax)
3. [Standard Traits](#standard-traits)
4. [Operator Overloading Traits](#operator-overloading-traits)
5. [Conversion Traits](#conversion-traits)
6. [Iterator Traits](#iterator-traits)
7. [Trait Bounds](#trait-bounds)
8. [Implementation Patterns](#implementation-patterns)

## Overview

Traits are Kraken's mechanism for defining shared behavior across types. They enable:
- **Polymorphism**: Write code that works with multiple types
- **Operator Overloading**: Define custom behavior for operators
- **Type Constraints**: Specify requirements for generic types
- **Code Reuse**: Share implementations across types

## Trait Syntax

### Trait Declaration

```kraken
trait TraitName {
    // Required methods (must be implemented)
    fn required_method(self) -> ReturnType;
    
    // Provided methods (default implementation)
    fn provided_method(self) -> ReturnType {
        // Default implementation
    }
    
    // Associated types
    type AssociatedType;
    
    // Associated constants
    const CONSTANT: Type;
}
```

### Trait Implementation

```kraken
impl TraitName for TypeName {
    fn required_method(self) -> ReturnType {
        // Implementation
    }
    
    // Can override provided methods
    fn provided_method(self) -> ReturnType {
        // Custom implementation
    }
    
    type AssociatedType = ConcreteType;
    const CONSTANT: Type = value;
}
```

### Generic Trait Implementation

```kraken
impl<T> TraitName for Vec<T> {
    fn method(self) -> ReturnType {
        // Implementation for Vec<T>
    }
}
```

### Trait Bounds

```kraken
// Single trait bound
fn function<T: Clone>(x: T) -> T {
    return x.clone();
}

// Multiple trait bounds
fn function<T: Clone + Debug>(x: T) -> void {
    println(x);
}

// Where clauses
fn function<T, U>(x: T, y: U) -> void
where
    T: Clone + Debug,
    U: Display
{
    // Implementation
}
```

## Standard Traits

### Clone - Explicit Copying

```kraken
trait Clone {
    fn clone(self) -> Self;
}
```

**Purpose**: Explicit duplication of values.

**Example Implementation**:
```kraken
struct Point {
    x: int,
    y: int,
}

impl Clone for Point {
    fn clone(self) -> Point {
        return Point { x: self.x, y: self.y };
    }
}

// Usage
let p1 = Point { x: 10, y: 20 };
let p2 = p1.clone();
```

### Copy - Implicit Copying

```kraken
trait Copy: Clone {
    // Marker trait - no methods
}
```

**Purpose**: Types that can be copied implicitly (bitwise copy).

**Requirements**:
- Must implement `Clone`
- All fields must be `Copy`
- No custom `Drop` implementation

**Example**:
```kraken
struct Point {
    x: int,
    y: int,
}

impl Clone for Point {
    fn clone(self) -> Point {
        return Point { x: self.x, y: self.y };
    }
}

impl Copy for Point {}

// Usage - implicit copy
let p1 = Point { x: 10, y: 20 };
let p2 = p1;  // p1 is copied, not moved
```

### Debug - Debug Formatting

```kraken
trait Debug {
    fn fmt(self, formatter: Formatter) -> Result<void, Error>;
}
```

**Purpose**: Format values for debugging output.

**Example**:
```kraken
struct Point {
    x: int,
    y: int,
}

impl Debug for Point {
    fn fmt(self, f: Formatter) -> Result<void, Error> {
        return write(f, "Point {{ x: {}, y: {} }}", self.x, self.y);
    }
}

// Usage
let p = Point { x: 10, y: 20 };
println("{:?}", p);  // Output: Point { x: 10, y: 20 }
```

### Display - User-Facing Formatting

```kraken
trait Display {
    fn fmt(self, formatter: Formatter) -> Result<void, Error>;
}
```

**Purpose**: Format values for user-facing output.

**Example**:
```kraken
struct Point {
    x: int,
    y: int,
}

impl Display for Point {
    fn fmt(self, f: Formatter) -> Result<void, Error> {
        return write(f, "({}, {})", self.x, self.y);
    }
}

// Usage
let p = Point { x: 10, y: 20 };
println("{}", p);  // Output: (10, 20)
```

### Default - Default Values

```kraken
trait Default {
    fn default() -> Self;
}
```

**Purpose**: Provide default values for types.

**Example**:
```kraken
struct Point {
    x: int,
    y: int,
}

impl Default for Point {
    fn default() -> Point {
        return Point { x: 0, y: 0 };
    }
}

// Usage
let p = Point::default();  // Point { x: 0, y: 0 }
```

### Drop - Custom Cleanup (RAII)

```kraken
trait Drop {
    fn drop(mut self) -> void;
}
```

**Purpose**: Custom cleanup when value goes out of scope.

**Example**:
```kraken
struct File {
    handle: int,
}

impl Drop for File {
    fn drop(mut self) -> void {
        close_file(self.handle);
    }
}

// Usage - automatic cleanup
{
    let f = File { handle: open_file("data.txt") };
    // Use file...
}  // f.drop() called automatically here
```

## Operator Overloading Traits

### Arithmetic Operators

```kraken
trait Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

trait Sub<Rhs = Self> {
    type Output;
    fn sub(self, rhs: Rhs) -> Self::Output;
}

trait Mul<Rhs = Self> {
    type Output;
    fn mul(self, rhs: Rhs) -> Self::Output;
}

trait Div<Rhs = Self> {
    type Output;
    fn div(self, rhs: Rhs) -> Self::Output;
}

trait Rem<Rhs = Self> {
    type Output;
    fn rem(self, rhs: Rhs) -> Self::Output;
}

trait Neg {
    type Output;
    fn neg(self) -> Self::Output;
}
```

**Example**:
```kraken
struct Point {
    x: int,
    y: int,
}

impl Add for Point {
    type Output = Point;
    
    fn add(self, rhs: Point) -> Point {
        return Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        };
    }
}

// Usage
let p1 = Point { x: 10, y: 20 };
let p2 = Point { x: 5, y: 15 };
let p3 = p1 + p2;  // Desugars to: p1.add(p2)
```

### Bitwise Operators

```kraken
trait BitAnd<Rhs = Self> {
    type Output;
    fn bitand(self, rhs: Rhs) -> Self::Output;
}

trait BitOr<Rhs = Self> {
    type Output;
    fn bitor(self, rhs: Rhs) -> Self::Output;
}

trait BitXor<Rhs = Self> {
    type Output;
    fn bitxor(self, rhs: Rhs) -> Self::Output;
}

trait Not {
    type Output;
    fn not(self) -> Self::Output;
}

trait Shl<Rhs = Self> {
    type Output;
    fn shl(self, rhs: Rhs) -> Self::Output;
}

trait Shr<Rhs = Self> {
    type Output;
    fn shr(self, rhs: Rhs) -> Self::Output;
}
```

### Comparison Operators

```kraken
trait PartialEq<Rhs = Self> {
    fn eq(self, other: Rhs) -> bool;
    
    fn ne(self, other: Rhs) -> bool {
        return !self.eq(other);
    }
}

trait Eq: PartialEq<Self> {
    // Marker trait - requires reflexivity, symmetry, transitivity
}

trait PartialOrd<Rhs = Self>: PartialEq<Rhs> {
    fn partial_cmp(self, other: Rhs) -> Option<Ordering>;
    
    fn lt(self, other: Rhs) -> bool {
        match self.partial_cmp(other) {
            Some(Ordering::Less) => true,
            _ => false,
        }
    }
    
    fn le(self, other: Rhs) -> bool;
    fn gt(self, other: Rhs) -> bool;
    fn ge(self, other: Rhs) -> bool;
}

trait Ord: Eq + PartialOrd<Self> {
    fn cmp(self, other: Self) -> Ordering;
}
```

**Example**:
```kraken
struct Point {
    x: int,
    y: int,
}

impl PartialEq for Point {
    fn eq(self, other: Point) -> bool {
        return self.x == other.x && self.y == other.y;
    }
}

impl Eq for Point {}

// Usage
let p1 = Point { x: 10, y: 20 };
let p2 = Point { x: 10, y: 20 };
if (p1 == p2) {  // Desugars to: p1.eq(p2)
    puts("Points are equal");
}
```

### Indexing Operators

```kraken
trait Index<Idx> {
    type Output;
    fn index(self, index: Idx) -> Self::Output;
}

trait IndexMut<Idx>: Index<Idx> {
    fn index_mut(mut self, index: Idx) -> mut Self::Output;
}
```

**Example**:
```kraken
struct Matrix {
    data: VecInt,
    rows: int,
    cols: int,
}

impl Index<int> for Matrix {
    type Output = int;
    
    fn index(self, idx: int) -> int {
        return self.data[idx];
    }
}

// Usage
let m = Matrix { /* ... */ };
let value = m[5];  // Desugars to: m.index(5)
```

### Dereference Operators

```kraken
trait Deref {
    type Target;
    fn deref(self) -> Self::Target;
}

trait DerefMut: Deref {
    fn deref_mut(mut self) -> mut Self::Target;
}
```

## Conversion Traits

### From and Into - Infallible Conversions

```kraken
trait From<T> {
    fn from(value: T) -> Self;
}

trait Into<T> {
    fn into(self) -> T;
}

// Automatic Into implementation from From
impl<T, U> Into<U> for T where U: From<T> {
    fn into(self) -> U {
        return U::from(self);
    }
}
```

**Example**:
```kraken
struct Meters(int);
struct Kilometers(int);

impl From<Kilometers> for Meters {
    fn from(km: Kilometers) -> Meters {
        return Meters(km.0 * 1000);
    }
}

// Usage
let km = Kilometers(5);
let m: Meters = km.into();  // or Meters::from(km)
```

### TryFrom and TryInto - Fallible Conversions

```kraken
trait TryFrom<T> {
    type Error;
    fn try_from(value: T) -> Result<Self, Self::Error>;
}

trait TryInto<T> {
    type Error;
    fn try_into(self) -> Result<T, Self::Error>;
}
```

**Example**:
```kraken
struct PositiveInt(int);

impl TryFrom<int> for PositiveInt {
    type Error = str;
    
    fn try_from(value: int) -> Result<PositiveInt, str> {
        if (value > 0) {
            return Ok(PositiveInt(value));
        } else {
            return Err("Value must be positive");
        }
    }
}

// Usage
let result = PositiveInt::try_from(-5);
match result {
    Ok(pos) => puts("Valid positive integer"),
    Err(e) => puts(e),
}
```

### AsRef and AsMut - Cheap Reference Conversions

```kraken
trait AsRef<T> {
    fn as_ref(self) -> T;
}

trait AsMut<T> {
    fn as_mut(mut self) -> mut T;
}
```

## Iterator Traits

### Iterator - Core Iterator Trait

```kraken
trait Iterator {
    type Item;
    
    fn next(mut self) -> Option<Self::Item>;
    
    // Provided methods
    fn map<B, F>(self, f: F) -> Map<Self, F>
    where
        F: FnMut(Self::Item) -> B
    {
        // Implementation
    }
    
    fn filter<P>(self, predicate: P) -> Filter<Self, P>
    where
        P: FnMut(Self::Item) -> bool
    {
        // Implementation
    }
    
    fn fold<B, F>(self, init: B, f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B
    {
        // Implementation
    }
    
    fn collect<B>(self) -> B
    where
        B: FromIterator<Self::Item>
    {
        // Implementation
    }
}
```

### IntoIterator - Convert to Iterator

```kraken
trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;
    
    fn into_iter(self) -> Self::IntoIter;
}
```

**Example**:
```kraken
struct Range {
    start: int,
    end: int,
}

impl Iterator for Range {
    type Item = int;
    
    fn next(mut self) -> Option<int> {
        if (self.start < self.end) {
            let value = self.start;
            self.start = self.start + 1;
            return Some(value);
        } else {
            return None;
        }
    }
}

// Usage
let range = Range { start: 0, end: 5 };
for i in range {
    println(i);
}
```

## Trait Bounds

### Generic Functions with Trait Bounds

```kraken
// Single trait bound
fn print_debug<T: Debug>(value: T) -> void {
    println("{:?}", value);
}

// Multiple trait bounds
fn compare_and_print<T: Ord + Debug>(a: T, b: T) -> void {
    if (a < b) {
        println("{:?} is less than {:?}", a, b);
    }
}

// Where clauses for complex bounds
fn complex_function<T, U>(t: T, u: U) -> void
where
    T: Clone + Debug + PartialEq,
    U: Display + Default
{
    // Implementation
}
```

### Generic Structs with Trait Bounds

```kraken
struct Container<T: Clone> {
    value: T,
}

impl<T: Clone> Container<T> {
    fn duplicate(self) -> T {
        return self.value.clone();
    }
}
```

## Implementation Patterns

### Trait Inheritance

```kraken
trait Animal {
    fn make_sound(self) -> str;
}

trait Dog: Animal {
    fn wag_tail(self) -> void;
}

// Must implement both Animal and Dog
impl Dog for GoldenRetriever {
    fn make_sound(self) -> str {
        return "Woof!";
    }
    
    fn wag_tail(self) -> void {
        puts("*wags tail*");
    }
}
```

### Associated Types

```kraken
trait Container {
    type Item;
    
    fn get(self, index: int) -> Option<Self::Item>;
}

impl Container for VecInt {
    type Item = int;
    
    fn get(self, index: int) -> Option<int> {
        // Implementation
    }
}
```

### Default Implementations

```kraken
trait Greet {
    fn name(self) -> str;
    
    fn greet(self) -> str {
        return "Hello, " + self.name() + "!";
    }
}

struct Person {
    name: str,
}

impl Greet for Person {
    fn name(self) -> str {
        return self.name;
    }
    // greet() uses default implementation
}
```

## Standard Library Trait Usage

### Vec Methods with Traits

```kraken
impl<T> Vec<T> {
    fn map<U, F>(self, f: F) -> Vec<U>
    where
        F: FnMut(T) -> U
    {
        // Transform each element
    }
    
    fn filter<P>(self, predicate: P) -> Vec<T>
    where
        P: FnMut(T) -> bool
    {
        // Keep elements matching predicate
    }
    
    fn fold<U, F>(self, init: U, f: F) -> U
    where
        F: FnMut(U, T) -> U
    {
        // Reduce to single value
    }
}
```

### Option Methods with Traits

```kraken
impl<T> Option<T> {
    fn map<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> U
    {
        match self {
            Some(x) => Some(f(x)),
            None => None,
        }
    }
    
    fn and_then<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> Option<U>
    {
        match self {
            Some(x) => f(x),
            None => None,
        }
    }
}
```

### Result Methods with Traits

```kraken
impl<T, E> Result<T, E> {
    fn map<U, F>(self, f: F) -> Result<U, E>
    where
        F: FnOnce(T) -> U
    {
        match self {
            Ok(x) => Ok(f(x)),
            Err(e) => Err(e),
        }
    }
    
    fn map_err<F, O>(self, f: O) -> Result<T, F>
    where
        O: FnOnce(E) -> F
    {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(f(e)),
        }
    }
}
```

## Best Practices

### 1. Use Trait Bounds Appropriately

```kraken
// Good: Specific trait bounds
fn process<T: Clone + Debug>(value: T) -> void {
    let copy = value.clone();
    println("{:?}", copy);
}

// Avoid: Overly restrictive bounds
fn process<T: Clone + Debug + Display + Eq + Ord>(value: T) -> void {
    // Only uses Clone and Debug
}
```

### 2. Prefer Small, Focused Traits

```kraken
// Good: Single responsibility
trait Drawable {
    fn draw(self) -> void;
}

trait Movable {
    fn move_to(mut self, x: int, y: int) -> void;
}

// Avoid: Kitchen sink trait
trait GameObject {
    fn draw(self) -> void;
    fn update(mut self) -> void;
    fn collide(self, other: GameObject) -> bool;
    fn serialize(self) -> str;
    // ... many more methods
}
```

### 3. Use Associated Types for Output Types

```kraken
// Good: Associated type
trait Container {
    type Item;
    fn get(self, index: int) -> Self::Item;
}

// Less flexible: Generic parameter
trait Container<T> {
    fn get(self, index: int) -> T;
}
```

### 4. Implement Standard Traits When Appropriate

```kraken
struct Point {
    x: int,
    y: int,
}

// Implement common traits for better integration
impl Clone for Point { /* ... */ }
impl Copy for Point {}
impl Debug for Point { /* ... */ }
impl PartialEq for Point { /* ... */ }
impl Eq for Point {}
```

## Future Extensions

- **Trait objects** for dynamic dispatch
- **Higher-ranked trait bounds** (HRTBs)
- **Negative trait bounds** (`T: !Send`)
- **Specialization** for optimized implementations
- **Const traits** for compile-time evaluation

## Resources

- **Examples**: `examples/traits/`
- **Tests**: `tests/traits/`
- **Standard Library**: `stdlib/traits/`

---

**Note**: This document specifies the trait system design. Full compiler implementation is in progress.
