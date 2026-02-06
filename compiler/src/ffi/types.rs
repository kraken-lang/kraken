#![allow(dead_code)]

/// C `int` type marker for FFI boundary declarations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CInt;

/// C `size_t` type marker (unsigned, pointer-width).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SizeT;

/// Unsigned pointer-width integer (`usize`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct USize;

/// Signed pointer-width integer (`isize`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ISize;

/// Opaque `void*` pointer for FFI.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VoidPtr;

/// C `FILE*` pointer for stdio operations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FilePtr;

/// Whether an FFI pointer may be null.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Nullability {
    NonNull,
    Nullable,
}

/// Ownership semantics for FFI pointers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ownership {
    Borrowed,
    Owned,
}

/// How a C function signals errors via its return value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrnoConvention {
    None,
    ReturnsNegOne,
    ReturnsNull,
}
