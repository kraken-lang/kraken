#![allow(dead_code)]

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CInt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SizeT;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct USize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ISize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VoidPtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FilePtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Nullability {
    NonNull,
    Nullable,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ownership {
    Borrowed,
    Owned,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrnoConvention {
    None,
    ReturnsNegOne,
    ReturnsNull,
}
