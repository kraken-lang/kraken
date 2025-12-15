#![allow(dead_code)]

pub type CInt = i32;
pub type SizeT = usize;
pub type USize = usize;
pub type ISize = isize;

pub type VoidPtr = *mut core::ffi::c_void;

pub enum OpaqueFile {}

pub type FilePtr = *mut OpaqueFile;

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
