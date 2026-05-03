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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nullability_variants() {
        assert_ne!(Nullability::NonNull, Nullability::Nullable);
        let a = Nullability::NonNull;
        let b = a;
        assert_eq!(a, b);
    }

    #[test]
    fn test_ownership_variants() {
        assert_ne!(Ownership::Borrowed, Ownership::Owned);
        let a = Ownership::Owned;
        let b = a;
        assert_eq!(a, b);
    }

    #[test]
    fn test_errno_convention_variants() {
        assert_ne!(ErrnoConvention::None, ErrnoConvention::ReturnsNegOne);
        assert_ne!(ErrnoConvention::None, ErrnoConvention::ReturnsNull);
        assert_ne!(ErrnoConvention::ReturnsNegOne, ErrnoConvention::ReturnsNull);
    }

    #[test]
    fn test_type_markers_clone_debug() {
        let c = CInt;
        let c2 = c;
        assert_eq!(c, c2);
        assert_eq!(format!("{:?}", c), "CInt");

        let s = SizeT;
        assert_eq!(format!("{:?}", s), "SizeT");

        let u = USize;
        assert_eq!(format!("{:?}", u), "USize");

        let i = ISize;
        assert_eq!(format!("{:?}", i), "ISize");

        let v = VoidPtr;
        assert_eq!(format!("{:?}", v), "VoidPtr");

        let f = FilePtr;
        assert_eq!(format!("{:?}", f), "FilePtr");
    }

    #[test]
    fn test_nullability_debug() {
        assert_eq!(format!("{:?}", Nullability::NonNull), "NonNull");
        assert_eq!(format!("{:?}", Nullability::Nullable), "Nullable");
    }

    #[test]
    fn test_ownership_debug() {
        assert_eq!(format!("{:?}", Ownership::Borrowed), "Borrowed");
        assert_eq!(format!("{:?}", Ownership::Owned), "Owned");
    }

    #[test]
    fn test_errno_debug() {
        assert_eq!(format!("{:?}", ErrnoConvention::None), "None");
        assert_eq!(
            format!("{:?}", ErrnoConvention::ReturnsNegOne),
            "ReturnsNegOne"
        );
        assert_eq!(format!("{:?}", ErrnoConvention::ReturnsNull), "ReturnsNull");
    }
}
