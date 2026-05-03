use crate::ffi::types::{ErrnoConvention, Nullability, Ownership};
use crate::parser::ast::Type;

/// C ABI type used at the LLVM codegen boundary.
///
/// Represents the actual C type that appears in the compiled function signature,
/// which may differ from the Kraken-side type (e.g. `int` in C vs `Int` in Kraken).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AbiType {
    /// `void` return type.
    Void,
    /// C `int` (32-bit signed).
    I32,
    /// C `int64_t` / Kraken `int` (64-bit signed).
    I64,
    /// `char*` / `void*` — any pointer-width pointer.
    I8Ptr,
}

/// How a C function that returns a 32-bit integer should be widened to the
/// Kraken 64-bit integer type after the call.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CIntWidening {
    /// Sign-extend the 32-bit result to 64 bits (`sext`).
    Signed,
    /// Zero-extend the 32-bit result to 64 bits (`zext`).
    Unsigned,
}

/// Canonical FFI signature for a single stdlib or runtime function.
///
/// Drives both the type checker (via `kraken_params` / `kraken_return`) and
/// the LLVM codegen (via `c_abi_params` / `c_abi_return` / `is_vararg`).
/// `c_int_widening` is applied when the C return is `I32` but Kraken expects `Int`.
pub struct StdlibFnSig {
    /// The name used at Kraken call sites (also the LLVM function name).
    pub name: &'static str,
    /// Kraken-side parameter types registered in the type environment.
    pub kraken_params: &'static [Type],
    /// Kraken-side return type registered in the type environment.
    pub kraken_return: Type,
    /// C ABI parameter types used to build the LLVM function type.
    pub c_abi_params: &'static [AbiType],
    /// C ABI return type used to build the LLVM function type.
    pub c_abi_return: AbiType,
    /// Whether the C function is variadic (e.g. `printf`).
    pub is_vararg: bool,
    /// If the C function returns `int` (I32) but Kraken treats it as `Int`,
    /// widen the result to i64 after the call.
    pub c_int_widening: Option<CIntWidening>,
    /// Whether the C return pointer can be null.
    pub c_abi_return_nullability: Nullability,
    /// Who owns the returned pointer (caller must free if `Owned`).
    pub c_abi_return_ownership: Ownership,
    /// How the function signals errors via its return value.
    pub errno: ErrnoConvention,
}

/// All stdlib and runtime functions exposed through the table-driven path.
///
/// Both the type checker and LLVM codegen iterate this slice. Every function
/// listed here is declared in the LLVM module via `declare_stdlib_functions`
/// and registered in the type environment during type-checker initialisation.
///
/// Functions that are declared manually in `declare_stdlib_functions` (e.g.
/// math, string libc wrappers, pthreads) are NOT listed here to avoid duplicate
/// LLVM declarations.
pub fn stdlib_functions() -> &'static [StdlibFnSig] {
    &STDLIB_TABLE
}

/// Look up the signature for a single function by name.
///
/// Returns `None` if the function is not in the table (i.e. it is either a
/// user-defined function or declared via the manual inline path).
pub fn stdlib_sig(name: &str) -> Option<&'static StdlibFnSig> {
    STDLIB_TABLE.iter().find(|s| s.name == name)
}

/// Validate that the stdlib table is internally consistent.
///
/// Checks that no two entries share the same name and that every entry with
/// `c_int_widening` set also has `c_abi_return == AbiType::I32`.
/// Called at compiler startup so misconfigurations surface immediately.
///
/// # Errors
/// Returns a descriptive `String` on the first inconsistency found.
pub fn validate_stdlib_table() -> Result<(), String> {
    let table = stdlib_functions();
    for (i, sig) in table.iter().enumerate() {
        for (j, other) in table.iter().enumerate() {
            if i != j && sig.name == other.name {
                return Err(format!("stdlib table: duplicate entry for '{}'", sig.name));
            }
        }
        if sig.c_int_widening.is_some() && sig.c_abi_return != AbiType::I32 {
            return Err(format!(
                "stdlib table: '{}' has c_int_widening but c_abi_return is not I32",
                sig.name
            ));
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Table
// ---------------------------------------------------------------------------

static STDLIB_TABLE: &[StdlibFnSig] = &[
    // -----------------------------------------------------------------------
    // Core I/O
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "puts",
        kraken_params: &[Type::String],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I32,
        is_vararg: false,
        c_int_widening: Some(CIntWidening::Signed),
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "printf",
        kraken_params: &[Type::String],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I32,
        is_vararg: true,
        c_int_widening: Some(CIntWidening::Signed),
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "strlen",
        kraken_params: &[Type::String],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    // -----------------------------------------------------------------------
    // Core file I/O (FILE* modeled as Bytes / i8*)
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "fopen",
        kraken_params: &[Type::String, Type::String],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "fputs",
        kraken_params: &[Type::String, Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "fclose",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "fread",
        kraken_params: &[Type::Bytes, Type::Int, Type::Int, Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64, AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "fwrite",
        kraken_params: &[Type::Bytes, Type::Int, Type::Int, Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64, AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    // -----------------------------------------------------------------------
    // Memory
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "malloc",
        kraken_params: &[Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "free",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Void,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::Void,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "memcpy",
        kraken_params: &[Type::Bytes, Type::Bytes, Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    // -----------------------------------------------------------------------
    // Process control
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "system",
        kraken_params: &[Type::String],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I32,
        is_vararg: false,
        c_int_widening: Some(CIntWidening::Signed),
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    // -----------------------------------------------------------------------
    // Kraken runtime: print helpers (kraken_stdlib.c / kraken_safety.c)
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "kraken_fseek",
        kraken_params: &[Type::Bytes, Type::Int, Type::Int],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "kraken_ftell",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "kraken_rewind",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Void,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::Void,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_feof",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_ferror",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_fopen",
        kraken_params: &[Type::String, Type::String],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "kraken_fclose",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "kraken_fread",
        kraken_params: &[Type::Bytes, Type::Int, Type::Int, Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64, AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_fwrite",
        kraken_params: &[Type::Bytes, Type::Int, Type::Int, Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64, AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_time",
        kraken_params: &[],
        kraken_return: Type::Int,
        c_abi_params: &[],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_clock",
        kraken_params: &[],
        kraken_return: Type::Int,
        c_abi_params: &[],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_clocks_per_sec",
        kraken_params: &[],
        kraken_return: Type::Int,
        c_abi_params: &[],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_difftime",
        kraken_params: &[Type::Int, Type::Int],
        kraken_return: Type::Float,
        c_abi_params: &[AbiType::I64, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_strftime",
        kraken_params: &[Type::String, Type::Int],
        kraken_return: Type::String,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "kraken_localtime",
        kraken_params: &[Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "kraken_calloc",
        kraken_params: &[Type::Int, Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I64, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "kraken_aligned_alloc",
        kraken_params: &[Type::Int, Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I64, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "realloc",
        kraken_params: &[Type::Bytes, Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "kraken_realloc",
        kraken_params: &[Type::Bytes, Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::Nullable,
        c_abi_return_ownership: Ownership::Owned,
        errno: ErrnoConvention::ReturnsNull,
    },
    StdlibFnSig {
        name: "kraken_free",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Void,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::Void,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_memcpy",
        kraken_params: &[Type::Bytes, Type::Bytes, Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_memset",
        kraken_params: &[Type::Bytes, Type::Int, Type::Int],
        kraken_return: Type::Bytes,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64],
        c_abi_return: AbiType::I8Ptr,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "kraken_memcmp",
        kraken_params: &[Type::Bytes, Type::Bytes, Type::Int],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I64],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    // -----------------------------------------------------------------------
    // libc stdio positional / status functions called directly by test programs
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "fseek",
        kraken_params: &[Type::Bytes, Type::Int, Type::Int],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "ftell",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    // -----------------------------------------------------------------------
    // libc string / memory functions called directly by test programs
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "strcmp",
        kraken_params: &[Type::String, Type::String],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
        c_abi_return: AbiType::I32,
        is_vararg: false,
        c_int_widening: Some(CIntWidening::Signed),
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "memcmp",
        kraken_params: &[Type::Bytes, Type::Bytes, Type::Int],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I64],
        c_abi_return: AbiType::I32,
        is_vararg: false,
        c_int_widening: Some(CIntWidening::Signed),
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    // -----------------------------------------------------------------------
    // libc stdio extras called directly by test programs
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "rewind",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Void,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::Void,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "fputc",
        kraken_params: &[Type::Int, Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I64, AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "fgetc",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "fflush",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "feof",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "ferror",
        kraken_params: &[Type::Bytes],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "rename",
        kraken_params: &[Type::String, Type::String],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    StdlibFnSig {
        name: "remove",
        kraken_params: &[Type::String],
        kraken_return: Type::Int,
        c_abi_params: &[AbiType::I8Ptr],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::ReturnsNegOne,
    },
    // -----------------------------------------------------------------------
    // Host platform detection — resolved at C compile time via #ifdef macros.
    // Implemented in kraken_stdlib.c; integer return values match krakenc's
    // OS_*() / ARCH_*() constants so krakenc's platform.kr can call them.
    // -----------------------------------------------------------------------
    StdlibFnSig {
        name: "detect_host_os",
        kraken_params: &[],
        kraken_return: Type::Int,
        c_abi_params: &[],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
    StdlibFnSig {
        name: "detect_host_arch",
        kraken_params: &[],
        kraken_return: Type::Int,
        c_abi_params: &[],
        c_abi_return: AbiType::I64,
        is_vararg: false,
        c_int_widening: None,
        c_abi_return_nullability: Nullability::NonNull,
        c_abi_return_ownership: Ownership::Borrowed,
        errno: ErrnoConvention::None,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_stdlib_table_passes() {
        validate_stdlib_table().unwrap();
    }

    #[test]
    fn test_stdlib_sig_found() {
        assert!(stdlib_sig("malloc").is_some());
        assert!(stdlib_sig("free").is_some());
        assert!(stdlib_sig("puts").is_some());
        assert!(stdlib_sig("printf").is_some());
    }

    #[test]
    fn test_stdlib_sig_not_found() {
        assert!(stdlib_sig("not_a_real_function").is_none());
    }

    #[test]
    fn test_no_duplicate_names() {
        let table = stdlib_functions();
        for (i, a) in table.iter().enumerate() {
            for (j, b) in table.iter().enumerate() {
                if i != j {
                    assert_ne!(a.name, b.name, "duplicate entry: {}", a.name);
                }
            }
        }
    }

    #[test]
    fn test_widening_implies_i32_return() {
        for sig in stdlib_functions() {
            if sig.c_int_widening.is_some() {
                assert_eq!(
                    sig.c_abi_return,
                    AbiType::I32,
                    "{}: c_int_widening set but c_abi_return is not I32",
                    sig.name
                );
            }
        }
    }

    #[test]
    fn test_malloc_abi_shape() {
        let sig = stdlib_sig("malloc").unwrap();
        assert_eq!(sig.c_abi_params, &[AbiType::I64]);
        assert_eq!(sig.c_abi_return, AbiType::I8Ptr);
        assert!(!sig.is_vararg);
    }

    #[test]
    fn test_printf_is_vararg() {
        let sig = stdlib_sig("printf").unwrap();
        assert!(sig.is_vararg);
    }
}
