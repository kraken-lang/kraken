use crate::ffi::types::{ErrnoConvention, Nullability, Ownership};
use crate::parser::ast::Type;

use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AbiType {
    Void,
    I32,
    I64,
    I8Ptr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CIntWidening {
    Signed,
    #[allow(dead_code)]
    Unsigned,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct StdlibFnSig {
    pub name: &'static str,
    pub kraken_params: &'static [Type],
    pub kraken_return: Type,
    pub is_vararg: bool,
    pub c_abi_params: &'static [AbiType],
    pub c_abi_param_nullability: &'static [Nullability],
    pub c_abi_param_ownership: &'static [Ownership],
    pub c_abi_return: AbiType,
    pub c_abi_return_nullability: Nullability,
    pub c_abi_return_ownership: Ownership,
    pub errno: ErrnoConvention,
    pub c_int_widening: Option<CIntWidening>,
}

pub fn stdlib_functions() -> &'static [StdlibFnSig] {
    &[
        StdlibFnSig {
            name: "printf",
            kraken_params: &[Type::String],
            kraken_return: Type::Int,
            is_vararg: true,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "puts",
            kraken_params: &[Type::String],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: Some(CIntWidening::Signed),
        },
        // Memory allocation
        StdlibFnSig {
            name: "malloc",
            kraken_params: &[Type::Int],
            kraken_return: Type::Bytes,
            is_vararg: false,
            c_abi_params: &[AbiType::I64],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::Nullable,
            c_abi_return_ownership: Ownership::Owned,
            errno: ErrnoConvention::ReturnsNull,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "free",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Void,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::Nullable],
            c_abi_param_ownership: &[Ownership::Owned],
            c_abi_return: AbiType::Void,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "realloc",
            kraken_params: &[Type::Bytes, Type::Int],
            kraken_return: Type::Bytes,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I64],
            c_abi_param_nullability: &[Nullability::Nullable, Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Owned, Ownership::Borrowed],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::Nullable,
            c_abi_return_ownership: Ownership::Owned,
            errno: ErrnoConvention::ReturnsNull,
            c_int_widening: None,
        },
        // String / memory primitives
        StdlibFnSig {
            name: "getenv",
            kraken_params: &[Type::String],
            kraken_return: Type::String,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::Nullable,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNull,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "setenv",
            kraken_params: &[Type::String, Type::String, Type::Int],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I32],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "unsetenv",
            kraken_params: &[Type::String],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        // File I/O (FILE* modeled as opaque pointer)
        StdlibFnSig {
            name: "fopen",
            kraken_params: &[Type::String, Type::String],
            kraken_return: Type::Bytes,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull, Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed, Ownership::Borrowed],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::Nullable,
            c_abi_return_ownership: Ownership::Owned,
            errno: ErrnoConvention::ReturnsNull,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "fclose",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Owned],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "fread",
            kraken_params: &[Type::Bytes, Type::Int, Type::Int, Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64, AbiType::I8Ptr],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I64,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "fwrite",
            kraken_params: &[Type::Bytes, Type::Int, Type::Int, Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I64, AbiType::I8Ptr],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I64,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "fgets",
            kraken_params: &[Type::Bytes, Type::Int, Type::Bytes],
            kraken_return: Type::Bytes,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I8Ptr],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::Nullable,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "fputs",
            kraken_params: &[Type::String, Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull, Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed, Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "fflush",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "fgetc",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "fputc",
            kraken_params: &[Type::Int, Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I32, AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull, Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed, Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "fseek",
            kraken_params: &[Type::Bytes, Type::Int, Type::Int],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I64, AbiType::I32],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "ftell",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I64,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "feof",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "ferror",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "remove",
            kraken_params: &[Type::String],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "rename",
            kraken_params: &[Type::String, Type::String],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull, Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed, Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::ReturnsNegOne,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "rewind",
            kraken_params: &[Type::Bytes],
            kraken_return: Type::Void,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::Void,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        // Timing
        StdlibFnSig {
            name: "sleep",
            kraken_params: &[Type::Int],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I32],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "strlen",
            kraken_params: &[Type::String],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed],
            c_abi_return: AbiType::I64,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "strcmp",
            kraken_params: &[Type::String, Type::String],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr],
            c_abi_param_nullability: &[Nullability::NonNull, Nullability::NonNull],
            c_abi_param_ownership: &[Ownership::Borrowed, Ownership::Borrowed],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: Some(CIntWidening::Signed),
        },
        StdlibFnSig {
            name: "memcpy",
            kraken_params: &[Type::Bytes, Type::Bytes, Type::Int],
            kraken_return: Type::Bytes,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I64],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "memmove",
            kraken_params: &[Type::Bytes, Type::Bytes, Type::Int],
            kraken_return: Type::Bytes,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I64],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "memset",
            kraken_params: &[Type::Bytes, Type::Int, Type::Int],
            kraken_return: Type::Bytes,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I32, AbiType::I64],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I8Ptr,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: None,
        },
        StdlibFnSig {
            name: "memcmp",
            kraken_params: &[Type::Bytes, Type::Bytes, Type::Int],
            kraken_return: Type::Int,
            is_vararg: false,
            c_abi_params: &[AbiType::I8Ptr, AbiType::I8Ptr, AbiType::I64],
            c_abi_param_nullability: &[
                Nullability::NonNull,
                Nullability::NonNull,
                Nullability::NonNull,
            ],
            c_abi_param_ownership: &[
                Ownership::Borrowed,
                Ownership::Borrowed,
                Ownership::Borrowed,
            ],
            c_abi_return: AbiType::I32,
            c_abi_return_nullability: Nullability::NonNull,
            c_abi_return_ownership: Ownership::Borrowed,
            errno: ErrnoConvention::None,
            c_int_widening: Some(CIntWidening::Signed),
        },
    ]
}

pub fn stdlib_sig(name: &str) -> Option<&'static StdlibFnSig> {
    static MAP: OnceLock<HashMap<&'static str, &'static StdlibFnSig>> = OnceLock::new();

    let map = MAP.get_or_init(|| {
        let mut m = HashMap::with_capacity(stdlib_functions().len());
        for sig in stdlib_functions() {
            m.insert(sig.name, sig);
        }
        m
    });

    map.get(name).copied()
}

pub fn validate_stdlib_table() -> std::result::Result<(), String> {
    for sig in stdlib_functions() {
        let params_len = sig.c_abi_params.len();
        if sig.c_abi_param_nullability.len() != params_len {
            return Err(format!(
                "StdlibFnSig {}: c_abi_param_nullability len mismatch (expected {}, got {})",
                sig.name,
                params_len,
                sig.c_abi_param_nullability.len()
            ));
        }
        if sig.c_abi_param_ownership.len() != params_len {
            return Err(format!(
                "StdlibFnSig {}: c_abi_param_ownership len mismatch (expected {}, got {})",
                sig.name,
                params_len,
                sig.c_abi_param_ownership.len()
            ));
        }

        if sig.c_abi_return == AbiType::I32
            && sig.kraken_return == Type::Int
            && sig.c_int_widening.is_none()
        {
            return Err(format!(
                "StdlibFnSig {}: c_int_widening must be explicit for c_abi_return=I32 and kraken_return=Int",
                sig.name
            ));
        }

        if sig.is_vararg && sig.c_abi_params.is_empty() {
            return Err(format!(
                "StdlibFnSig {}: vararg function must have at least one fixed parameter",
                sig.name
            ));
        }

        if sig.c_int_widening.is_some() {
            if sig.c_abi_return != AbiType::I32 {
                return Err(format!(
                    "StdlibFnSig {}: c_int_widening requires c_abi_return=I32",
                    sig.name
                ));
            }
            if sig.kraken_return != Type::Int {
                return Err(format!(
                    "StdlibFnSig {}: c_int_widening requires kraken_return=Int",
                    sig.name
                ));
            }
        }
    }

    Ok(())
}
