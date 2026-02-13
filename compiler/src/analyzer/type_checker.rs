use super::types::{EnumType, FunctionType, StructType, TraitType, TypeEnvironment};
use crate::error::{CompilerError, CompilerResult, SourceLocation};
use crate::ffi::stdlib::stdlib_functions;
use crate::lexer::token::Operator;
use crate::parser::ast::*;
use std::collections::HashMap;
use std::path::PathBuf;

/// Type checker for Kraken AST.
///
/// Performs semantic analysis and type checking on the parsed AST.
pub struct TypeChecker {
    env: TypeEnvironment,
    file_path: PathBuf,
    current_function_return_type: Option<Type>,
    current_generic_params: Vec<String>,
    method_return_types: HashMap<(String, String), Type>, // (type_name, method_name) -> return type
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new(file_path: PathBuf) -> Self {
        let mut env = TypeEnvironment::new();

        // Add standard library functions
        for sig in stdlib_functions() {
            env.define_function(
                sig.name.to_string(),
                FunctionType {
                    parameter_types: sig.kraken_params.to_vec(),
                    return_type: sig.kraken_return.clone(),
                    is_async: false,
                },
            );
        }

        env.define_function(
            "cstr".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Bytes,
                is_async: false,
            },
        );
        env.define_function(
            "from_cstr".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes],
                return_type: Type::String,
                is_async: false,
            },
        );

        // Concurrency intrinsics
        env.define_function(
            "join".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Handle type
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "join_all".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes], // Array of handles
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "block_on".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Future/handle
                return_type: Type::Int,             // Returns the result
                is_async: false,
            },
        );

        // Mutex intrinsics
        env.define_function(
            "mutex_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Bytes, // Mutex handle
                is_async: false,
            },
        );
        env.define_function(
            "mutex_lock".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Mutex handle
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "mutex_unlock".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Mutex handle
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "mutex_free".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Mutex handle
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Channel intrinsics
        env.define_function(
            "channel_new".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // Capacity
                return_type: Type::Bytes,         // Channel handle
                is_async: false,
            },
        );
        env.define_function(
            "channel_send".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int], // Channel, value
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "channel_recv".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Channel
                return_type: Type::Int,             // Received value
                is_async: false,
            },
        );
        env.define_function(
            "channel_close".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Channel
                return_type: Type::Void,
                is_async: false,
            },
        );

        // AtomicInt intrinsics
        env.define_function(
            "atomic_new".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // Initial value
                return_type: Type::Bytes,         // Atomic handle
                is_async: false,
            },
        );
        env.define_function(
            "atomic_load".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Atomic handle
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "atomic_store".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int], // Atomic, value
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "atomic_add".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int], // Atomic, delta
                return_type: Type::Int,                        // Previous value
                is_async: false,
            },
        );
        env.define_function(
            "atomic_sub".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int], // Atomic, delta
                return_type: Type::Int,                        // Previous value
                is_async: false,
            },
        );
        env.define_function(
            "atomic_cas".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int, Type::Int], // Atomic, expected, new
                return_type: Type::Int, // 1 if swapped, 0 otherwise
                is_async: false,
            },
        );

        // Timing intrinsics
        env.define_function(
            "sleep_ms".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // Milliseconds
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Thread pool intrinsics
        env.define_function(
            "pool_new".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // Number of workers
                return_type: Type::Bytes,         // Pool handle
                is_async: false,
            },
        );
        env.define_function(
            "pool_spawn".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Bytes], // Pool, task function ptr
                return_type: Type::Bytes,                        // Task handle
                is_async: false,
            },
        );
        env.define_function(
            "pool_shutdown".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Pool
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Executor intrinsics
        env.define_function(
            "executor_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Bytes, // Executor handle
                is_async: false,
            },
        );
        env.define_function(
            "executor_spawn".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Bytes], // Executor, future
                return_type: Type::Bytes,                        // Task handle
                is_async: false,
            },
        );
        env.define_function(
            "executor_run".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Executor
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "executor_shutdown".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Executor
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Cancellation intrinsics
        env.define_function(
            "cancel_token_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Bytes, // Token handle
                is_async: false,
            },
        );
        env.define_function(
            "cancel_token_cancel".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Token
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "cancel_token_is_cancelled".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes], // Token
                return_type: Type::Int,             // 1 if cancelled, 0 otherwise
                is_async: false,
            },
        );

        // Timeout intrinsics
        env.define_function(
            "timeout".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int], // Future, milliseconds
                return_type: Type::Int,                        // 0 = completed, 1 = timed out
                is_async: false,
            },
        );

        // VecInt intrinsics
        env.define_function(
            "vec_int_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::VecInt,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_push".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_pop".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_len".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_get".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_set".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt, Type::Int, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_clear".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_free".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // VecString intrinsics
        env.define_function(
            "vec_string_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::VecString,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_push".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString, Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_pop".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_len".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_get".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString, Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_set".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString, Type::Int, Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_clear".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_free".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // VecBytes intrinsics
        env.define_function(
            "vec_bytes_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::VecBytes,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_push".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_pop".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_len".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_get".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_set".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes, Type::Int, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_clear".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_free".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // MapStringInt intrinsics
        env.define_function(
            "map_string_int_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::MapStringInt,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_set".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt, Type::String, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_get".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt, Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_has".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt, Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_delete".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt, Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_len".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_clear".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_free".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // MapStringString intrinsics
        env.define_function(
            "map_string_string_new".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::MapStringString,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_set".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString, Type::String, Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_get".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_has".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString, Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_delete".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString, Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_len".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_clear".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_free".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Map iteration intrinsics
        env.define_function(
            "map_string_int_keys".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt],
                return_type: Type::VecString,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_int_values".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringInt],
                return_type: Type::VecInt,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_keys".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString],
                return_type: Type::VecString,
                is_async: false,
            },
        );
        env.define_function(
            "map_string_string_values".to_string(),
            FunctionType {
                parameter_types: vec![Type::MapStringString],
                return_type: Type::VecString,
                is_async: false,
            },
        );

        // Math stdlib: math_sqrt, math_pow, math_abs, etc.
        env.define_function(
            "math_sqrt".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_pow".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float, Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_abs".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "math_floor".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_ceil".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_round".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_sin".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_cos".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_tan".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "math_min".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "math_max".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );

        // Random stdlib: rand_int, rand_float, rand_seed
        env.define_function(
            "rand_seed".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "rand_int".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "rand_float".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "rand_bytes".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Bytes,
                is_async: false,
            },
        );

        // Log stdlib: log_debug, log_info, log_warn, log_error
        env.define_function(
            "log_debug".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "log_info".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "log_warn".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "log_error".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "log_set_level".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Container capacity APIs
        env.define_function(
            "vec_int_capacity".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_with_capacity".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::VecInt,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_reserve".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_shrink_to_fit".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_reserve".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_shrink_to_fit".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_reserve".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_shrink_to_fit".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_capacity".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_with_capacity".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::VecString,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_capacity".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_with_capacity".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::VecBytes,
                is_async: false,
            },
        );

        // Advanced vec mutation
        env.define_function(
            "vec_int_insert".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt, Type::Int, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_remove".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_int_swap_remove".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecInt, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "vec_string_swap_remove".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString, Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "vec_bytes_swap_remove".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecBytes, Type::Int],
                return_type: Type::Bytes,
                is_async: false,
            },
        );

        // Format stdlib: fmt_int, fmt_float, fmt_bool, fmt_hex
        env.define_function(
            "fmt_int".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "fmt_hex".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "fmt_bool".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bool],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "fmt_float".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float, Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );

        // Test framework: assert, assert_eq, assert_ne
        env.define_function(
            "assert".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bool],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "assert_eq".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "assert_ne".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Test harness helpers
        env.define_function(
            "test_pass".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "test_fail".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "test_skip".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "test_section".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Runtime benchmark helpers
        env.define_function(
            "bench_start".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "bench_end".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::String, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );

        // Threading primitives
        env.define_function(
            "thread_sleep_ms".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "mutex_create".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Int, // Returns mutex handle (pointer as int)
                is_async: false,
            },
        );
        env.define_function(
            "mutex_lock".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "mutex_unlock".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "mutex_destroy".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Channel primitives
        env.define_function(
            "channel_create".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Int, // Channel handle
                is_async: false,
            },
        );
        env.define_function(
            "channel_send".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int], // channel, value
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "channel_recv".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // channel
                return_type: Type::Int,           // value
                is_async: false,
            },
        );
        env.define_function(
            "channel_try_send".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int], // channel, value
                return_type: Type::Bool,                     // success
                is_async: false,
            },
        );
        env.define_function(
            "channel_try_recv".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // channel
                return_type: Type::Int,           // value or 0 if empty
                is_async: false,
            },
        );
        env.define_function(
            "channel_close".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Thread primitives
        env.define_function(
            "thread_spawn".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // Function pointer as int
                return_type: Type::Int,           // Thread handle
                is_async: false,
            },
        );
        env.define_function(
            "thread_join".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // Thread handle
                return_type: Type::Int,           // Return value
                is_async: false,
            },
        );
        env.define_function(
            "thread_detach".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // Thread handle
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Condition variable primitives
        env.define_function(
            "condvar_create".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Int, // Returns condvar handle
                is_async: false,
            },
        );
        env.define_function(
            "condvar_wait".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int], // condvar, mutex
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "condvar_signal".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "condvar_broadcast".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "condvar_destroy".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Thread pool primitives
        env.define_function(
            "pool_new".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // num_threads
                return_type: Type::Int,           // pool handle
                is_async: false,
            },
        );
        env.define_function(
            "pool_spawn".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int, Type::Int], // pool, function pointer
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "pool_shutdown".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int], // pool handle
                return_type: Type::Void,
                is_async: false,
            },
        );

        // String equality intrinsics
        env.define_function(
            "str_eq".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Bool,
                is_async: false,
            },
        );
        env.define_function(
            "str_ne".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Bool,
                is_async: false,
            },
        );
        env.define_function(
            "bytes_eq".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Bytes],
                return_type: Type::Bool,
                is_async: false,
            },
        );

        // String manipulation intrinsics
        env.define_function(
            "str_len".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "str_slice".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::Int, Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_concat".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_char_at".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "str_trim".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_contains".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Bool,
                is_async: false,
            },
        );
        env.define_function(
            "str_starts_with".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Bool,
                is_async: false,
            },
        );
        env.define_function(
            "str_ends_with".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Bool,
                is_async: false,
            },
        );
        env.define_function(
            "str_is_valid_utf8".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Bool,
                is_async: false,
            },
        );
        env.define_function(
            "str_split".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::VecString,
                is_async: false,
            },
        );
        env.define_function(
            "str_join".to_string(),
            FunctionType {
                parameter_types: vec![Type::VecString, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_to_upper".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_to_lower".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_replace".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_index_of".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "str_char_count".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "str_char_at_utf8".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );

        // String functions
        env.define_function(
            "strcpy".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "strcat".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "strstr".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "strchr".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "strncpy".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String, Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "strncmp".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );

        // Memory functions

        // Math functions
        env.define_function(
            "sqrt".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "pow".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float, Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "abs".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "fabs".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "floor".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "ceil".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "round".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "sin".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "cos".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "tan".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "log".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "log10".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "exp".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );

        // Random functions
        env.define_function(
            "rand".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "srand".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Time functions
        env.define_function(
            "time".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );

        // System & Process functions
        env.define_function(
            "exit".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );
        env.define_function(
            "system".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );

        // String conversion functions
        env.define_function(
            "atoi".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "atof".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Float,
                is_async: false,
            },
        );

        // Advanced math functions
        env.define_function(
            "asin".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "acos".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "atan".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "atan2".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float, Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "sinh".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "cosh".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "tanh".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );
        env.define_function(
            "fmod".to_string(),
            FunctionType {
                parameter_types: vec![Type::Float, Type::Float],
                return_type: Type::Float,
                is_async: false,
            },
        );

        // Process execution
        env.define_function(
            "system".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "exit".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Sleep function
        env.define_function(
            "usleep".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );

        // Character classification
        for func in [
            "isalpha", "isdigit", "isalnum", "isspace", "isupper", "islower", "toupper", "tolower",
        ] {
            env.define_function(
                func.to_string(),
                FunctionType {
                    parameter_types: vec![Type::Int],
                    return_type: Type::Int,
                    is_async: false,
                },
            );
        }

        // File I/O (FILE* modeled as Bytes)
        env.define_function(
            "fopen".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Bytes,
                is_async: false,
            },
        );
        env.define_function(
            "fclose".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "fread".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int, Type::Int, Type::Bytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "fwrite".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int, Type::Int, Type::Bytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "fputs".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::Bytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "fseek".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int, Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "ftell".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "getenv".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );

        // File I/O helpers
        env.define_function(
            "file_read_string".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "str_from_char_code".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::String,
                is_async: false,
            },
        );

        // String utilities
        env.define_function(
            "strdup".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );
        env.define_function(
            "strtok".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::String,
                is_async: false,
            },
        );

        // Error handling
        env.define_function(
            "abort".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Void,
                is_async: false,
            },
        );

        // Additional I/O
        env.define_function(
            "putchar".to_string(),
            FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "getchar".to_string(),
            FunctionType {
                parameter_types: vec![],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "sprintf".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "sscanf".to_string(),
            FunctionType {
                parameter_types: vec![Type::String, Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );

        Self {
            env,
            file_path,
            current_function_return_type: None,
            current_generic_params: Vec::new(),
            method_return_types: HashMap::new(),
        }
    }

    fn is_generic_param(&self, name: &str) -> bool {
        self.current_generic_params.iter().any(|p| p == name)
    }

    /// Type check a program.
    ///
    /// # Arguments
    /// * `program` - The AST program to check
    ///
    /// # Returns
    /// Ok if type checking succeeds
    ///
    /// # Errors
    /// Returns `CompilerError::TypeError` if type checking fails
    pub fn check_program(&mut self, program: &Program) -> CompilerResult<()> {
        for statement in &program.statements {
            self.predeclare_statement(statement)?;
        }

        for statement in &program.statements {
            self.check_statement(statement)?;
        }

        Ok(())
    }

    fn predeclare_statement(&mut self, statement: &Statement) -> CompilerResult<()> {
        match statement {
            Statement::Module { .. } => Ok(()),
            Statement::Import { .. } => Ok(()),

            Statement::FunctionDeclaration {
                name,
                generic_params: _,
                parameters,
                return_type,
                is_async,
                ..
            } => {
                if self.env.lookup_function(name).is_some() {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Duplicate function: {name}"),
                    ));
                }

                let param_types: Vec<Type> =
                    parameters.iter().map(|p| p.param_type.clone()).collect();
                let ret_type = return_type.clone().unwrap_or(Type::Void);
                let func_type = FunctionType::new(param_types, ret_type, *is_async);
                self.env.define_function(name.clone(), func_type);
                Ok(())
            }

            Statement::StructDeclaration { name, fields, .. }
            | Statement::ClassDeclaration { name, fields, .. } => {
                if self.env.lookup_struct(name).is_some() {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Duplicate type: {name}"),
                    ));
                }

                let mut field_map = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.clone(), field.field_type.clone());
                }

                let struct_type = StructType::new(field_map);
                self.env.define_struct(name.clone(), struct_type);
                Ok(())
            }

            Statement::EnumDeclaration { name, variants, .. } => {
                if self.env.lookup_enum(name).is_some() {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Duplicate enum: {name}"),
                    ));
                }

                let enum_type = EnumType::new(name.clone(), variants.clone());
                self.env.define_enum(name.clone(), enum_type);
                // Also register as custom type for type checking
                self.env
                    .define_struct(name.clone(), StructType::new(HashMap::new()));
                Ok(())
            }

            Statement::TraitDeclaration {
                name,
                generic_params,
                super_traits,
                methods,
                associated_types,
                ..
            } => {
                if self.env.lookup_trait(name).is_some() {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Duplicate trait: {name}"),
                    ));
                }

                let trait_type = TraitType::new(
                    name.to_string(),
                    generic_params.to_vec(),
                    super_traits.to_vec(),
                    methods.to_vec(),
                    associated_types.to_vec(),
                );
                self.env.define_trait(name.to_string(), trait_type);
                Ok(())
            }

            Statement::ImplBlock {
                type_name, methods, ..
            }
            | Statement::TraitImpl {
                type_name, methods, ..
            } => {
                for method in methods {
                    if let Statement::FunctionDeclaration {
                        name, return_type, ..
                    } = method
                    {
                        let ret = return_type.clone().unwrap_or(Type::Void);
                        self.method_return_types
                            .insert((type_name.clone(), name.clone()), ret);
                    }
                }
                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Type check a statement.
    fn check_statement(&mut self, statement: &Statement) -> CompilerResult<()> {
        match statement {
            Statement::Module { .. } => Ok(()),
            Statement::Import { .. } => Ok(()),

            Statement::VariableDeclaration {
                pattern,
                type_annotation,
                initializer,
                is_mutable: _,
            } => {
                let var_type = if let Some(init) = initializer {
                    let init_type = self.check_expression(init)?;

                    if let Some(annotation) = type_annotation {
                        if !self.types_compatible(annotation, &init_type) {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!("Type mismatch: expected {annotation}, found {init_type}"),
                            ));
                        }
                        annotation.clone()
                    } else {
                        init_type
                    }
                } else if let Some(annotation) = type_annotation {
                    annotation.clone()
                } else {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Variable declaration must have type annotation or initializer",
                    ));
                };

                // Bind pattern to type
                self.bind_pattern(pattern, &var_type)?;
                Ok(())
            }

            Statement::ConstantDeclaration {
                name,
                type_annotation,
                initializer,
            } => {
                let init_type = self.check_expression(initializer)?;

                if let Some(annotation) = type_annotation {
                    if !self.types_compatible(annotation, &init_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Type mismatch: expected {annotation}, found {init_type}"),
                        ));
                    }
                }

                self.env.define_variable(name.clone(), init_type);
                Ok(())
            }

            Statement::FunctionDeclaration {
                name: _,
                generic_params,
                where_constraints: _,
                parameters,
                return_type,
                body,
                is_async: _,
                is_unsafe: _,
                is_public: _,
                is_variadic,
            } => {
                // Validate variadic functions
                if *is_variadic && parameters.is_empty() {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Variadic function must have at least one fixed parameter before '...'",
                    ));
                }

                let ret_type = return_type.clone().unwrap_or(Type::Void);

                let previous_return_type = self.current_function_return_type.clone();
                self.current_function_return_type = Some(ret_type);

                let previous_generic_params = std::mem::take(&mut self.current_generic_params);
                self.current_generic_params = generic_params.clone();

                let mut func_env = self.env.child();
                for param in parameters {
                    // Bind parameter pattern to function environment
                    self.bind_pattern_to_env(&param.pattern, &param.param_type, &mut func_env)?;
                }

                let saved_env = std::mem::replace(&mut self.env, func_env);
                self.check_block(body)?;
                self.env = saved_env;

                self.current_generic_params = previous_generic_params;

                self.current_function_return_type = previous_return_type;
                Ok(())
            }

            Statement::StructDeclaration {
                name,
                generic_params: _,
                where_constraints: _,
                fields,
                is_public: _,
                repr: _,
            } => {
                let mut field_map = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.clone(), field.field_type.clone());
                }

                let struct_type = StructType::new(field_map);
                self.env.define_struct(name.clone(), struct_type);
                Ok(())
            }

            Statement::ClassDeclaration {
                name,
                fields,
                methods,
                is_public: _,
            } => {
                let mut field_map = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.clone(), field.field_type.clone());
                }

                let struct_type = StructType::new(field_map);
                self.env.define_struct(name.clone(), struct_type);

                for method in methods {
                    self.check_statement(method)?;
                }

                Ok(())
            }

            Statement::InterfaceDeclaration { name, methods } => {
                // Validate interface method signatures: check parameter types and return types exist
                for method in methods {
                    for param in &method.parameters {
                        self.validate_type(&param.param_type)?;
                    }
                    if let Some(ret_ty) = &method.return_type {
                        self.validate_type(ret_ty)?;
                    }
                }
                // Register interface name so it can be referenced as a type
                let _ = name;
                Ok(())
            }

            Statement::EnumDeclaration { .. } => {
                // Already registered during predeclaration phase
                Ok(())
            }

            Statement::UnionDeclaration { name, fields, .. } => {
                // Register union as a struct-like type
                let mut field_types = HashMap::new();
                for field in fields {
                    field_types.insert(field.name.clone(), field.field_type.clone());
                }
                self.env
                    .define_struct(name.clone(), StructType::new(field_types));
                Ok(())
            }

            Statement::Return { value } => {
                let return_type = if let Some(expr) = value {
                    self.check_expression(expr)?
                } else {
                    Type::Void
                };

                if let Some(expected) = &self.current_function_return_type {
                    if !self.types_compatible(expected, &return_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!(
                                "Return type mismatch: expected {expected}, found {return_type}"
                            ),
                        ));
                    }
                }

                Ok(())
            }

            Statement::Expression(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("If condition must be bool, found {cond_type}"),
                    ));
                }

                self.check_block(then_branch)?;

                if let Some(else_block) = else_branch {
                    self.check_block(else_block)?;
                }

                Ok(())
            }

            Statement::While { condition, body } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("While condition must be bool, found {cond_type}"),
                    ));
                }

                self.check_block(body)?;
                Ok(())
            }

            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = initializer {
                    self.check_statement(init)?;
                }

                if let Some(cond) = condition {
                    let cond_type = self.check_expression(cond)?;
                    if cond_type != Type::Bool {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("For condition must be bool, found {cond_type}"),
                        ));
                    }
                }

                if let Some(inc) = increment {
                    self.check_expression(inc)?;
                }

                self.check_block(body)?;
                Ok(())
            }

            Statement::ForIn {
                variable,
                iterable,
                body,
            } => {
                // Check that iterable is a valid range expression
                let iterable_type = self.check_expression(iterable)?;

                // For now, we only support ranges as iterables
                // The iterable_type will be Int (from Range expression check)
                if iterable_type != Type::Int {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("For-in loop requires range expression, found {iterable_type}"),
                    ));
                }

                // Create a new scope for the loop body and define the loop variable
                let child_env = self.env.child();
                let saved_env = std::mem::replace(&mut self.env, child_env);

                // Loop variable is always int (from range)
                self.env.define_variable(variable.clone(), Type::Int);

                self.check_block(body)?;

                // Restore environment
                self.env = saved_env;
                Ok(())
            }

            Statement::Match { expression, arms } => {
                let expr_type = self.check_expression(expression)?;

                for arm in arms {
                    // Check pattern compatibility with expression type
                    match &arm.pattern {
                        Pattern::Literal(lit_expr) => {
                            let lit_type = self.check_expression(lit_expr)?;
                            if !self.types_compatible(&expr_type, &lit_type) {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!(
                                        "Pattern type mismatch: expected {expr_type}, found {lit_type}"
                                    ),
                                ));
                            }
                        }
                        Pattern::Identifier(name) => {
                            let mut arm_env = self.env.child();
                            arm_env.define_variable(name.clone(), expr_type.clone());
                            let saved_env = std::mem::replace(&mut self.env, arm_env);
                            self.check_block(&arm.body)?;
                            self.env = saved_env;
                            continue;
                        }
                        Pattern::Wildcard => {}
                        Pattern::Tuple { patterns } => {
                            // Check tuple pattern against tuple type
                            if let Type::Tuple { element_types } = &expr_type {
                                if patterns.len() != element_types.len() {
                                    return Err(CompilerError::type_error(
                                        SourceLocation::new(self.file_path.clone(), 0, 0),
                                        format!(
                                            "Tuple pattern has {} elements but type has {} elements",
                                            patterns.len(),
                                            element_types.len()
                                        ),
                                    ));
                                }
                                // Create child env and bind pattern variables to it BEFORE switching
                                let mut arm_env = self.env.child();
                                for (pat, elem_ty) in patterns.iter().zip(element_types.iter()) {
                                    self.bind_pattern_to_env(pat, elem_ty, &mut arm_env)?;
                                }
                                // Switch to arm env and check body
                                let saved_env = std::mem::replace(&mut self.env, arm_env);
                                self.check_block(&arm.body)?;
                                self.env = saved_env;
                                continue;
                            } else {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Cannot match tuple pattern against non-tuple type: {expr_type}"),
                                ));
                            }
                        }
                        Pattern::EnumVariant {
                            enum_name,
                            variant_name,
                            bindings,
                        } => {
                            // Create child env with bindings using actual payload types
                            let mut arm_env = self.env.child();

                            // Look up the enum and variant to get payload types
                            if let Some(enum_type) = self.env.lookup_enum(enum_name) {
                                // get_variant_payload returns Option<Option<Vec<Type>>>
                                // Outer Option: variant exists? Inner Option: has payload?
                                if let Some(Some(payload)) =
                                    enum_type.get_variant_payload(variant_name)
                                {
                                    // Extract types from payload (tuple or struct)
                                    let payload_types = match payload {
                                        EnumVariantPayload::Tuple(types) => types,
                                        EnumVariantPayload::Struct(fields) => {
                                            fields.iter().map(|(_, ty)| ty.clone()).collect()
                                        }
                                    };

                                    // Bind each variable to its corresponding payload type
                                    for (i, binding) in bindings.iter().enumerate() {
                                        let binding_type =
                                            payload_types.get(i).cloned().unwrap_or(Type::Int);
                                        arm_env.define_variable(binding.clone(), binding_type);
                                    }
                                }
                                // else: variant has no payload, no bindings to define
                            }
                            // else: enum not found, type checker already reported error

                            let saved_env = std::mem::replace(&mut self.env, arm_env);
                            self.check_block(&arm.body)?;
                            self.env = saved_env;
                            continue;
                        }
                        Pattern::Range {
                            start,
                            end,
                            inclusive: _,
                        } => {
                            // Validate range bounds are int type
                            let start_type = self.check_expression(start)?;
                            let end_type = self.check_expression(end)?;
                            if start_type != Type::Int || end_type != Type::Int {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Range pattern requires int bounds, found {start_type}..{end_type}"),
                                ));
                            }
                            // Check that expression type is int
                            if expr_type != Type::Int {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!(
                                        "Range pattern requires int expression, found {expr_type}"
                                    ),
                                ));
                            }
                        }
                        Pattern::Or { patterns } => {
                            // Or patterns: check each alternative matches the expression type
                            for pat in patterns {
                                // Recursively type check each pattern alternative
                                // For now, we just validate they're compatible with expr_type
                                match pat {
                                    Pattern::Literal(lit_expr) => {
                                        let lit_type = self.check_expression(lit_expr)?;
                                        if !self.types_compatible(&expr_type, &lit_type) {
                                            return Err(CompilerError::type_error(
                                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                                format!("Or pattern type mismatch: expected {expr_type}, found {lit_type}"),
                                            ));
                                        }
                                    }
                                    _ => {
                                        // Other pattern types in or patterns are validated recursively
                                    }
                                }
                            }
                        }
                        Pattern::Struct {
                            struct_name,
                            fields,
                            partial,
                        } => {
                            // Check struct pattern against struct type
                            if let Type::Custom(type_name) = &expr_type {
                                if type_name != struct_name {
                                    return Err(CompilerError::type_error(
                                        SourceLocation::new(self.file_path.clone(), 0, 0),
                                        format!("Struct pattern {struct_name} does not match type {type_name}"),
                                    ));
                                }

                                // Look up struct definition and validate fields
                                if let Some(struct_def) = self.env.lookup_struct(struct_name) {
                                    // Create child env and bind pattern variables
                                    let mut arm_env = self.env.child();
                                    for (field_name, field_pattern) in fields {
                                        if let Some(field_type) = struct_def
                                            .fields
                                            .iter()
                                            .find(|(name, _)| name.as_str() == field_name.as_str())
                                            .map(|(_, ty)| ty)
                                        {
                                            self.bind_pattern_to_env(
                                                field_pattern,
                                                field_type,
                                                &mut arm_env,
                                            )?;
                                        } else {
                                            return Err(CompilerError::type_error(
                                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                                format!("Struct {struct_name} has no field named {field_name}"),
                                            ));
                                        }
                                    }

                                    // If not partial, ensure all fields are covered
                                    if !partial && fields.len() != struct_def.fields.len() {
                                        return Err(CompilerError::type_error(
                                            SourceLocation::new(self.file_path.clone(), 0, 0),
                                            format!(
                                                "Struct pattern for {struct_name} must match all {} fields or use .. for partial match",
                                                struct_def.fields.len()
                                            ),
                                        ));
                                    }

                                    // Switch to arm env and check body
                                    let saved_env = std::mem::replace(&mut self.env, arm_env);
                                    self.check_block(&arm.body)?;
                                    self.env = saved_env;
                                    continue;
                                } else {
                                    return Err(CompilerError::type_error(
                                        SourceLocation::new(self.file_path.clone(), 0, 0),
                                        format!("Unknown struct type: {struct_name}"),
                                    ));
                                }
                            } else {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Cannot match struct pattern against non-struct type: {expr_type}"),
                                ));
                            }
                        }
                    }

                    // Check guard clause if present
                    if let Some(guard_expr) = &arm.guard {
                        let guard_type = self.check_expression(guard_expr)?;
                        if guard_type != Type::Bool {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!("Guard clause must be bool, found {guard_type}"),
                            ));
                        }
                    }

                    self.check_block(&arm.body)?;
                }

                // Check exhaustiveness
                if !self.is_match_exhaustive(arms, &expr_type) {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Non-exhaustive match expression for type {expr_type}. Add a wildcard pattern (_) to handle all cases."),
                    ));
                }

                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),

            Statement::Defer { statement } => self.check_statement(statement),

            Statement::Unsafe { block } => {
                for stmt in &block.statements {
                    self.check_statement(stmt)?;
                }
                Ok(())
            }

            Statement::TypeAlias { .. } | Statement::ImplBlock { .. } => {
                // Type aliases and impl blocks are handled separately
                // Type aliases are resolved during type resolution
                // Impl blocks define methods which are type checked when called
                Ok(())
            }

            Statement::TraitDeclaration {
                name,
                generic_params,
                super_traits,
                methods,
                associated_types,
                is_public: _,
            } => self.check_trait_declaration(
                name,
                generic_params,
                super_traits,
                methods,
                associated_types,
            ),
            Statement::TraitImpl {
                trait_name,
                type_name,
                generic_params,
                where_constraints: _,
                methods,
            } => self.check_trait_impl(trait_name, type_name, generic_params, methods),

            Statement::MacroDeclaration { .. }
            | Statement::ConstFunctionDeclaration { .. }
            | Statement::StaticAssert { .. }
            | Statement::Attribute { .. } => Ok(()),
        }
    }

    /// Type check a block.
    fn check_block(&mut self, block: &Block) -> CompilerResult<()> {
        let block_env = self.env.child();
        let saved_env = std::mem::replace(&mut self.env, block_env);

        for statement in &block.statements {
            self.check_statement(statement)?;
        }

        self.env = saved_env;
        Ok(())
    }

    /// Type check an expression and return its type.
    fn check_expression(&mut self, expression: &Expression) -> CompilerResult<Type> {
        match expression {
            Expression::IntLiteral(_) => Ok(Type::Int),
            Expression::FloatLiteral(_) => Ok(Type::Float),
            Expression::StringLiteral(_) => Ok(Type::String),
            Expression::BoolLiteral(_) => Ok(Type::Bool),
            Expression::NullLiteral => Ok(Type::Void),

            Expression::Identifier(name) => self.env.lookup_variable(name).ok_or_else(|| {
                CompilerError::type_error(
                    SourceLocation::new(self.file_path.clone(), 0, 0),
                    format!("Undefined variable: {name}"),
                )
            }),

            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                self.check_binary_operation(&left_type, operator, &right_type)
            }

            Expression::Unary { operator, operand } => {
                let operand_type = self.check_expression(operand)?;
                self.check_unary_operation(operator, &operand_type)
            }

            Expression::Call {
                callee,
                type_args,
                arguments,
            } => {
                if type_args.is_some() {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Generic type arguments are not allowed here (monomorphization expected)"
                            .to_string(),
                    ));
                }
                // Check if it's a function call first (before checking as variable)
                if let Expression::Identifier(func_name) = callee.as_ref() {
                    // First check if it's a declared function
                    if let Some(func_type) = self.env.lookup_function(func_name) {
                        if arguments.len() != func_type.parameter_types.len() {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Function {} expects {} arguments, found {}",
                                    func_name,
                                    func_type.parameter_types.len(),
                                    arguments.len()
                                ),
                            ));
                        }

                        for (i, arg) in arguments.iter().enumerate() {
                            let arg_type = self.check_expression(arg)?;
                            let expected_type = &func_type.parameter_types[i];

                            if !self.types_compatible(expected_type, &arg_type) {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!(
                                        "Function {} argument {} type mismatch: expected {}, found {}",
                                        func_name,
                                        i + 1,
                                        expected_type,
                                        arg_type
                                    ),
                                ));
                            }
                        }

                        // Async functions return a future pointer at the call site
                        if func_type.is_async {
                            return Ok(Type::Bytes);
                        }
                        return Ok(func_type.return_type.clone());
                    }

                    // Check if it's a variable with function type (higher-order function parameter)
                    if let Some(Type::Function {
                        param_types,
                        return_type,
                    }) = self.env.lookup_variable(func_name)
                    {
                        if arguments.len() != param_types.len() {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Function {} expects {} arguments, found {}",
                                    func_name,
                                    param_types.len(),
                                    arguments.len()
                                ),
                            ));
                        }

                        for (i, arg) in arguments.iter().enumerate() {
                            let arg_type = self.check_expression(arg)?;
                            let expected_type = &param_types[i];

                            if !self.types_compatible(expected_type, &arg_type) {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!(
                                        "Function {} argument {} type mismatch: expected {}, found {}",
                                        func_name,
                                        i + 1,
                                        expected_type,
                                        arg_type
                                    ),
                                ));
                            }
                        }

                        return Ok(*return_type.clone());
                    }

                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!(
                            "Undefined function: {func_name}. Known functions: {}",
                            self.env.function_names().join(", ")
                        ),
                    ));
                }

                // Method call: object.method(args)
                if let Expression::MemberAccess { object, member } = callee.as_ref() {
                    // Type-check the receiver and all arguments
                    let obj_type = self.check_expression(object)?;
                    for arg in arguments {
                        self.check_expression(arg)?;
                    }

                    // Look up method return type from impl block registry
                    if let Type::Custom(type_name) = &obj_type {
                        let key = (type_name.clone(), member.clone());
                        if let Some(ret_ty) = self.method_return_types.get(&key) {
                            return Ok(ret_ty.clone());
                        }
                    }

                    // Trait object method resolution: dyn Trait → look up trait method signature
                    if let Type::TraitObject { trait_name, .. } = &obj_type {
                        if let Some(trait_type) = self.env.lookup_trait(trait_name) {
                            for method in &trait_type.methods {
                                if method.name == *member {
                                    return Ok(method.return_type.clone().unwrap_or(Type::Void));
                                }
                            }
                        }
                    }

                    // Fallback: return the object's own type (common for builder patterns)
                    return Ok(obj_type);
                }

                Err(CompilerError::type_error(
                    SourceLocation::new(self.file_path.clone(), 0, 0),
                    "Invalid function call",
                ))
            }

            Expression::Array { elements } => {
                if elements.is_empty() {
                    return Ok(Type::Array {
                        element_type: Box::new(Type::Void),
                        size: Some(0),
                    });
                }

                let first_type = self.check_expression(&elements[0])?;

                for elem in &elements[1..] {
                    let elem_type = self.check_expression(elem)?;
                    if !self.types_compatible(&first_type, &elem_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            "Array elements must have the same type",
                        ));
                    }
                }

                Ok(Type::Array {
                    element_type: Box::new(first_type),
                    size: Some(elements.len()),
                })
            }

            Expression::Index { array, index } => {
                let array_type = self.check_expression(array)?;
                let index_type = self.check_expression(index)?;

                if index_type != Type::Int {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Array index must be int, found {index_type}"),
                    ));
                }

                match array_type {
                    Type::Array { element_type, .. } => Ok(*element_type),
                    Type::Bytes | Type::String => Ok(Type::Int),
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Cannot index non-array type",
                    )),
                }
            }

            Expression::Slice { array, start, end } => {
                let array_type = self.check_expression(array)?;
                let start_type = self.check_expression(start)?;
                let end_type = self.check_expression(end)?;

                if start_type != Type::Int {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Slice start must be int, found {start_type}"),
                    ));
                }
                if end_type != Type::Int {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Slice end must be int, found {end_type}"),
                    ));
                }

                // Slicing returns the same type (string -> string, bytes -> bytes)
                match array_type {
                    Type::String => Ok(Type::String),
                    Type::Bytes => Ok(Type::Bytes),
                    Type::Str => Ok(Type::Str),
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot slice type {array_type}"),
                    )),
                }
            }

            Expression::MemberAccess { object, member } => {
                let object_type = self.check_expression(object)?;

                match object_type {
                    Type::Custom(struct_name) => {
                        if let Some(struct_type) = self.env.lookup_struct(&struct_name) {
                            struct_type.get_field_type(member).cloned().ok_or_else(|| {
                                CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Struct {struct_name} has no field {member}"),
                                )
                            })
                        } else {
                            Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!("Undefined struct: {struct_name}"),
                            ))
                        }
                    }
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Cannot access member of non-struct type",
                    )),
                }
            }

            Expression::StructLiteral {
                name,
                type_args,
                fields,
            } => {
                if type_args.is_some() {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Generic type arguments are not allowed here (monomorphization expected)"
                            .to_string(),
                    ));
                }
                // Look up the struct type
                let struct_type = self.env.lookup_struct(name).ok_or_else(|| {
                    CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Undefined struct: {name}"),
                    )
                })?;

                // Check that all fields are provided and have correct types
                for (field_name, field_expr) in fields {
                    let field_type = struct_type.get_field_type(field_name).ok_or_else(|| {
                        CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Struct {name} has no field {field_name}"),
                        )
                    })?;

                    let expr_type = self.check_expression(field_expr)?;
                    if !self.types_compatible(field_type, &expr_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!(
                                "Field {field_name} expects type {field_type}, found {expr_type}"
                            ),
                        ));
                    }
                }

                Ok(Type::Custom(name.clone()))
            }

            Expression::Assignment { target, value } => {
                let target_type = self.check_expression(target)?;
                let value_type = self.check_expression(value)?;

                if !self.types_compatible(&target_type, &value_type) {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!(
                            "Assignment type mismatch: expected {target_type}, found {value_type}"
                        ),
                    ));
                }

                Ok(value_type)
            }

            Expression::Reference { expression } => {
                // Special case: &function_name returns function pointer as Int
                if let Expression::Identifier(name) = &**expression {
                    // Check for function - either direct match or with module prefix (mangled names)
                    let is_function = self.env.lookup_function(name).is_some()
                        || self
                            .env
                            .function_names()
                            .iter()
                            .any(|n| n.ends_with(&format!("_{name}")));
                    if is_function {
                        return Ok(Type::Int);
                    }
                }
                // Otherwise, regular variable reference
                let inner_type = self.check_expression(expression)?;
                Ok(Type::Reference {
                    inner_type: Box::new(inner_type),
                    is_mutable: false,
                })
            }

            Expression::Dereference { expression } => {
                let expr_type = self.check_expression(expression)?;
                match expr_type {
                    Type::Reference { inner_type, .. } | Type::Pointer { inner_type, .. } => {
                        Ok(*inner_type)
                    }
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Cannot dereference non-reference/pointer type",
                    )),
                }
            }

            Expression::Await { expression } => {
                // await unwraps a future: if the inner expression is a call to an async fn,
                // return the async fn's original declared return type (not the Bytes future ptr)
                if let Expression::Call { callee, .. } = expression.as_ref() {
                    if let Expression::Identifier(func_name) = callee.as_ref() {
                        if let Some(func_type) = self.env.lookup_function(func_name) {
                            if func_type.is_async {
                                // Check the call arguments for side effects
                                let _ = self.check_expression(expression)?;
                                return Ok(func_type.return_type.clone());
                            }
                        }
                    }
                }
                // Fallback: check the inner expression and return Int
                let _ = self.check_expression(expression)?;
                Ok(Type::Int)
            }

            Expression::Spawn { body } => {
                // Spawn returns a handle type (for now, use Bytes as opaque handle)
                // Check the body for type errors
                for stmt in &body.statements {
                    self.check_statement(stmt)?;
                }
                Ok(Type::Bytes) // Handle type placeholder
            }

            Expression::EnumVariant {
                enum_name,
                variant_name,
                payload,
            } => {
                // Verify enum exists and variant is valid
                if let Some(enum_type) = self.env.lookup_enum(enum_name) {
                    if !enum_type.has_variant(variant_name) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Unknown variant '{variant_name}' for enum '{enum_name}'"),
                        ));
                    }
                    // Type check payload if present
                    if let Some(args) = payload {
                        for arg in args {
                            self.check_expression(arg)?;
                        }
                    }
                    // Return the enum type
                    Ok(Type::Custom(enum_name.clone()))
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Unknown enum '{enum_name}'"),
                    ))
                }
            }

            Expression::Tuple { elements } => {
                let mut element_types = Vec::new();
                for elem in elements {
                    element_types.push(self.check_expression(elem)?);
                }
                Ok(Type::Tuple { element_types })
            }

            Expression::TupleIndex { tuple, index } => {
                let tuple_type = self.check_expression(tuple)?;
                match tuple_type {
                    Type::Tuple { element_types } => {
                        if *index < element_types.len() {
                            Ok(element_types[*index].clone())
                        } else {
                            Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Tuple index {} out of bounds (tuple has {} elements)",
                                    index,
                                    element_types.len()
                                ),
                            ))
                        }
                    }
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot index non-tuple type: {tuple_type}"),
                    )),
                }
            }

            Expression::Range {
                start,
                end,
                inclusive: _,
            } => {
                let start_type = self.check_expression(start)?;
                let end_type = self.check_expression(end)?;

                // Both start and end must be int type
                if start_type != Type::Int {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Range start must be int, found {start_type}"),
                    ));
                }
                if end_type != Type::Int {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Range end must be int, found {end_type}"),
                    ));
                }

                // Range expressions don't have a specific type - they're used in for loops
                // For now, we'll just validate them and return int (representing the iterator)
                Ok(Type::Int)
            }

            Expression::Try { expression } => {
                // Type check the inner expression
                let inner_type = self.check_expression(expression)?;

                // The ? operator works on Result<T, E> and Option<T>
                // For Result<T, E>, it returns T and propagates E
                // For Option<T>, it returns T and propagates None
                // Proper error propagation semantics handled during desugaring phase
                Ok(inner_type)
            }

            Expression::Closure {
                parameters,
                return_type,
                body,
                is_move: _,
            } => {
                // Create new environment for closure scope
                let mut closure_env = self.env.clone();

                // Add parameters to closure environment
                let mut param_types = Vec::new();
                for param in parameters {
                    let param_type = param.param_type.clone();
                    param_types.push(param_type.clone());

                    // Bind parameter pattern to environment
                    self.bind_pattern_to_env(&param.pattern, &param_type, &mut closure_env)?;
                }

                // Type check closure body
                let saved_return_type = self.current_function_return_type.clone();
                if let Some(ret_type) = return_type {
                    self.current_function_return_type = Some(ret_type.clone());
                }

                let body_type = match body {
                    ClosureBody::Expression(expr) => {
                        // Temporarily swap environment
                        let old_env = std::mem::replace(&mut self.env, closure_env);
                        let result = self.check_expression(expr);
                        self.env = old_env;
                        result?
                    }
                    ClosureBody::Block(block) => {
                        // Temporarily swap environment
                        let old_env = std::mem::replace(&mut self.env, closure_env);
                        self.check_block(block)?;
                        self.env = old_env;
                        // Blocks return void unless they have an explicit return
                        Type::Void
                    }
                };

                self.current_function_return_type = saved_return_type;

                // Determine return type
                let inferred_return_type = if let Some(ret_type) = return_type {
                    // Block closures with explicit return type use return statements
                    // so body_type == Void is expected
                    if body_type != Type::Void && body_type != *ret_type && *ret_type != Type::Void
                    {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Closure body type {body_type} does not match declared return type {ret_type}"),
                        ));
                    }
                    ret_type.clone()
                } else {
                    // Infer return type from body
                    body_type
                };

                // Return function type
                Ok(Type::Function {
                    param_types,
                    return_type: Box::new(inferred_return_type),
                })
            }
        }
    }

    /// Check binary operation type compatibility.
    fn check_binary_operation(
        &self,
        left: &Type,
        operator: &Operator,
        right: &Type,
    ) -> CompilerResult<Type> {
        match operator {
            Operator::Plus
            | Operator::Minus
            | Operator::Star
            | Operator::Slash
            | Operator::Percent => {
                if (left == &Type::Int || left == &Type::Float)
                    && (right == &Type::Int || right == &Type::Float)
                {
                    if left == right {
                        Ok(left.clone())
                    } else {
                        Ok(Type::Float)
                    }
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Invalid operands for {operator}: {left} and {right}"),
                    ))
                }
            }

            Operator::Equal | Operator::NotEqual => {
                if self.types_compatible(left, right) {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot compare {left} and {right}"),
                    ))
                }
            }

            Operator::Less | Operator::LessEqual | Operator::Greater | Operator::GreaterEqual => {
                if (left == &Type::Int || left == &Type::Float)
                    && (right == &Type::Int || right == &Type::Float)
                {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot compare {left} and {right}"),
                    ))
                }
            }

            Operator::And | Operator::Or => {
                if left == &Type::Bool && right == &Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!(
                            "Logical operators require bool operands, found {left} and {right}"
                        ),
                    ))
                }
            }

            Operator::BitAnd
            | Operator::BitOr
            | Operator::BitXor
            | Operator::LeftShift
            | Operator::RightShift => {
                if left == &Type::Int && right == &Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Bitwise operators require int operands, found {left} and {right}"),
                    ))
                }
            }

            _ => Err(CompilerError::type_error(
                SourceLocation::new(self.file_path.clone(), 0, 0),
                format!("Unsupported binary operator: {operator}"),
            )),
        }
    }

    /// Check unary operation type compatibility.
    fn check_unary_operation(&self, operator: &Operator, operand: &Type) -> CompilerResult<Type> {
        match operator {
            Operator::Minus => {
                if operand == &Type::Int || operand == &Type::Float {
                    Ok(operand.clone())
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot negate {operand}"),
                    ))
                }
            }

            Operator::Not => {
                if operand == &Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Logical not requires bool operand, found {operand}"),
                    ))
                }
            }

            Operator::BitNot => {
                if operand == &Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Bitwise not requires int operand, found {operand}"),
                    ))
                }
            }

            _ => Err(CompilerError::type_error(
                SourceLocation::new(self.file_path.clone(), 0, 0),
                format!("Unsupported unary operator: {operator}"),
            )),
        }
    }

    /// Bind a pattern to a type, defining variables in the environment
    fn bind_pattern(&mut self, pattern: &Pattern, ty: &Type) -> CompilerResult<()> {
        match pattern {
            Pattern::Identifier(name) => {
                self.env.define_variable(name.clone(), ty.clone());
                Ok(())
            }
            Pattern::Wildcard => {
                // Wildcard doesn't bind anything
                Ok(())
            }
            Pattern::Tuple { patterns } => {
                // Destructure tuple type
                match ty {
                    Type::Tuple { element_types } => {
                        if patterns.len() != element_types.len() {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Tuple pattern has {} elements but type has {} elements",
                                    patterns.len(),
                                    element_types.len()
                                ),
                            ));
                        }
                        for (pat, elem_ty) in patterns.iter().zip(element_types.iter()) {
                            self.bind_pattern(pat, elem_ty)?;
                        }
                        Ok(())
                    }
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot destructure non-tuple type: {ty}"),
                    )),
                }
            }
            Pattern::Literal(_) => {
                // Literal patterns don't bind variables
                Ok(())
            }
            Pattern::EnumVariant { .. } => {
                // Enum variant patterns are handled in match expressions
                Ok(())
            }
            Pattern::Range { .. } => {
                // Range patterns don't bind variables
                Ok(())
            }
            Pattern::Or { patterns } => {
                // Or patterns: all alternatives must bind the same variables with the same types
                // For now, we don't bind variables from or patterns (they're typically used with literals)
                // Full implementation would require checking all patterns bind the same vars
                for pat in patterns {
                    self.bind_pattern(pat, ty)?;
                }
                Ok(())
            }
            Pattern::Struct {
                struct_name,
                fields,
                partial,
            } => {
                // Validate struct type and destructure fields
                if let Type::Custom(type_name) = ty {
                    if type_name != struct_name {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Struct pattern {struct_name} does not match type {type_name}"),
                        ));
                    }

                    // Look up struct definition
                    if let Some(struct_def) = self.env.lookup_struct(struct_name) {
                        // Validate all pattern fields exist in struct
                        for (field_name, field_pattern) in fields {
                            if let Some(field_type) = struct_def
                                .fields
                                .iter()
                                .find(|(name, _)| name.as_str() == field_name.as_str())
                                .map(|(_, ty)| ty)
                            {
                                self.bind_pattern(field_pattern, field_type)?;
                            } else {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Struct {struct_name} has no field named {field_name}"),
                                ));
                            }
                        }

                        // If not partial, ensure all fields are covered
                        if !partial && fields.len() != struct_def.fields.len() {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Struct pattern for {struct_name} must match all {} fields or use .. for partial match",
                                    struct_def.fields.len()
                                ),
                            ));
                        }

                        Ok(())
                    } else {
                        Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Unknown struct type: {struct_name}"),
                        ))
                    }
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot destructure non-struct type: {ty}"),
                    ))
                }
            }
        }
    }

    /// Bind a pattern to a type in a specific environment
    fn bind_pattern_to_env(
        &self,
        pattern: &Pattern,
        ty: &Type,
        env: &mut TypeEnvironment,
    ) -> CompilerResult<()> {
        match pattern {
            Pattern::Identifier(name) => {
                env.define_variable(name.clone(), ty.clone());
                Ok(())
            }
            Pattern::Wildcard => {
                // Wildcard doesn't bind anything
                Ok(())
            }
            Pattern::Tuple { patterns } => {
                // Destructure tuple type
                match ty {
                    Type::Tuple { element_types } => {
                        if patterns.len() != element_types.len() {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Tuple pattern has {} elements but type has {} elements",
                                    patterns.len(),
                                    element_types.len()
                                ),
                            ));
                        }
                        for (pat, elem_ty) in patterns.iter().zip(element_types.iter()) {
                            self.bind_pattern_to_env(pat, elem_ty, env)?;
                        }
                        Ok(())
                    }
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot destructure non-tuple type: {ty}"),
                    )),
                }
            }
            Pattern::Literal(_) => {
                // Literal patterns don't bind variables
                Ok(())
            }
            Pattern::EnumVariant { .. } => {
                // Enum variant patterns are handled in match expressions
                Ok(())
            }
            Pattern::Range { .. } => {
                // Range patterns don't bind variables
                Ok(())
            }
            Pattern::Or { patterns } => {
                // Or patterns: bind variables from all alternatives
                for pat in patterns {
                    self.bind_pattern_to_env(pat, ty, env)?;
                }
                Ok(())
            }
            Pattern::Struct {
                struct_name,
                fields,
                partial: _,
            } => {
                // Validate struct type and bind field patterns
                if let Type::Custom(type_name) = ty {
                    if type_name != struct_name {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Struct pattern {struct_name} does not match type {type_name}"),
                        ));
                    }

                    // Look up struct definition
                    if let Some(struct_def) = self.env.lookup_struct(struct_name) {
                        // Bind variables from field patterns
                        for (field_name, field_pattern) in fields {
                            if let Some(field_type) = struct_def
                                .fields
                                .iter()
                                .find(|(name, _)| name.as_str() == field_name.as_str())
                                .map(|(_, ty)| ty)
                            {
                                self.bind_pattern_to_env(field_pattern, field_type, env)?;
                            } else {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Struct {struct_name} has no field named {field_name}"),
                                ));
                            }
                        }
                        Ok(())
                    } else {
                        Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Unknown struct type: {struct_name}"),
                        ))
                    }
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot destructure non-struct type: {ty}"),
                    ))
                }
            }
        }
    }

    /// Check if two types are compatible.
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        if let Type::Custom(name) = expected {
            if self.is_generic_param(name) {
                return true;
            }
        }

        // Generics should be fully erased by monomorphization before type checking.
        if matches!(expected, Type::Generic { .. }) || matches!(actual, Type::Generic { .. }) {
            return false;
        }

        if expected == actual {
            return true;
        }

        // Self in trait declarations matches any concrete type in trait impls
        if let Type::Custom(name) = expected {
            if name == "Self" {
                return matches!(actual, Type::Custom(_));
            }
        }
        if let Type::Custom(name) = actual {
            if name == "Self" {
                return matches!(expected, Type::Custom(_));
            }
        }

        // dyn Trait is compatible with concrete struct types (for assignment and param passing)
        if matches!(expected, Type::TraitObject { .. }) && matches!(actual, Type::Custom(_)) {
            return true;
        }
        if matches!(expected, Type::Custom(_)) && matches!(actual, Type::TraitObject { .. }) {
            return true;
        }

        matches!(
            (expected, actual),
            (Type::String, Type::Bytes) | (Type::Bytes, Type::String)
        )
    }

    /// Check if a match expression is exhaustive.
    fn is_match_exhaustive(&self, arms: &[MatchArm], expr_type: &Type) -> bool {
        // Check if there's a wildcard pattern (always exhaustive)
        for arm in arms {
            if self.pattern_is_wildcard(&arm.pattern) {
                return true;
            }
        }

        // For enum types, check if all variants are covered
        if let Type::Custom(enum_name) = expr_type {
            if let Some(enum_type) = self.env.lookup_enum(enum_name) {
                return self.enum_variants_covered(arms, &enum_type);
            }
        }

        // For primitive types without wildcard, not exhaustive
        // (would need to enumerate all possible values, which is impractical)
        false
    }

    /// Check if a pattern is effectively a wildcard (matches everything).
    #[allow(clippy::only_used_in_recursion)]
    fn pattern_is_wildcard(&self, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::Wildcard => true,
            Pattern::Identifier(_) => true, // Identifier patterns match everything
            Pattern::Or { patterns } => {
                // Or pattern is wildcard if any alternative is wildcard
                patterns.iter().any(|p| self.pattern_is_wildcard(p))
            }
            _ => false,
        }
    }

    /// Check if all enum variants are covered by the match arms.
    fn enum_variants_covered(&self, arms: &[MatchArm], enum_type: &EnumType) -> bool {
        let mut covered_variants = std::collections::HashSet::new();

        for arm in arms {
            self.collect_covered_variants(&arm.pattern, &mut covered_variants);
        }

        // Check if all variants are covered
        for (variant_name, _tag, _payload) in &enum_type.variants {
            if !covered_variants.contains(variant_name) {
                return false;
            }
        }

        true
    }

    /// Collect which enum variants are covered by a pattern.
    #[allow(clippy::only_used_in_recursion)]
    fn collect_covered_variants(
        &self,
        pattern: &Pattern,
        covered: &mut std::collections::HashSet<String>,
    ) {
        match pattern {
            Pattern::EnumVariant { variant_name, .. } => {
                covered.insert(variant_name.clone());
            }
            Pattern::Or { patterns } => {
                for p in patterns {
                    self.collect_covered_variants(p, covered);
                }
            }
            _ => {}
        }
    }

    /// Create a type error.
    fn type_error(&self, message: impl Into<String>) -> CompilerError {
        CompilerError::type_error(SourceLocation::new(self.file_path.clone(), 0, 0), message)
    }

    /// Type check a trait declaration.
    fn check_trait_declaration(
        &mut self,
        _name: &str,
        _generic_params: &[String],
        super_traits: &[String],
        methods: &[crate::parser::ast::TraitMethod],
        _associated_types: &[crate::parser::ast::AssociatedType],
    ) -> CompilerResult<()> {
        // Check that super traits exist
        for super_trait in super_traits {
            if self.env.lookup_trait(super_trait).is_none() {
                return Err(self.type_error(format!("Super trait '{super_trait}' not found")));
            }
        }

        // Add 'Self' as a recognized type parameter within trait context
        let previous_generic_params = std::mem::take(&mut self.current_generic_params);
        self.current_generic_params = _generic_params.to_vec();
        self.current_generic_params.push("Self".to_string());

        // Validate method signatures
        for method in methods {
            // Check parameter types
            for param in &method.parameters {
                self.validate_type(&param.param_type)?;
            }

            // Check return type
            if let Some(return_type) = &method.return_type {
                self.validate_type(return_type)?;
            }

            // If method has a body (provided method), type check it
            if let Some(body) = &method.body {
                let saved_return_type = self.current_function_return_type.clone();
                self.current_function_return_type = method.return_type.clone();

                self.check_block(body)?;

                self.current_function_return_type = saved_return_type;
            }
        }

        self.current_generic_params = previous_generic_params;

        // Trait already registered during predeclaration phase.
        // Re-register with validated method bodies if needed.
        Ok(())
    }

    /// Type check a trait implementation.
    fn check_trait_impl(
        &mut self,
        trait_name: &str,
        type_name: &str,
        generic_params: &[String],
        methods: &[Statement],
    ) -> CompilerResult<()> {
        use super::types::{FunctionType, TraitImpl};
        use std::collections::HashMap;

        // Add 'Self' as a recognized type parameter within trait impl context
        let previous_generic_params = std::mem::take(&mut self.current_generic_params);
        self.current_generic_params = generic_params.to_vec();
        self.current_generic_params.push("Self".to_string());

        // Check that the trait exists
        let trait_def = self
            .env
            .lookup_trait(trait_name)
            .ok_or_else(|| self.type_error(format!("Trait '{trait_name}' not found")))?;

        // Check that the type exists (struct or enum)
        if self.env.lookup_struct(type_name).is_none() && self.env.lookup_enum(type_name).is_none()
        {
            return Err(self.type_error(format!("Type '{type_name}' not found")));
        }

        // Orphan rules (coherence checking): Check for duplicate implementations
        // A trait can only be implemented once for a given type
        if self.env.lookup_trait_impl(trait_name, type_name).is_some() {
            return Err(self.type_error(format!(
                "Trait '{trait_name}' is already implemented for type '{type_name}'"
            )));
        }

        // Type check each method implementation
        let mut impl_methods = HashMap::new();
        for method_stmt in methods {
            if let Statement::FunctionDeclaration {
                name: method_name,
                parameters,
                return_type,
                body,
                ..
            } = method_stmt
            {
                // Check that this method is required by the trait
                let trait_method = trait_def.get_method(method_name).ok_or_else(|| {
                    self.type_error(format!(
                        "Method '{method_name}' is not part of trait '{trait_name}'"
                    ))
                })?;

                // Verify method signature matches trait requirement
                if parameters.len() != trait_method.parameters.len() {
                    return Err(self.type_error(format!(
                        "Method '{method_name}' has {} parameters, but trait requires {}",
                        parameters.len(),
                        trait_method.parameters.len()
                    )));
                }

                // Check parameter types match
                for (i, (impl_param, trait_param)) in
                    parameters.iter().zip(&trait_method.parameters).enumerate()
                {
                    if !self.types_compatible(&impl_param.param_type, &trait_param.param_type) {
                        return Err(self.type_error(format!(
                            "Parameter {i} of method '{method_name}' has type {:?}, but trait requires {:?}",
                            impl_param.param_type, trait_param.param_type
                        )));
                    }
                }

                // Check return type matches
                let impl_return = return_type.clone().unwrap_or(Type::Void);
                let trait_return = trait_method.return_type.clone().unwrap_or(Type::Void);
                if !self.types_compatible(&impl_return, &trait_return) {
                    return Err(self.type_error(format!(
                        "Method '{method_name}' returns {impl_return:?}, but trait requires {trait_return:?}"
                    )));
                }

                // Type check the method body in a child environment with parameters bound
                let saved_return_type = self.current_function_return_type.clone();
                self.current_function_return_type = return_type.clone();

                let mut method_env = self.env.child();
                for param in parameters {
                    // Resolve Self → concrete type for method body checking
                    let resolved_type = if param.param_type == Type::Custom("Self".to_string()) {
                        Type::Custom(type_name.to_string())
                    } else {
                        param.param_type.clone()
                    };
                    self.bind_pattern_to_env(&param.pattern, &resolved_type, &mut method_env)?;
                }
                let saved_env = std::mem::replace(&mut self.env, method_env);
                self.check_block(body)?;
                self.env = saved_env;

                self.current_function_return_type = saved_return_type;

                // Store the method signature
                let param_types = parameters.iter().map(|p| p.param_type.clone()).collect();
                impl_methods.insert(
                    method_name.clone(),
                    FunctionType::new(param_types, impl_return, false),
                );
            }
        }

        // Check that all required methods are implemented
        for trait_method in &trait_def.methods {
            if trait_method.body.is_none() && !impl_methods.contains_key(&trait_method.name) {
                return Err(self.type_error(format!(
                    "Missing implementation for required method '{}' from trait '{trait_name}'",
                    trait_method.name
                )));
            }
        }

        // Register the trait implementation
        let trait_impl = TraitImpl::new(
            trait_name.to_string(),
            type_name.to_string(),
            generic_params.to_vec(),
            impl_methods,
        );
        self.env
            .define_trait_impl(trait_name.to_string(), type_name.to_string(), trait_impl);

        self.current_generic_params = previous_generic_params;

        Ok(())
    }

    /// Validate that a type is well-formed.
    fn validate_type(&self, ty: &Type) -> CompilerResult<()> {
        match ty {
            Type::Custom(name) => {
                if self.env.lookup_struct(name).is_none()
                    && self.env.lookup_enum(name).is_none()
                    && !self.current_generic_params.contains(name)
                {
                    return Err(self.type_error(format!("Type '{name}' not found")));
                }
                Ok(())
            }
            Type::Array { element_type, .. } => self.validate_type(element_type),
            Type::Reference { inner_type, .. } => self.validate_type(inner_type),
            Type::Tuple { element_types } => {
                for ty in element_types {
                    self.validate_type(ty)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenizer::Tokenizer;
    use crate::parser::parser::Parser;
    use std::path::PathBuf;

    fn type_check_source(source: &str) -> CompilerResult<()> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.kr"));
        let program = parser.parse()?;
        let mut checker = TypeChecker::new(PathBuf::from("test.kr"));
        checker.check_program(&program)
    }

    #[test]
    fn test_variable_declaration() {
        assert!(type_check_source("let x: int = 42;").is_ok());
        assert!(type_check_source("let x = 42;").is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = type_check_source("let x: int = 3.14;");
        assert!(result.is_err());
    }

    #[test]
    fn test_function_declaration() {
        let source = r#"
            fn add(a: int, b: int) -> int {
                return a + b;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_if_statement() {
        let source = r#"
            let x = 5;
            if (x > 0) {
                let y = 10;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }
}
