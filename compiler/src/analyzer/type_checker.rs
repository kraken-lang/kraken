use super::types::{EnumType, FunctionType, StructType, TypeEnvironment};
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
                return_type: Type::Int, // Returns the result
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
                return_type: Type::Bytes, // Channel handle
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
                return_type: Type::Int, // Received value
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
                return_type: Type::Bytes, // Atomic handle
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
                return_type: Type::Int, // Previous value
                is_async: false,
            },
        );
        env.define_function(
            "atomic_sub".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int], // Atomic, delta
                return_type: Type::Int, // Previous value
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
                return_type: Type::Bytes, // Pool handle
                is_async: false,
            },
        );
        env.define_function(
            "pool_spawn".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Bytes], // Pool, task function ptr
                return_type: Type::Bytes, // Task handle
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
                return_type: Type::Bytes, // Task handle
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
                return_type: Type::Int, // 1 if cancelled, 0 otherwise
                is_async: false,
            },
        );

        // Timeout intrinsics
        env.define_function(
            "timeout".to_string(),
            FunctionType {
                parameter_types: vec![Type::Bytes, Type::Int], // Future, milliseconds
                return_type: Type::Int, // 0 = completed, 1 = timed out
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
        }
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

            _ => Ok(()),
        }
    }

    /// Type check a statement.
    fn check_statement(&mut self, statement: &Statement) -> CompilerResult<()> {
        match statement {
            Statement::Module { .. } => Ok(()),
            Statement::Import { .. } => Ok(()),

            Statement::VariableDeclaration {
                name,
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

                self.env.define_variable(name.clone(), var_type);
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
                parameters,
                return_type,
                body,
                is_async: _,
                is_public: _,
            } => {
                let ret_type = return_type.clone().unwrap_or(Type::Void);

                let previous_return_type = self.current_function_return_type.clone();
                self.current_function_return_type = Some(ret_type);

                let mut func_env = self.env.child();
                for param in parameters {
                    func_env.define_variable(param.name.clone(), param.param_type.clone());
                }

                let saved_env = std::mem::replace(&mut self.env, func_env);
                self.check_block(body)?;
                self.env = saved_env;

                self.current_function_return_type = previous_return_type;
                Ok(())
            }

            Statement::StructDeclaration {
                name,
                fields,
                is_public: _,
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

            Statement::InterfaceDeclaration { .. } => {
                // TODO: Implement interface checking
                Ok(())
            }

            Statement::EnumDeclaration { name, variants, .. } => {
                // Register enum with proper variant tracking
                let enum_type = EnumType::new(name.clone(), variants.clone());
                self.env.define_enum(name.clone(), enum_type);
                // Also register as custom type for type checking
                self.env.define_struct(name.clone(), StructType::new(HashMap::new()));
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
                        Pattern::EnumVariant { enum_name, variant_name, bindings } => {
                            // Create child env with bindings using actual payload types
                            let mut arm_env = self.env.child();
                            
                            // Look up the enum and variant to get payload types
                            if let Some(enum_type) = self.env.lookup_enum(enum_name) {
                                // get_variant_payload returns Option<Option<Vec<Type>>>
                                // Outer Option: variant exists? Inner Option: has payload?
                                if let Some(Some(payload_types)) = enum_type.get_variant_payload(variant_name) {
                                    // Bind each variable to its corresponding payload type
                                    for (i, binding) in bindings.iter().enumerate() {
                                        let binding_type = payload_types
                                            .get(i)
                                            .cloned()
                                            .unwrap_or(Type::Int);
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
                    }

                    self.check_block(&arm.body)?;
                }

                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),

            Statement::Defer { statement } => self.check_statement(statement),
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

            Expression::Call { callee, arguments } => {
                // Check if it's a function call first (before checking as variable)
                if let Expression::Identifier(func_name) = callee.as_ref() {
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

                        return Ok(func_type.return_type.clone());
                    }

                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!(
                            "Undefined function: {func_name}. Known functions: {}",
                            self.env.function_names().join(", ")
                        ),
                    ));
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

            Expression::StructLiteral { name, fields } => {
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
                // For now, await returns the inner type (Future<T> -> T)
                // Full implementation will check for Future type
                self.check_expression(expression)
            }

            Expression::Spawn { body } => {
                // Spawn returns a handle type (for now, use Bytes as opaque handle)
                // Check the body for type errors
                for stmt in &body.statements {
                    self.check_statement(stmt)?;
                }
                Ok(Type::Bytes) // Handle type placeholder
            }

            Expression::EnumVariant { enum_name, variant_name, payload } => {
                // Verify enum exists and variant is valid
                if let Some(enum_type) = self.env.lookup_enum(enum_name) {
                    if !enum_type.has_variant(variant_name) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Unknown variant '{}' for enum '{}'", variant_name, enum_name),
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
                        format!("Unknown enum '{}'", enum_name),
                    ))
                }
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

    /// Check if two types are compatible.
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        if expected == actual {
            return true;
        }

        matches!(
            (expected, actual),
            (Type::String, Type::Bytes) | (Type::Bytes, Type::String)
        )
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
