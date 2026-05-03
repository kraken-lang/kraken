//! Compiler optimizations for Kraken.
//!
//! Provides optimization passes including constant folding, dead code elimination,
//! and common pattern optimizations.

/// Value representation for optimization.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Constant(i64),
    Register(usize),
}

/// Instruction representation for optimization.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    BinaryOp {
        op: String,
        left: Value,
        right: Value,
        result: Value,
    },
    UnaryOp {
        op: String,
        operand: Value,
        result: Value,
    },
    Load {
        address: Value,
        result: Value,
    },
    Store {
        address: Value,
        value: Value,
    },
    Call {
        function: String,
        args: Vec<Value>,
        result: Option<Value>,
    },
    Return {
        value: Option<Value>,
    },
    Branch {
        target: String,
    },
    CondBranch {
        condition: Value,
        true_target: String,
        false_target: String,
    },
}

/// Constant folding optimization pass.
pub struct ConstantFolder;

impl ConstantFolder {
    /// Create a new constant folder.
    pub fn new() -> Self {
        Self
    }

    /// Fold binary operations with constant operands.
    pub fn fold_binary_op(&self, op: &str, left: i64, right: i64) -> Option<i64> {
        match op {
            "+" => Some(left.wrapping_add(right)),
            "-" => Some(left.wrapping_sub(right)),
            "*" => Some(left.wrapping_mul(right)),
            "/" => {
                if right != 0 {
                    Some(left.wrapping_div(right))
                } else {
                    None
                }
            }
            "%" => {
                if right != 0 {
                    Some(left.wrapping_rem(right))
                } else {
                    None
                }
            }
            "&" => Some(left & right),
            "|" => Some(left | right),
            "^" => Some(left ^ right),
            "<<" => Some(left << (right & 63)),
            ">>" => Some(left >> (right & 63)),
            _ => None,
        }
    }

    /// Fold comparison operations with constant operands.
    pub fn fold_comparison(&self, op: &str, left: i64, right: i64) -> Option<bool> {
        match op {
            "==" => Some(left == right),
            "!=" => Some(left != right),
            "<" => Some(left < right),
            "<=" => Some(left <= right),
            ">" => Some(left > right),
            ">=" => Some(left >= right),
            _ => None,
        }
    }

    /// Fold unary operations with constant operands.
    pub fn fold_unary_op(&self, op: &str, operand: i64) -> Option<i64> {
        match op {
            "-" => Some(operand.wrapping_neg()),
            "~" => Some(!operand),
            _ => None,
        }
    }

    /// Optimize algebraic identities (e.g., x + 0 = x, x * 1 = x).
    pub fn fold_identity(&self, op: &str, left: &Value, right: &Value) -> Option<Value> {
        match (op, left, right) {
            ("+", val, Value::Constant(0)) | ("+", Value::Constant(0), val) => Some(val.clone()),
            ("-", val, Value::Constant(0)) => Some(val.clone()),
            ("*", val, Value::Constant(1)) | ("*", Value::Constant(1), val) => Some(val.clone()),
            ("*", _, Value::Constant(0)) | ("*", Value::Constant(0), _) => Some(Value::Constant(0)),
            ("/", val, Value::Constant(1)) => Some(val.clone()),
            ("|", val, Value::Constant(0)) | ("|", Value::Constant(0), val) => Some(val.clone()),
            ("&", _, Value::Constant(0)) | ("&", Value::Constant(0), _) => Some(Value::Constant(0)),
            ("^", val, Value::Constant(0)) | ("^", Value::Constant(0), val) => Some(val.clone()),
            _ => None,
        }
    }
}

impl Default for ConstantFolder {
    fn default() -> Self {
        Self::new()
    }
}

/// Dead code elimination pass.
pub struct DeadCodeEliminator;

impl DeadCodeEliminator {
    /// Create a new dead code eliminator.
    pub fn new() -> Self {
        Self
    }

    /// Check if an instruction has side effects.
    pub fn has_side_effects(&self, instr: &Instruction) -> bool {
        matches!(
            instr,
            Instruction::Call { .. }
                | Instruction::Store { .. }
                | Instruction::Return { .. }
                | Instruction::Branch { .. }
                | Instruction::CondBranch { .. }
        )
    }

    /// Check if a value is used.
    pub fn is_value_used(&self, value: &Value, instructions: &[Instruction]) -> bool {
        instructions
            .iter()
            .any(|instr| self.instruction_uses_value(instr, value))
    }

    /// Check if an instruction uses a value.
    fn instruction_uses_value(&self, instr: &Instruction, value: &Value) -> bool {
        match instr {
            Instruction::BinaryOp { left, right, .. } => left == value || right == value,
            Instruction::UnaryOp { operand, .. } => operand == value,
            Instruction::Load { address, .. } => address == value,
            Instruction::Store {
                address, value: v, ..
            } => address == value || v == value,
            Instruction::Call { args, .. } => args.contains(value),
            Instruction::Return { value: Some(v), .. } => v == value,
            Instruction::CondBranch { condition, .. } => condition == value,
            _ => false,
        }
    }
}

impl Default for DeadCodeEliminator {
    fn default() -> Self {
        Self::new()
    }
}

/// Allocation strategy recommendation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocationStrategy {
    /// Zero-size type, no allocation needed.
    None,
    /// Small enough to inline in the struct itself.
    Inline,
    /// Stack allocation for short-lived values.
    Stack,
    /// Small heap allocation (arena-friendly).
    SmallHeap,
    /// Large heap allocation.
    LargeHeap,
}

/// SIMD optimization hints for vectorizable operations.
pub struct SimdHints;

impl SimdHints {
    /// Check if an operation can be vectorized with SIMD.
    ///
    /// Operations on contiguous arrays of uniform numeric types
    /// are candidates for SIMD vectorization.
    pub fn is_vectorizable(op: &str, element_count: usize) -> bool {
        const MIN_SIMD_ELEMENTS: usize = 4;
        let supported_op = matches!(op, "+" | "-" | "*" | "/" | "&" | "|" | "^");
        supported_op && element_count >= MIN_SIMD_ELEMENTS
    }

    /// Recommend SIMD width for a given element size in bytes.
    pub fn recommended_width(element_size: usize) -> usize {
        const SIMD_REGISTER_BYTES: usize = 32; // AVX2
        if element_size == 0 {
            return 0;
        }
        SIMD_REGISTER_BYTES / element_size
    }
}

/// Compilation artifact cache for incremental builds.
pub struct CompilationCache {
    entries: std::collections::HashMap<u64, CacheEntry>,
}

/// A cached compilation artifact.
#[derive(Debug, Clone)]
pub struct CacheEntry {
    /// Hash of the source content.
    pub source_hash: u64,
    /// Whether the cached artifact is still valid.
    pub valid: bool,
}

impl CompilationCache {
    /// Create a new empty compilation cache.
    pub fn new() -> Self {
        Self {
            entries: std::collections::HashMap::new(),
        }
    }

    /// Check if a source file has a valid cached artifact.
    pub fn is_cached(&self, source_hash: u64) -> bool {
        self.entries.get(&source_hash).is_some_and(|e| e.valid)
    }

    /// Insert or update a cache entry.
    pub fn insert(&mut self, source_hash: u64) {
        self.entries.insert(
            source_hash,
            CacheEntry {
                source_hash,
                valid: true,
            },
        );
    }

    /// Invalidate a cache entry.
    pub fn invalidate(&mut self, source_hash: u64) {
        if let Some(entry) = self.entries.get_mut(&source_hash) {
            entry.valid = false;
        }
    }

    /// Invalidate all cache entries.
    pub fn invalidate_all(&mut self) {
        for entry in self.entries.values_mut() {
            entry.valid = false;
        }
    }

    /// Get the number of valid cache entries.
    pub fn valid_count(&self) -> usize {
        self.entries.values().filter(|e| e.valid).count()
    }

    /// Compute a hash for source content.
    pub fn hash_source(source: &str) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        source.hash(&mut hasher);
        hasher.finish()
    }
}

impl Default for CompilationCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Loop optimization pass.
pub struct LoopOptimizer;

impl LoopOptimizer {
    /// Create a new loop optimizer.
    pub fn new() -> Self {
        Self
    }

    /// Detect loop invariant code that can be hoisted.
    ///
    /// A value is loop-invariant if it is a constant or if it only depends
    /// on values defined outside the loop body.
    pub fn is_loop_invariant(&self, value: &Value, loop_body: &[Instruction]) -> bool {
        match value {
            Value::Constant(_) => true,
            Value::Register(reg) => {
                // Check if any instruction in the loop defines this register
                !loop_body.iter().any(|instr| match instr {
                    Instruction::BinaryOp { result, .. }
                    | Instruction::UnaryOp { result, .. }
                    | Instruction::Load { result, .. } => result == &Value::Register(*reg),
                    Instruction::Call {
                        result: Some(r), ..
                    } => r == &Value::Register(*reg),
                    _ => false,
                })
            }
        }
    }

    /// Detect induction variables for strength reduction.
    ///
    /// An induction variable is one that increments by a constant on each
    /// loop iteration (e.g., `i = i + 1`).
    pub fn is_induction_variable(&self, value: &Value, loop_body: &[Instruction]) -> bool {
        if let Value::Register(reg) = value {
            loop_body.iter().any(|instr| {
                if let Instruction::BinaryOp {
                    op,
                    left,
                    right,
                    result,
                } = instr
                {
                    (op == "+" || op == "-")
                        && result == &Value::Register(*reg)
                        && ((left == &Value::Register(*reg) && matches!(right, Value::Constant(_)))
                            || (right == &Value::Register(*reg)
                                && matches!(left, Value::Constant(_))))
                } else {
                    false
                }
            })
        } else {
            false
        }
    }

    /// Count the number of loop-invariant instructions that could be hoisted.
    pub fn count_hoistable(&self, loop_body: &[Instruction]) -> usize {
        loop_body
            .iter()
            .filter(|instr| match instr {
                Instruction::BinaryOp { left, right, .. } => {
                    self.is_loop_invariant(left, loop_body)
                        && self.is_loop_invariant(right, loop_body)
                }
                Instruction::UnaryOp { operand, .. } => self.is_loop_invariant(operand, loop_body),
                _ => false,
            })
            .count()
    }
}

impl Default for LoopOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory allocation optimizer.
pub struct MemoryOptimizer;

impl MemoryOptimizer {
    /// Create a new memory optimizer.
    pub fn new() -> Self {
        Self
    }

    /// Check if allocations can be combined.
    ///
    /// Two consecutive allocations can be combined if they are of compatible
    /// types and their combined size doesn't exceed the maximum allocation unit.
    pub fn can_combine_allocations(&self, size1: usize, size2: usize) -> bool {
        const MAX_COMBINED_ALLOCATION: usize = 4096;
        let combined = size1.saturating_add(size2);
        combined <= MAX_COMBINED_ALLOCATION && size1 > 0 && size2 > 0
    }

    /// Check if allocation can be stack-allocated instead of heap.
    ///
    /// Stack allocation is preferred for small, short-lived allocations
    /// with known lifetime that don't escape the current scope.
    pub fn can_stack_allocate(&self, size: usize, lifetime: &str) -> bool {
        const MAX_STACK_ALLOCATION: usize = 1024;
        size <= MAX_STACK_ALLOCATION && matches!(lifetime, "local" | "block" | "scope")
    }

    /// Estimate the optimal allocation strategy for a given size.
    pub fn allocation_strategy(&self, size: usize) -> AllocationStrategy {
        if size == 0 {
            AllocationStrategy::None
        } else if size <= 64 {
            AllocationStrategy::Inline
        } else if size <= 1024 {
            AllocationStrategy::Stack
        } else if size <= 65536 {
            AllocationStrategy::SmallHeap
        } else {
            AllocationStrategy::LargeHeap
        }
    }
}

impl Default for MemoryOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_fold_binary_add() {
        let folder = ConstantFolder::new();
        assert_eq!(folder.fold_binary_op("+", 5, 3), Some(8));
        assert_eq!(folder.fold_binary_op("+", -5, 3), Some(-2));
    }

    #[test]
    fn test_constant_fold_binary_mul() {
        let folder = ConstantFolder::new();
        assert_eq!(folder.fold_binary_op("*", 5, 3), Some(15));
        assert_eq!(folder.fold_binary_op("*", -5, 3), Some(-15));
    }

    #[test]
    fn test_constant_fold_binary_div() {
        let folder = ConstantFolder::new();
        assert_eq!(folder.fold_binary_op("/", 15, 3), Some(5));
        assert_eq!(folder.fold_binary_op("/", 15, 0), None); // Division by zero
    }

    #[test]
    fn test_constant_fold_comparison() {
        let folder = ConstantFolder::new();
        assert_eq!(folder.fold_comparison("==", 5, 5), Some(true));
        assert_eq!(folder.fold_comparison("==", 5, 3), Some(false));
        assert_eq!(folder.fold_comparison("<", 3, 5), Some(true));
        assert_eq!(folder.fold_comparison(">", 5, 3), Some(true));
    }

    #[test]
    fn test_constant_fold_unary() {
        let folder = ConstantFolder::new();
        assert_eq!(folder.fold_unary_op("-", 5), Some(-5));
        assert_eq!(folder.fold_unary_op("~", 0), Some(-1));
    }

    #[test]
    fn test_fold_identity_add_zero() {
        let folder = ConstantFolder::new();
        let val = Value::Register(1);
        assert_eq!(
            folder.fold_identity("+", &val, &Value::Constant(0)),
            Some(val.clone())
        );
    }

    #[test]
    fn test_fold_identity_mul_one() {
        let folder = ConstantFolder::new();
        let val = Value::Register(1);
        assert_eq!(
            folder.fold_identity("*", &val, &Value::Constant(1)),
            Some(val.clone())
        );
    }

    #[test]
    fn test_fold_identity_mul_zero() {
        let folder = ConstantFolder::new();
        let val = Value::Register(1);
        assert_eq!(
            folder.fold_identity("*", &val, &Value::Constant(0)),
            Some(Value::Constant(0))
        );
    }

    #[test]
    fn test_dead_code_has_side_effects() {
        let eliminator = DeadCodeEliminator::new();
        assert!(eliminator.has_side_effects(&Instruction::Call {
            function: "test".to_string(),
            args: vec![],
            result: None,
        }));
        assert!(eliminator.has_side_effects(&Instruction::Return { value: None }));
    }

    #[test]
    fn test_memory_optimizer_stack_allocate() {
        let optimizer = MemoryOptimizer::new();
        assert!(optimizer.can_stack_allocate(512, "local"));
        assert!(optimizer.can_stack_allocate(512, "block"));
        assert!(optimizer.can_stack_allocate(512, "scope"));
        assert!(!optimizer.can_stack_allocate(2048, "local"));
        assert!(!optimizer.can_stack_allocate(512, "global"));
    }

    #[test]
    fn test_memory_optimizer_combine_allocations() {
        let optimizer = MemoryOptimizer::new();
        assert!(optimizer.can_combine_allocations(100, 200));
        assert!(optimizer.can_combine_allocations(2048, 2048));
        assert!(!optimizer.can_combine_allocations(4000, 4000));
        assert!(!optimizer.can_combine_allocations(0, 100));
        assert!(!optimizer.can_combine_allocations(100, 0));
    }

    #[test]
    fn test_memory_optimizer_allocation_strategy() {
        let optimizer = MemoryOptimizer::new();
        assert_eq!(optimizer.allocation_strategy(0), AllocationStrategy::None);
        assert_eq!(
            optimizer.allocation_strategy(32),
            AllocationStrategy::Inline
        );
        assert_eq!(
            optimizer.allocation_strategy(64),
            AllocationStrategy::Inline
        );
        assert_eq!(
            optimizer.allocation_strategy(512),
            AllocationStrategy::Stack
        );
        assert_eq!(
            optimizer.allocation_strategy(1024),
            AllocationStrategy::Stack
        );
        assert_eq!(
            optimizer.allocation_strategy(8192),
            AllocationStrategy::SmallHeap
        );
        assert_eq!(
            optimizer.allocation_strategy(65536),
            AllocationStrategy::SmallHeap
        );
        assert_eq!(
            optimizer.allocation_strategy(100000),
            AllocationStrategy::LargeHeap
        );
    }

    #[test]
    fn test_simd_hints_vectorizable() {
        assert!(SimdHints::is_vectorizable("+", 4));
        assert!(SimdHints::is_vectorizable("*", 8));
        assert!(SimdHints::is_vectorizable("&", 16));
        assert!(!SimdHints::is_vectorizable("+", 3)); // Too few elements
        assert!(!SimdHints::is_vectorizable("==", 8)); // Unsupported op
    }

    #[test]
    fn test_simd_hints_recommended_width() {
        assert_eq!(SimdHints::recommended_width(4), 8); // 32/4 = 8 i32s
        assert_eq!(SimdHints::recommended_width(8), 4); // 32/8 = 4 i64s
        assert_eq!(SimdHints::recommended_width(1), 32); // 32/1 = 32 bytes
        assert_eq!(SimdHints::recommended_width(0), 0); // Zero-size
    }

    #[test]
    fn test_compilation_cache_basic() {
        let mut cache = CompilationCache::new();
        let hash = CompilationCache::hash_source("fn main() {}");

        assert!(!cache.is_cached(hash));
        cache.insert(hash);
        assert!(cache.is_cached(hash));
        assert_eq!(cache.valid_count(), 1);
    }

    #[test]
    fn test_compilation_cache_invalidate() {
        let mut cache = CompilationCache::new();
        let hash = CompilationCache::hash_source("fn main() {}");

        cache.insert(hash);
        assert!(cache.is_cached(hash));

        cache.invalidate(hash);
        assert!(!cache.is_cached(hash));
        assert_eq!(cache.valid_count(), 0);
    }

    #[test]
    fn test_compilation_cache_invalidate_all() {
        let mut cache = CompilationCache::new();
        let h1 = CompilationCache::hash_source("fn a() {}");
        let h2 = CompilationCache::hash_source("fn b() {}");

        cache.insert(h1);
        cache.insert(h2);
        assert_eq!(cache.valid_count(), 2);

        cache.invalidate_all();
        assert_eq!(cache.valid_count(), 0);
    }

    #[test]
    fn test_compilation_cache_different_sources() {
        let h1 = CompilationCache::hash_source("fn a() {}");
        let h2 = CompilationCache::hash_source("fn b() {}");
        assert_ne!(h1, h2);
    }

    #[test]
    fn test_loop_optimizer_invariant_constant() {
        let optimizer = LoopOptimizer::new();
        let loop_body = vec![Instruction::BinaryOp {
            op: "+".to_string(),
            left: Value::Register(0),
            right: Value::Constant(1),
            result: Value::Register(0),
        }];
        // Constants are always loop-invariant
        assert!(optimizer.is_loop_invariant(&Value::Constant(42), &loop_body));
    }

    #[test]
    fn test_loop_optimizer_invariant_register() {
        let optimizer = LoopOptimizer::new();
        let loop_body = vec![Instruction::BinaryOp {
            op: "+".to_string(),
            left: Value::Register(0),
            right: Value::Constant(1),
            result: Value::Register(0),
        }];
        // Register 0 is modified in the loop, not invariant
        assert!(!optimizer.is_loop_invariant(&Value::Register(0), &loop_body));
        // Register 1 is not modified in the loop, is invariant
        assert!(optimizer.is_loop_invariant(&Value::Register(1), &loop_body));
    }

    #[test]
    fn test_loop_optimizer_induction_variable() {
        let optimizer = LoopOptimizer::new();
        let loop_body = vec![Instruction::BinaryOp {
            op: "+".to_string(),
            left: Value::Register(0),
            right: Value::Constant(1),
            result: Value::Register(0),
        }];
        assert!(optimizer.is_induction_variable(&Value::Register(0), &loop_body));
        assert!(!optimizer.is_induction_variable(&Value::Register(1), &loop_body));
        assert!(!optimizer.is_induction_variable(&Value::Constant(0), &loop_body));
    }

    #[test]
    fn test_loop_optimizer_count_hoistable() {
        let optimizer = LoopOptimizer::new();
        let loop_body = vec![
            // i = i + 1 (not hoistable, depends on loop var)
            Instruction::BinaryOp {
                op: "+".to_string(),
                left: Value::Register(0),
                right: Value::Constant(1),
                result: Value::Register(0),
            },
            // 5 + 10 (hoistable, both constants)
            Instruction::BinaryOp {
                op: "+".to_string(),
                left: Value::Constant(5),
                right: Value::Constant(10),
                result: Value::Register(2),
            },
        ];
        assert_eq!(optimizer.count_hoistable(&loop_body), 1);
    }

    #[test]
    fn test_dead_code_value_used() {
        let eliminator = DeadCodeEliminator::new();
        let val = Value::Register(1);
        let instructions = vec![Instruction::BinaryOp {
            op: "+".to_string(),
            left: Value::Register(1),
            right: Value::Constant(2),
            result: Value::Register(3),
        }];
        assert!(eliminator.is_value_used(&val, &instructions));
        assert!(!eliminator.is_value_used(&Value::Register(5), &instructions));
    }

    #[test]
    fn test_constant_fold_bitwise() {
        let folder = ConstantFolder::new();
        assert_eq!(folder.fold_binary_op("&", 0xFF, 0x0F), Some(0x0F));
        assert_eq!(folder.fold_binary_op("|", 0xF0, 0x0F), Some(0xFF));
        assert_eq!(folder.fold_binary_op("^", 0xFF, 0xFF), Some(0));
        assert_eq!(folder.fold_binary_op("<<", 1, 4), Some(16));
        assert_eq!(folder.fold_binary_op(">>", 16, 4), Some(1));
    }

    #[test]
    fn test_constant_fold_modulo() {
        let folder = ConstantFolder::new();
        assert_eq!(folder.fold_binary_op("%", 10, 3), Some(1));
        assert_eq!(folder.fold_binary_op("%", 10, 0), None); // Division by zero
    }

    #[test]
    fn test_fold_identity_xor_zero() {
        let folder = ConstantFolder::new();
        let val = Value::Register(1);
        assert_eq!(
            folder.fold_identity("^", &val, &Value::Constant(0)),
            Some(val.clone())
        );
    }

    #[test]
    fn test_fold_identity_and_zero() {
        let folder = ConstantFolder::new();
        let val = Value::Register(1);
        assert_eq!(
            folder.fold_identity("&", &val, &Value::Constant(0)),
            Some(Value::Constant(0))
        );
    }

    // --- Default impls ---

    #[test]
    fn test_constant_folder_default() {
        let f = ConstantFolder;
        assert_eq!(f.fold_binary_op("+", 1, 2), Some(3));
    }

    #[test]
    fn test_dead_code_eliminator_default() {
        let e = DeadCodeEliminator;
        assert!(e.has_side_effects(&Instruction::Return { value: None }));
    }

    #[test]
    fn test_loop_optimizer_default() {
        let o = LoopOptimizer;
        assert!(o.is_loop_invariant(&Value::Constant(0), &[]));
    }

    #[test]
    fn test_memory_optimizer_default() {
        let m = MemoryOptimizer;
        assert_eq!(m.allocation_strategy(0), AllocationStrategy::None);
    }

    #[test]
    fn test_compilation_cache_default() {
        let c = CompilationCache::default();
        assert_eq!(c.valid_count(), 0);
    }

    // --- Unknown ops return None ---

    #[test]
    fn test_fold_binary_unknown_op() {
        let f = ConstantFolder::new();
        assert_eq!(f.fold_binary_op("??", 1, 2), None);
    }

    #[test]
    fn test_fold_unary_unknown_op() {
        let f = ConstantFolder::new();
        assert_eq!(f.fold_unary_op("!", 1), None);
    }

    #[test]
    fn test_fold_comparison_unknown_op() {
        let f = ConstantFolder::new();
        assert_eq!(f.fold_comparison("??", 1, 2), None);
    }

    // --- Remaining comparison operators ---

    #[test]
    fn test_fold_comparison_all_ops() {
        let f = ConstantFolder::new();
        assert_eq!(f.fold_comparison("!=", 1, 2), Some(true));
        assert_eq!(f.fold_comparison("!=", 1, 1), Some(false));
        assert_eq!(f.fold_comparison("<=", 1, 2), Some(true));
        assert_eq!(f.fold_comparison("<=", 2, 2), Some(true));
        assert_eq!(f.fold_comparison("<=", 3, 2), Some(false));
        assert_eq!(f.fold_comparison(">=", 3, 2), Some(true));
        assert_eq!(f.fold_comparison(">=", 2, 2), Some(true));
        assert_eq!(f.fold_comparison(">=", 1, 2), Some(false));
    }

    // --- Subtraction folding ---

    #[test]
    fn test_constant_fold_binary_sub() {
        let f = ConstantFolder::new();
        assert_eq!(f.fold_binary_op("-", 10, 3), Some(7));
        assert_eq!(f.fold_binary_op("-", 3, 10), Some(-7));
    }

    // --- fold_identity: commutative left-side and missing branches ---

    #[test]
    fn test_fold_identity_add_zero_left() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(f.fold_identity("+", &Value::Constant(0), &val), Some(val));
    }

    #[test]
    fn test_fold_identity_sub_zero() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(f.fold_identity("-", &val, &Value::Constant(0)), Some(val));
    }

    #[test]
    fn test_fold_identity_mul_one_left() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(f.fold_identity("*", &Value::Constant(1), &val), Some(val));
    }

    #[test]
    fn test_fold_identity_mul_zero_left() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(
            f.fold_identity("*", &Value::Constant(0), &val),
            Some(Value::Constant(0))
        );
    }

    #[test]
    fn test_fold_identity_div_one() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(f.fold_identity("/", &val, &Value::Constant(1)), Some(val));
    }

    #[test]
    fn test_fold_identity_or_zero_left() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(f.fold_identity("|", &Value::Constant(0), &val), Some(val));
    }

    #[test]
    fn test_fold_identity_and_zero_left() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(
            f.fold_identity("&", &Value::Constant(0), &val),
            Some(Value::Constant(0))
        );
    }

    #[test]
    fn test_fold_identity_xor_zero_left() {
        let f = ConstantFolder::new();
        let val = Value::Register(2);
        assert_eq!(f.fold_identity("^", &Value::Constant(0), &val), Some(val));
    }

    #[test]
    fn test_fold_identity_unknown_op() {
        let f = ConstantFolder::new();
        assert_eq!(
            f.fold_identity("??", &Value::Register(0), &Value::Constant(0)),
            None
        );
    }

    // --- DeadCodeEliminator: all side-effect instruction types ---

    #[test]
    fn test_has_side_effects_store() {
        let e = DeadCodeEliminator::new();
        assert!(e.has_side_effects(&Instruction::Store {
            address: Value::Register(0),
            value: Value::Constant(1),
        }));
    }

    #[test]
    fn test_has_side_effects_branch() {
        let e = DeadCodeEliminator::new();
        assert!(e.has_side_effects(&Instruction::Branch {
            target: "loop".into(),
        }));
    }

    #[test]
    fn test_has_side_effects_cond_branch() {
        let e = DeadCodeEliminator::new();
        assert!(e.has_side_effects(&Instruction::CondBranch {
            condition: Value::Register(0),
            true_target: "then".into(),
            false_target: "else".into(),
        }));
    }

    #[test]
    fn test_no_side_effects_binary_op() {
        let e = DeadCodeEliminator::new();
        assert!(!e.has_side_effects(&Instruction::BinaryOp {
            op: "+".into(),
            left: Value::Constant(1),
            right: Value::Constant(2),
            result: Value::Register(0),
        }));
    }

    #[test]
    fn test_no_side_effects_unary_op() {
        let e = DeadCodeEliminator::new();
        assert!(!e.has_side_effects(&Instruction::UnaryOp {
            op: "-".into(),
            operand: Value::Constant(1),
            result: Value::Register(0),
        }));
    }

    #[test]
    fn test_no_side_effects_load() {
        let e = DeadCodeEliminator::new();
        assert!(!e.has_side_effects(&Instruction::Load {
            address: Value::Register(0),
            result: Value::Register(1),
        }));
    }

    // --- instruction_uses_value: all instruction types ---

    #[test]
    fn test_value_used_in_unary_op() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::UnaryOp {
            op: "-".into(),
            operand: Value::Register(1),
            result: Value::Register(2),
        }];
        assert!(e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_used_in_load() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::Load {
            address: Value::Register(1),
            result: Value::Register(2),
        }];
        assert!(e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_used_in_store_address() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::Store {
            address: Value::Register(1),
            value: Value::Constant(0),
        }];
        assert!(e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_used_in_store_value() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::Store {
            address: Value::Register(9),
            value: Value::Register(1),
        }];
        assert!(e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_used_in_call_args() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::Call {
            function: "f".into(),
            args: vec![Value::Register(1)],
            result: None,
        }];
        assert!(e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_used_in_return() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::Return {
            value: Some(Value::Register(1)),
        }];
        assert!(e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_used_in_cond_branch() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::CondBranch {
            condition: Value::Register(1),
            true_target: "t".into(),
            false_target: "f".into(),
        }];
        assert!(e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_not_used_in_branch() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::Branch {
            target: "loop".into(),
        }];
        assert!(!e.is_value_used(&v, &instrs));
    }

    #[test]
    fn test_value_not_used_in_return_none() {
        let e = DeadCodeEliminator::new();
        let v = Value::Register(1);
        let instrs = vec![Instruction::Return { value: None }];
        assert!(!e.is_value_used(&v, &instrs));
    }

    // --- LoopOptimizer: invariant for UnaryOp, Load, Call results ---

    #[test]
    fn test_loop_invariant_register_defined_by_unary() {
        let o = LoopOptimizer::new();
        let body = vec![Instruction::UnaryOp {
            op: "-".into(),
            operand: Value::Constant(1),
            result: Value::Register(3),
        }];
        assert!(!o.is_loop_invariant(&Value::Register(3), &body));
        assert!(o.is_loop_invariant(&Value::Register(9), &body));
    }

    #[test]
    fn test_loop_invariant_register_defined_by_load() {
        let o = LoopOptimizer::new();
        let body = vec![Instruction::Load {
            address: Value::Register(0),
            result: Value::Register(4),
        }];
        assert!(!o.is_loop_invariant(&Value::Register(4), &body));
    }

    #[test]
    fn test_loop_invariant_register_defined_by_call() {
        let o = LoopOptimizer::new();
        let body = vec![Instruction::Call {
            function: "f".into(),
            args: vec![],
            result: Some(Value::Register(5)),
        }];
        assert!(!o.is_loop_invariant(&Value::Register(5), &body));
    }

    #[test]
    fn test_loop_invariant_call_no_result() {
        let o = LoopOptimizer::new();
        let body = vec![Instruction::Call {
            function: "f".into(),
            args: vec![],
            result: None,
        }];
        assert!(o.is_loop_invariant(&Value::Register(0), &body));
    }

    // --- Induction variable: subtraction and constant-on-left ---

    #[test]
    fn test_induction_variable_sub() {
        let o = LoopOptimizer::new();
        let body = vec![Instruction::BinaryOp {
            op: "-".into(),
            left: Value::Register(0),
            right: Value::Constant(1),
            result: Value::Register(0),
        }];
        assert!(o.is_induction_variable(&Value::Register(0), &body));
    }

    #[test]
    fn test_induction_variable_constant_on_left() {
        let o = LoopOptimizer::new();
        let body = vec![Instruction::BinaryOp {
            op: "+".into(),
            left: Value::Constant(1),
            right: Value::Register(0),
            result: Value::Register(0),
        }];
        assert!(o.is_induction_variable(&Value::Register(0), &body));
    }

    #[test]
    fn test_not_induction_variable_mul() {
        let o = LoopOptimizer::new();
        let body = vec![Instruction::BinaryOp {
            op: "*".into(),
            left: Value::Register(0),
            right: Value::Constant(2),
            result: Value::Register(0),
        }];
        assert!(!o.is_induction_variable(&Value::Register(0), &body));
    }

    // --- count_hoistable: with UnaryOp ---

    #[test]
    fn test_count_hoistable_unary() {
        let o = LoopOptimizer::new();
        let body = vec![
            Instruction::UnaryOp {
                op: "-".into(),
                operand: Value::Constant(42),
                result: Value::Register(1),
            },
            Instruction::UnaryOp {
                op: "-".into(),
                operand: Value::Register(0),
                result: Value::Register(2),
            },
        ];
        // First is hoistable (constant operand), second depends on reg defined outside
        // Register(0) is not defined in the loop so it's invariant
        assert_eq!(o.count_hoistable(&body), 2);
    }

    // --- CompilationCache: invalidate non-existent ---

    #[test]
    fn test_invalidate_nonexistent() {
        let mut c = CompilationCache::new();
        c.invalidate(999); // Should not panic
        assert_eq!(c.valid_count(), 0);
    }

    // --- SIMD: remaining ops ---

    #[test]
    fn test_simd_all_supported_ops() {
        for op in &["+", "-", "*", "/", "&", "|", "^"] {
            assert!(SimdHints::is_vectorizable(op, 4));
        }
    }
}
