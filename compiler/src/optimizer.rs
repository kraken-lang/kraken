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

/// Loop optimization pass.
pub struct LoopOptimizer;

impl LoopOptimizer {
    /// Create a new loop optimizer.
    pub fn new() -> Self {
        Self
    }

    /// Detect loop invariant code that can be hoisted.
    pub fn is_loop_invariant(&self, _value: &Value, _loop_body: &[Instruction]) -> bool {
        // Simplified: check if value doesn't depend on loop variables
        // In a real implementation, this would do proper data flow analysis
        false
    }

    /// Detect induction variables for strength reduction.
    pub fn is_induction_variable(&self, _value: &Value, _loop_body: &[Instruction]) -> bool {
        // Simplified: detect variables that increment by constant in loop
        false
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
    pub fn can_combine_allocations(&self, _size1: usize, _size2: usize) -> bool {
        // Simplified: check if consecutive allocations can be merged
        false
    }

    /// Check if allocation can be stack-allocated instead of heap.
    pub fn can_stack_allocate(&self, size: usize, _lifetime: &str) -> bool {
        // Simple heuristic: small allocations with known lifetime
        size <= 1024
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
        assert!(!optimizer.can_stack_allocate(2048, "local"));
    }
}
