//! Intermediate Representation (IR) generation for bootstrap compiler.
//!
//! Provides IR generation infrastructure for translating AST to LLVM IR.

use std::collections::HashMap;

/// IR instruction types.
#[derive(Debug, Clone, PartialEq)]
pub enum IRInstruction {
    /// Allocate stack space for a variable.
    Alloca { name: String, ty: IRType },
    /// Store a value to memory.
    Store { value: IRValue, ptr: IRValue },
    /// Load a value from memory.
    Load { ptr: IRValue, result: String },
    /// Binary operation.
    BinaryOp {
        op: BinaryOp,
        left: IRValue,
        right: IRValue,
        result: String,
    },
    /// Function call.
    Call {
        function: String,
        args: Vec<IRValue>,
        result: Option<String>,
    },
    /// Return from function.
    Return { value: Option<IRValue> },
    /// Unconditional branch.
    Branch { target: String },
    /// Conditional branch.
    CondBranch {
        condition: IRValue,
        true_label: String,
        false_label: String,
    },
    /// Label for branching.
    Label { name: String },
}

/// Binary operation types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl BinaryOp {
    /// Parse binary operator from string.
    pub fn parse_op(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "%" => Some(Self::Mod),
            "==" => Some(Self::Eq),
            "!=" => Some(Self::Ne),
            "<" => Some(Self::Lt),
            "<=" => Some(Self::Le),
            ">" => Some(Self::Gt),
            ">=" => Some(Self::Ge),
            "&&" => Some(Self::And),
            "||" => Some(Self::Or),
            _ => None,
        }
    }
}

/// IR value representation.
#[derive(Debug, Clone, PartialEq)]
pub enum IRValue {
    /// Constant integer.
    ConstInt(i64),
    /// Constant float.
    ConstFloat(f64),
    /// Constant boolean.
    ConstBool(bool),
    /// Variable reference.
    Variable(String),
    /// Temporary register.
    Register(String),
}

/// IR type representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IRType {
    Void,
    I1,
    I8,
    I32,
    I64,
    F64,
    Pointer(Box<IRType>),
    Function {
        params: Vec<IRType>,
        return_type: Box<IRType>,
    },
}

impl IRType {
    /// Get the size of this type in bytes.
    pub fn size_bytes(&self) -> usize {
        match self {
            IRType::Void => 0,
            IRType::I1 => 1,
            IRType::I8 => 1,
            IRType::I32 => 4,
            IRType::I64 => 8,
            IRType::F64 => 8,
            IRType::Pointer(_) => 8,
            IRType::Function { .. } => 8,
        }
    }

    /// Check if this is a numeric type.
    pub fn is_numeric(&self) -> bool {
        matches!(self, IRType::I8 | IRType::I32 | IRType::I64 | IRType::F64)
    }

    /// Check if this is an integer type.
    pub fn is_integer(&self) -> bool {
        matches!(self, IRType::I1 | IRType::I8 | IRType::I32 | IRType::I64)
    }
}

/// IR function representation.
#[derive(Debug, Clone)]
pub struct IRFunction {
    pub name: String,
    pub params: Vec<(String, IRType)>,
    pub return_type: IRType,
    pub instructions: Vec<IRInstruction>,
}

impl IRFunction {
    /// Create a new IR function.
    pub fn new(name: String, params: Vec<(String, IRType)>, return_type: IRType) -> Self {
        Self {
            name,
            params,
            return_type,
            instructions: Vec::new(),
        }
    }

    /// Add an instruction.
    pub fn add_instruction(&mut self, instr: IRInstruction) {
        self.instructions.push(instr);
    }

    /// Get the number of instructions.
    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }
}

/// IR module containing functions and globals.
#[derive(Debug, Clone)]
pub struct IRModule {
    pub name: String,
    pub functions: HashMap<String, IRFunction>,
    pub globals: HashMap<String, IRType>,
}

impl IRModule {
    /// Create a new IR module.
    pub fn new(name: String) -> Self {
        Self {
            name,
            functions: HashMap::new(),
            globals: HashMap::new(),
        }
    }

    /// Add a function to the module.
    pub fn add_function(&mut self, function: IRFunction) {
        self.functions.insert(function.name.clone(), function);
    }

    /// Add a global variable.
    pub fn add_global(&mut self, name: String, ty: IRType) {
        self.globals.insert(name, ty);
    }

    /// Get a function by name.
    pub fn get_function(&self, name: &str) -> Option<&IRFunction> {
        self.functions.get(name)
    }

    /// Print the module as text.
    pub fn print(&self) -> String {
        let mut output = format!("module {}\n\n", self.name);

        for (name, ty) in &self.globals {
            output.push_str(&format!("global {name}: {ty:?}\n"));
        }

        if !self.globals.is_empty() {
            output.push('\n');
        }

        for function in self.functions.values() {
            output.push_str(&format!("function {}(", function.name));
            for (i, (param_name, param_type)) in function.params.iter().enumerate() {
                if i > 0 {
                    output.push_str(", ");
                }
                output.push_str(&format!("{param_name}: {param_type:?}"));
            }
            output.push_str(&format!(") -> {:?} {{\n", function.return_type));

            for instr in &function.instructions {
                output.push_str(&format!("  {instr:?}\n"));
            }

            output.push_str("}\n\n");
        }

        output
    }
}

/// IR generator for converting AST to IR.
pub struct IRGenerator {
    module: IRModule,
    current_function: Option<String>,
    temp_counter: usize,
    label_counter: usize,
}

impl IRGenerator {
    /// Create a new IR generator.
    pub fn new(module_name: String) -> Self {
        Self {
            module: IRModule::new(module_name),
            current_function: None,
            temp_counter: 0,
            label_counter: 0,
        }
    }

    /// Get the generated module.
    pub fn module(&self) -> &IRModule {
        &self.module
    }

    /// Take ownership of the generated module.
    pub fn into_module(self) -> IRModule {
        self.module
    }

    /// Generate a new temporary name.
    pub fn new_temp(&mut self) -> String {
        let name = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    /// Generate a new label name.
    pub fn new_label(&mut self) -> String {
        let name = format!("L{}", self.label_counter);
        self.label_counter += 1;
        name
    }

    /// Start generating a function.
    pub fn start_function(
        &mut self,
        name: String,
        params: Vec<(String, IRType)>,
        return_type: IRType,
    ) {
        let function = IRFunction::new(name.clone(), params, return_type);
        self.module.add_function(function);
        self.current_function = Some(name);
    }

    /// Finish generating the current function.
    pub fn finish_function(&mut self) {
        self.current_function = None;
    }

    /// Add an instruction to the current function.
    pub fn add_instruction(&mut self, instr: IRInstruction) {
        if let Some(func_name) = &self.current_function {
            if let Some(function) = self.module.functions.get_mut(func_name) {
                function.add_instruction(instr);
            }
        }
    }

    /// Generate IR for a binary operation.
    pub fn gen_binary_op(&mut self, op: BinaryOp, left: IRValue, right: IRValue) -> IRValue {
        let result = self.new_temp();
        self.add_instruction(IRInstruction::BinaryOp {
            op,
            left,
            right,
            result: result.clone(),
        });
        IRValue::Register(result)
    }

    /// Generate IR for a function call.
    pub fn gen_call(
        &mut self,
        function: String,
        args: Vec<IRValue>,
        has_return: bool,
    ) -> Option<IRValue> {
        if has_return {
            let result = self.new_temp();
            self.add_instruction(IRInstruction::Call {
                function,
                args,
                result: Some(result.clone()),
            });
            Some(IRValue::Register(result))
        } else {
            self.add_instruction(IRInstruction::Call {
                function,
                args,
                result: None,
            });
            None
        }
    }

    /// Generate IR for a return statement.
    pub fn gen_return(&mut self, value: Option<IRValue>) {
        self.add_instruction(IRInstruction::Return { value });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_op_parse() {
        assert_eq!(BinaryOp::parse_op("+"), Some(BinaryOp::Add));
        assert_eq!(BinaryOp::parse_op("-"), Some(BinaryOp::Sub));
        assert_eq!(BinaryOp::parse_op("=="), Some(BinaryOp::Eq));
        assert_eq!(BinaryOp::parse_op("invalid"), None);
    }

    #[test]
    fn test_ir_type_size() {
        assert_eq!(IRType::I32.size_bytes(), 4);
        assert_eq!(IRType::I64.size_bytes(), 8);
        assert_eq!(IRType::F64.size_bytes(), 8);
        assert_eq!(IRType::Void.size_bytes(), 0);
    }

    #[test]
    fn test_ir_type_predicates() {
        assert!(IRType::I32.is_numeric());
        assert!(IRType::I32.is_integer());
        assert!(IRType::F64.is_numeric());
        assert!(!IRType::F64.is_integer());
        assert!(!IRType::Void.is_numeric());
    }

    #[test]
    fn test_ir_function() {
        let mut func = IRFunction::new(
            "test".to_string(),
            vec![("x".to_string(), IRType::I32)],
            IRType::I32,
        );

        assert_eq!(func.instruction_count(), 0);

        func.add_instruction(IRInstruction::Return {
            value: Some(IRValue::Variable("x".to_string())),
        });

        assert_eq!(func.instruction_count(), 1);
    }

    #[test]
    fn test_ir_module() {
        let mut module = IRModule::new("test_module".to_string());

        let func = IRFunction::new("main".to_string(), vec![], IRType::I32);

        module.add_function(func);
        assert!(module.get_function("main").is_some());
        assert!(module.get_function("nonexistent").is_none());
    }

    #[test]
    fn test_ir_generator() {
        let mut gen = IRGenerator::new("test".to_string());

        gen.start_function("main".to_string(), vec![], IRType::I32);
        gen.gen_return(Some(IRValue::ConstInt(42)));
        gen.finish_function();

        let module = gen.module();
        assert!(module.get_function("main").is_some());
    }

    #[test]
    fn test_ir_generator_binary_op() {
        let mut gen = IRGenerator::new("test".to_string());

        gen.start_function("add".to_string(), vec![], IRType::I32);
        let result = gen.gen_binary_op(BinaryOp::Add, IRValue::ConstInt(1), IRValue::ConstInt(2));
        gen.gen_return(Some(result));
        gen.finish_function();

        let module = gen.module();
        let func = module.get_function("add").unwrap();
        assert_eq!(func.instruction_count(), 2);
    }

    #[test]
    fn test_ir_generator_temp_names() {
        let mut gen = IRGenerator::new("test".to_string());
        assert_eq!(gen.new_temp(), "t0");
        assert_eq!(gen.new_temp(), "t1");
        assert_eq!(gen.new_temp(), "t2");
    }

    #[test]
    fn test_ir_generator_label_names() {
        let mut gen = IRGenerator::new("test".to_string());
        assert_eq!(gen.new_label(), "L0");
        assert_eq!(gen.new_label(), "L1");
        assert_eq!(gen.new_label(), "L2");
    }

    #[test]
    fn test_ir_module_print() {
        let mut module = IRModule::new("test".to_string());
        let func = IRFunction::new("main".to_string(), vec![], IRType::I32);
        module.add_function(func);

        let output = module.print();
        assert!(output.contains("module test"));
        assert!(output.contains("function main"));
    }
}
