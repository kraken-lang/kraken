//! Kraken IR Type Definitions
//!
//! Core data structures for the intermediate representation.

use crate::lexer::token::Operator;
use std::fmt;

/// A unique identifier for IR values (registers, temporaries).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

/// A unique identifier for basic blocks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

/// IR type representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrType {
    Void,
    Int,
    Float,
    Bool,
    String,
    Bytes,
    VecInt,
    VecString,
    VecBytes,
    MapStringInt,
    MapStringString,
    Array {
        element: Box<IrType>,
        size: Option<usize>,
    },
    Pointer(Box<IrType>),
    Struct(String),
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::Void => write!(f, "void"),
            IrType::Int => write!(f, "i64"),
            IrType::Float => write!(f, "f64"),
            IrType::Bool => write!(f, "i1"),
            IrType::String => write!(f, "str"),
            IrType::Bytes => write!(f, "bytes"),
            IrType::VecInt => write!(f, "VecInt"),
            IrType::VecString => write!(f, "VecString"),
            IrType::VecBytes => write!(f, "VecBytes"),
            IrType::MapStringInt => write!(f, "MapStringInt"),
            IrType::MapStringString => write!(f, "MapStringString"),
            IrType::Array { element, size } => {
                if let Some(s) = size {
                    write!(f, "[{}; {}]", element, s)
                } else {
                    write!(f, "[{}]", element)
                }
            }
            IrType::Pointer(inner) => write!(f, "*{}", inner),
            IrType::Struct(name) => write!(f, "%{}", name),
        }
    }
}

/// IR value - either a constant or a reference to a computed value.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum IrValue {
    /// Reference to a computed value (register/temporary).
    Register(ValueId),
    /// Integer constant.
    ConstInt(i64),
    /// Float constant.
    ConstFloat(f64),
    /// Boolean constant.
    ConstBool(bool),
    /// String constant.
    ConstString(String),
    /// Null pointer constant.
    Null,
    /// Named variable reference.
    Variable(String),
    /// Function reference.
    Function(String),
}

impl fmt::Display for IrValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValue::Register(id) => write!(f, "{}", id),
            IrValue::ConstInt(v) => write!(f, "{}", v),
            IrValue::ConstFloat(v) => write!(f, "{:.6}", v),
            IrValue::ConstBool(v) => write!(f, "{}", v),
            IrValue::ConstString(s) => write!(f, "\"{}\"", s.escape_default()),
            IrValue::Null => write!(f, "null"),
            IrValue::Variable(name) => write!(f, "${}", name),
            IrValue::Function(name) => write!(f, "@{}", name),
        }
    }
}

/// IR instruction - a single operation in the IR.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum IrInstruction {
    /// Allocate local variable: %dest = alloca <type>
    Alloca {
        dest: ValueId,
        ty: IrType,
        name: String,
    },

    /// Store value to memory: store <value>, <ptr>
    Store { value: IrValue, ptr: IrValue },

    /// Load value from memory: %dest = load <ptr>
    Load {
        dest: ValueId,
        ptr: IrValue,
        ty: IrType,
    },

    /// Binary operation: %dest = <op> <left>, <right>
    BinaryOp {
        dest: ValueId,
        op: Operator,
        left: IrValue,
        right: IrValue,
        ty: IrType,
    },

    /// Unary operation: %dest = <op> <operand>
    UnaryOp {
        dest: ValueId,
        op: Operator,
        operand: IrValue,
        ty: IrType,
    },

    /// Function call: %dest = call @<func>(<args>)
    Call {
        dest: Option<ValueId>,
        func: String,
        args: Vec<IrValue>,
        ret_ty: IrType,
    },

    /// Return from function: ret <value>
    Return { value: Option<IrValue> },

    /// Unconditional branch: br <block>
    Branch { target: BlockId },

    /// Conditional branch: br <cond>, <then_block>, <else_block>
    CondBranch {
        cond: IrValue,
        then_block: BlockId,
        else_block: BlockId,
    },

    /// Phi node for SSA: %dest = phi [<val1>, <block1>], [<val2>, <block2>], ...
    Phi {
        dest: ValueId,
        ty: IrType,
        incoming: Vec<(IrValue, BlockId)>,
    },

    /// Get element pointer: %dest = gep <ptr>, <indices>
    GetElementPtr {
        dest: ValueId,
        ptr: IrValue,
        indices: Vec<IrValue>,
        ty: IrType,
    },

    /// Member access: %dest = extractvalue <ptr>, <field_idx>
    ExtractValue {
        dest: ValueId,
        ptr: IrValue,
        field_idx: u32,
        ty: IrType,
    },

    /// Insert value into struct: %dest = insertvalue <struct>, <value>, <field_idx>
    InsertValue {
        dest: ValueId,
        struct_val: IrValue,
        value: IrValue,
        field_idx: u32,
    },
}

impl fmt::Display for IrInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrInstruction::Alloca { dest, ty, name } => {
                write!(f, "    {} = alloca {} ; {}", dest, ty, name)
            }
            IrInstruction::Store { value, ptr } => {
                write!(f, "    store {}, {}", value, ptr)
            }
            IrInstruction::Load { dest, ptr, ty } => {
                write!(f, "    {} = load {} from {}", dest, ty, ptr)
            }
            IrInstruction::BinaryOp {
                dest,
                op,
                left,
                right,
                ty,
            } => {
                write!(f, "    {} = {:?} {} {}, {}", dest, op, ty, left, right)
            }
            IrInstruction::UnaryOp {
                dest,
                op,
                operand,
                ty,
            } => {
                write!(f, "    {} = {:?} {} {}", dest, op, ty, operand)
            }
            IrInstruction::Call {
                dest,
                func,
                args,
                ret_ty,
            } => {
                let args_str: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                if let Some(d) = dest {
                    write!(
                        f,
                        "    {} = call {} @{}({})",
                        d,
                        ret_ty,
                        func,
                        args_str.join(", ")
                    )
                } else {
                    write!(f, "    call void @{}({})", func, args_str.join(", "))
                }
            }
            IrInstruction::Return { value } => {
                if let Some(v) = value {
                    write!(f, "    ret {}", v)
                } else {
                    write!(f, "    ret void")
                }
            }
            IrInstruction::Branch { target } => {
                write!(f, "    br {}", target)
            }
            IrInstruction::CondBranch {
                cond,
                then_block,
                else_block,
            } => {
                write!(f, "    br {}, {}, {}", cond, then_block, else_block)
            }
            IrInstruction::Phi { dest, ty, incoming } => {
                let pairs: Vec<String> = incoming
                    .iter()
                    .map(|(v, b)| format!("[{}, {}]", v, b))
                    .collect();
                write!(f, "    {} = phi {} {}", dest, ty, pairs.join(", "))
            }
            IrInstruction::GetElementPtr {
                dest,
                ptr,
                indices,
                ty,
            } => {
                let idx_str: Vec<String> = indices.iter().map(|i| i.to_string()).collect();
                write!(
                    f,
                    "    {} = gep {} {}, {}",
                    dest,
                    ty,
                    ptr,
                    idx_str.join(", ")
                )
            }
            IrInstruction::ExtractValue {
                dest,
                ptr,
                field_idx,
                ty,
            } => {
                write!(
                    f,
                    "    {} = extractvalue {} {}, {}",
                    dest, ty, ptr, field_idx
                )
            }
            IrInstruction::InsertValue {
                dest,
                struct_val,
                value,
                field_idx,
            } => {
                write!(
                    f,
                    "    {} = insertvalue {}, {}, {}",
                    dest, struct_val, value, field_idx
                )
            }
        }
    }
}

/// A basic block - a sequence of instructions with a single entry and exit.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct IrBlock {
    pub id: BlockId,
    pub name: String,
    pub instructions: Vec<IrInstruction>,
}

impl IrBlock {
    pub fn new(id: BlockId, name: String) -> Self {
        Self {
            id,
            name,
            instructions: Vec::new(),
        }
    }
}

impl fmt::Display for IrBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}

/// IR function parameter.
#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
}

/// IR function definition.
#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<IrParam>,
    pub return_type: IrType,
    pub blocks: Vec<IrBlock>,
    pub is_public: bool,
}

impl IrFunction {
    pub fn new(name: String, params: Vec<IrParam>, return_type: IrType, is_public: bool) -> Self {
        Self {
            name,
            params,
            return_type,
            blocks: Vec::new(),
            is_public,
        }
    }
}

impl fmt::Display for IrFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let vis = if self.is_public { "pub " } else { "" };
        let params: Vec<String> = self
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name, p.ty))
            .collect();
        writeln!(
            f,
            "{}fn @{}({}) -> {} {{",
            vis,
            self.name,
            params.join(", "),
            self.return_type
        )?;
        for block in &self.blocks {
            write!(f, "{}", block)?;
        }
        writeln!(f, "}}")
    }
}

/// IR struct definition.
#[derive(Debug, Clone)]
pub struct IrStruct {
    pub name: String,
    pub fields: Vec<(String, IrType)>,
    pub is_public: bool,
}

impl fmt::Display for IrStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let vis = if self.is_public { "pub " } else { "" };
        writeln!(f, "{}struct %{} {{", vis, self.name)?;
        for (name, ty) in &self.fields {
            writeln!(f, "    {}: {},", name, ty)?;
        }
        writeln!(f, "}}")
    }
}

/// IR program - the top-level container.
#[derive(Debug, Clone)]
pub struct IrProgram {
    pub structs: Vec<IrStruct>,
    pub functions: Vec<IrFunction>,
}

impl IrProgram {
    pub fn new() -> Self {
        Self {
            structs: Vec::new(),
            functions: Vec::new(),
        }
    }
}

impl Default for IrProgram {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for IrProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "; Kraken IR")?;
        writeln!(f)?;
        for s in &self.structs {
            write!(f, "{}", s)?;
            writeln!(f)?;
        }
        for func in &self.functions {
            write!(f, "{}", func)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_id_display() {
        assert_eq!(format!("{}", ValueId(0)), "%0");
        assert_eq!(format!("{}", ValueId(42)), "%42");
    }

    #[test]
    fn test_block_id_display() {
        assert_eq!(format!("{}", BlockId(0)), "bb0");
        assert_eq!(format!("{}", BlockId(5)), "bb5");
    }

    #[test]
    fn test_ir_type_display() {
        assert_eq!(format!("{}", IrType::Int), "i64");
        assert_eq!(format!("{}", IrType::Bool), "i1");
        assert_eq!(
            format!("{}", IrType::Pointer(Box::new(IrType::Int))),
            "*i64"
        );
    }

    #[test]
    fn test_ir_value_display() {
        assert_eq!(format!("{}", IrValue::ConstInt(42)), "42");
        assert_eq!(format!("{}", IrValue::Variable("x".to_string())), "$x");
        assert_eq!(
            format!("{}", IrValue::Function("main".to_string())),
            "@main"
        );
    }
}
