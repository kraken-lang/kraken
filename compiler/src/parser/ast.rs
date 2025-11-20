use crate::lexer::token::{Keyword, Operator};

/// Abstract Syntax Tree root node.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

/// Statement types in Kraken.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Variable declaration: let x = expr;
    VariableDeclaration {
        name: String,
        type_annotation: Option<Type>,
        initializer: Option<Expression>,
        is_mutable: bool,
    },

    /// Constant declaration: const X = expr;
    ConstantDeclaration {
        name: String,
        type_annotation: Option<Type>,
        initializer: Expression,
    },

    /// Function declaration
    FunctionDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Block,
        is_async: bool,
        is_public: bool,
    },

    /// Struct declaration
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
        is_public: bool,
    },

    /// Class declaration
    ClassDeclaration {
        name: String,
        fields: Vec<StructField>,
        methods: Vec<Statement>,
        is_public: bool,
    },

    /// Interface declaration
    InterfaceDeclaration {
        name: String,
        methods: Vec<FunctionSignature>,
    },

    /// Return statement
    Return {
        value: Option<Expression>,
    },

    /// Expression statement
    Expression(Expression),

    /// If statement
    If {
        condition: Expression,
        then_branch: Block,
        else_branch: Option<Block>,
    },

    /// While loop
    While {
        condition: Expression,
        body: Block,
    },

    /// For loop
    For {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Block,
    },

    /// Match statement
    Match {
        expression: Expression,
        arms: Vec<MatchArm>,
    },

    /// Break statement
    Break,

    /// Continue statement
    Continue,

    /// Defer statement
    Defer {
        statement: Box<Statement>,
    },
}

/// Expression types in Kraken.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Integer literal
    IntLiteral(i64),

    /// Float literal
    FloatLiteral(f64),

    /// String literal
    StringLiteral(String),

    /// Boolean literal
    BoolLiteral(bool),

    /// Null literal
    NullLiteral,

    /// Identifier (variable reference)
    Identifier(String),

    /// Binary operation
    Binary {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },

    /// Unary operation
    Unary {
        operator: Operator,
        operand: Box<Expression>,
    },

    /// Function call
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },

    /// Array literal
    Array {
        elements: Vec<Expression>,
    },

    /// Array indexing
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },

    /// Member access (struct.field)
    MemberAccess {
        object: Box<Expression>,
        member: String,
    },

    /// Assignment
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },

    /// Reference (&expr)
    Reference {
        expression: Box<Expression>,
    },

    /// Dereference (*expr)
    Dereference {
        expression: Box<Expression>,
    },
}

/// Code block containing statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

/// Function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
    pub is_reference: bool,
}

/// Struct field.
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub field_type: Type,
    pub is_public: bool,
}

/// Function signature (for interfaces).
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
}

/// Match arm.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

/// Pattern for match expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Literal pattern
    Literal(Expression),
    
    /// Identifier pattern (binds value)
    Identifier(String),
    
    /// Wildcard pattern (_)
    Wildcard,
}

/// Type representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Primitive types
    Int,
    Float,
    Bool,
    String,
    Void,

    /// Array type
    Array {
        element_type: Box<Type>,
        size: Option<usize>,
    },

    /// Reference type
    Reference {
        inner_type: Box<Type>,
        is_mutable: bool,
    },

    /// Custom type (struct, class, etc.)
    Custom(String),

    /// Generic type
    Generic {
        name: String,
        type_params: Vec<Type>,
    },
}

impl Type {
    /// Create a type from a keyword.
    pub fn from_keyword(keyword: Keyword) -> Option<Self> {
        match keyword {
            Keyword::Int => Some(Type::Int),
            Keyword::Float => Some(Type::Float),
            Keyword::Bool => Some(Type::Bool),
            Keyword::String => Some(Type::String),
            Keyword::Void => Some(Type::Void),
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Array { element_type, size } => {
                if let Some(s) = size {
                    write!(f, "[{element_type}; {s}]")
                } else {
                    write!(f, "[{element_type}]")
                }
            }
            Type::Reference { inner_type, is_mutable } => {
                if *is_mutable {
                    write!(f, "&mut {inner_type}")
                } else {
                    write!(f, "&{inner_type}")
                }
            }
            Type::Custom(name) => write!(f, "{name}"),
            Type::Generic { name, type_params } => {
                write!(f, "{name}<")?;
                for (i, param) in type_params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{param}")?;
                }
                write!(f, ">")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_from_keyword() {
        assert_eq!(Type::from_keyword(Keyword::Int), Some(Type::Int));
        assert_eq!(Type::from_keyword(Keyword::Float), Some(Type::Float));
        assert_eq!(Type::from_keyword(Keyword::Bool), Some(Type::Bool));
        assert_eq!(Type::from_keyword(Keyword::If), None);
    }

    #[test]
    fn test_type_display() {
        assert_eq!(Type::Int.to_string(), "int");
        assert_eq!(Type::Float.to_string(), "float");
        assert_eq!(
            Type::Array {
                element_type: Box::new(Type::Int),
                size: Some(10)
            }
            .to_string(),
            "[int; 10]"
        );
    }

    #[test]
    fn test_program_creation() {
        let program = Program::new(vec![]);
        assert_eq!(program.statements.len(), 0);
    }

    #[test]
    fn test_block_creation() {
        let block = Block::new(vec![]);
        assert_eq!(block.statements.len(), 0);
    }
}
