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
    Module {
        path: Vec<String>,
    },

    Import {
        path: Vec<String>,
    },

    /// Variable declaration: let x = expr; or let (x, y) = tuple;
    VariableDeclaration {
        pattern: Pattern,
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
        generic_params: Vec<String>,
        where_constraints: Vec<WhereConstraint>,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Block,
        is_async: bool,
        is_unsafe: bool,
        is_public: bool,
        is_variadic: bool,
    },

    /// Struct declaration
    StructDeclaration {
        name: String,
        generic_params: Vec<String>,
        where_constraints: Vec<WhereConstraint>,
        fields: Vec<StructField>,
        is_public: bool,
        repr: Option<StructRepr>,
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

    /// Enum declaration
    EnumDeclaration {
        name: String,
        generic_params: Vec<String>,
        where_constraints: Vec<WhereConstraint>,
        variants: Vec<(String, Option<EnumVariantPayload>)>, // (variant_name, optional_payload)
        is_public: bool,
    },

    /// Union declaration
    #[allow(dead_code)]
    UnionDeclaration {
        name: String,
        fields: Vec<StructField>,
        is_public: bool,
    },

    /// Type alias: type MyInt = int;
    TypeAlias {
        name: String,
        generic_params: Vec<String>,
        target_type: Type,
        is_public: bool,
    },

    /// Impl block: impl TypeName { ... }
    ImplBlock {
        type_name: String,
        generic_params: Vec<String>,
        methods: Vec<Statement>, // FunctionDeclarations
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

    /// For loop (C-style)
    For {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Block,
    },

    /// For-in loop (iterator-based): for (x in range) { ... }
    ForIn {
        variable: String,
        iterable: Expression,
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

    /// Unsafe block
    Unsafe {
        block: Block,
    },

    /// Trait declaration: trait Name { ... }
    TraitDeclaration {
        name: String,
        generic_params: Vec<String>,
        super_traits: Vec<String>,
        methods: Vec<TraitMethod>,
        associated_types: Vec<AssociatedType>,
        is_public: bool,
    },

    /// Trait implementation: impl TraitName for TypeName { ... }
    TraitImpl {
        trait_name: String,
        type_name: String,
        generic_params: Vec<String>,
        where_constraints: Vec<WhereConstraint>,
        methods: Vec<Statement>,
    },

    /// Macro declaration: macro_rules! name { ... }
    #[allow(dead_code)]
    MacroDeclaration {
        name: String,
        rules: Vec<MacroRule>,
    },

    /// Const function declaration: const fn name() -> T { ... }
    #[allow(dead_code)]
    ConstFunctionDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Block,
        is_public: bool,
    },

    /// Static assertion: static_assert!(condition, "message")
    #[allow(dead_code)]
    StaticAssert {
        condition: Expression,
        message: String,
    },

    /// Attribute: #[derive(Clone, Debug)]
    #[allow(dead_code)]
    Attribute {
        name: String,
        args: Vec<String>,
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
        type_args: Option<Vec<Type>>,
        arguments: Vec<Expression>,
    },

    /// Array literal
    Array { elements: Vec<Expression> },

    /// Array indexing
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },

    /// Slice expression: x[start:end]
    Slice {
        array: Box<Expression>,
        start: Box<Expression>,
        end: Box<Expression>,
    },

    /// Member access (struct.field)
    MemberAccess {
        object: Box<Expression>,
        member: String,
    },

    /// Struct literal (Point { x: 1, y: 2 })
    StructLiteral {
        name: String,
        type_args: Option<Vec<Type>>,
        fields: Vec<(String, Expression)>,
    },

    /// Assignment
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },

    /// Reference (&expr)
    Reference { expression: Box<Expression> },

    /// Dereference (*expr)
    Dereference { expression: Box<Expression> },

    /// Await expression (await expr)
    Await { expression: Box<Expression> },

    /// Spawn expression (spawn { block })
    Spawn { body: Block },

    /// Enum variant expression (EnumName::VariantName or EnumName::VariantName(payload))
    EnumVariant {
        enum_name: String,
        variant_name: String,
        payload: Option<Vec<Expression>>,
    },

    /// Tuple literal expression: (1, "hello", true)
    Tuple { elements: Vec<Expression> },

    /// Tuple indexing: tuple.0, tuple.1
    TupleIndex {
        tuple: Box<Expression>,
        index: usize,
    },

    /// Try expression: expr? (for Result/Option error propagation)
    Try { expression: Box<Expression> },

    /// Range expression: 0..10 or 0..=10
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        inclusive: bool,
    },

    /// Closure/Lambda expression: |x, y| x + y or |x| { ... }
    Closure {
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: ClosureBody,
        is_move: bool, // move keyword for capture by value
    },
}

/// Closure body can be either an expression or a block
#[derive(Debug, Clone, PartialEq)]
pub enum ClosureBody {
    Expression(Box<Expression>),
    Block(Block),
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
    pub pattern: Pattern, // Changed from name: String to support destructuring
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

/// Struct representation attribute for FFI compatibility
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum StructRepr {
    /// #[repr(C)] - C-compatible layout
    C,
    /// #[repr(packed)] - Packed layout (no padding)
    Packed,
    /// #[repr(align(N))] - Specific alignment
    Align(u32),
}

/// Enum variant payload type.
#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantPayload {
    /// Tuple payload: Some(int, string)
    Tuple(Vec<Type>),
    /// Struct payload: Point { x: int, y: int }
    Struct(Vec<(String, Type)>),
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
    pub guard: Option<Expression>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereConstraint {
    pub type_param: String,
    pub trait_name: String,
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

    /// Enum variant pattern: EnumName::Variant or EnumName::Variant(a, b)
    EnumVariant {
        enum_name: String,
        variant_name: String,
        bindings: Vec<String>,
    },

    /// Tuple pattern: (x, y, z)
    Tuple { patterns: Vec<Pattern> },

    /// Range pattern: 0..10 or 0..=10
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        inclusive: bool,
    },

    /// Or pattern: 1 | 2 | 3
    Or { patterns: Vec<Pattern> },

    /// Struct pattern: Point { x, y } or Point { x, .. }
    Struct {
        struct_name: String,
        fields: Vec<(String, Pattern)>,
        partial: bool, // true if using .. to ignore remaining fields
    },
}

/// Type representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Type {
    /// Primitive types
    Int,
    Float,
    Bool,
    String,
    Str,
    Bytes,
    Void,

    /// Container types
    VecInt,
    VecString,
    VecBytes,
    MapStringInt,
    MapStringString,

    /// Slice types (borrowed views)
    SliceInt,
    SliceString,
    SliceBytes,

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

    /// Pointer type
    Pointer {
        inner_type: Box<Type>,
        is_mutable: bool,
    },

    /// Raw pointer type: *const T or *mut T (unsafe)
    RawPointer {
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

    /// Tuple type
    Tuple {
        element_types: Vec<Type>,
    },

    /// Function type: fn(int, int) -> int
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },

    /// Trait object type: dyn Trait or dyn Trait + Send + Sync
    TraitObject {
        trait_name: String,
        bounds: Vec<String>,
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
            Keyword::Str => Some(Type::Str),
            Keyword::Bytes => Some(Type::Bytes),
            Keyword::Void => Some(Type::Void),
            Keyword::VecInt => Some(Type::VecInt),
            Keyword::VecString => Some(Type::VecString),
            Keyword::VecBytes => Some(Type::VecBytes),
            Keyword::MapStringInt => Some(Type::MapStringInt),
            Keyword::MapStringString => Some(Type::MapStringString),
            Keyword::SliceInt => Some(Type::SliceInt),
            Keyword::SliceString => Some(Type::SliceString),
            Keyword::SliceBytes => Some(Type::SliceBytes),
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
            Type::Str => write!(f, "str"),
            Type::Bytes => write!(f, "bytes"),
            Type::Void => write!(f, "void"),
            Type::VecInt => write!(f, "VecInt"),
            Type::VecString => write!(f, "VecString"),
            Type::VecBytes => write!(f, "VecBytes"),
            Type::MapStringInt => write!(f, "MapStringInt"),
            Type::MapStringString => write!(f, "MapStringString"),
            Type::SliceInt => write!(f, "SliceInt"),
            Type::SliceString => write!(f, "SliceString"),
            Type::SliceBytes => write!(f, "SliceBytes"),
            Type::Array { element_type, size } => {
                if let Some(s) = size {
                    write!(f, "[{element_type}; {s}]")
                } else {
                    write!(f, "[{element_type}]")
                }
            }
            Type::Reference {
                inner_type,
                is_mutable,
            } => {
                if *is_mutable {
                    write!(f, "&mut {inner_type}")
                } else {
                    write!(f, "&{inner_type}")
                }
            }
            Type::Pointer {
                inner_type,
                is_mutable,
            } => {
                if *is_mutable {
                    write!(f, "*mut {inner_type}")
                } else {
                    write!(f, "*{inner_type}")
                }
            }
            Type::RawPointer {
                inner_type,
                is_mutable,
            } => {
                if *is_mutable {
                    write!(f, "*mut {inner_type}")
                } else {
                    write!(f, "*const {inner_type}")
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
            Type::Tuple { element_types } => {
                write!(f, "(")?;
                for (i, ty) in element_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ")")
            }
            Type::Function {
                param_types,
                return_type,
            } => {
                write!(f, "fn(")?;
                for (i, ty) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ") -> {return_type}")
            }
            Type::TraitObject { trait_name, bounds } => {
                write!(f, "dyn {trait_name}")?;
                if !bounds.is_empty() {
                    for bound in bounds {
                        write!(f, " + {bound}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

/// Trait method declaration (can be required or provided)
#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Option<Block>, // None for required methods, Some for provided methods
    pub is_async: bool,
}

/// Associated type in a trait
#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedType {
    pub name: String,
    pub bounds: Vec<String>, // Trait bounds on the associated type
}

/// Macro rule for declarative macros
#[derive(Debug, Clone, PartialEq)]
pub struct MacroRule {
    pub pattern: Vec<MacroToken>,
    pub expansion: Vec<MacroToken>,
}

/// Token in a macro pattern or expansion
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum MacroToken {
    Literal(String),
    Variable(String),
    Repetition(Vec<MacroToken>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_from_keyword() {
        assert_eq!(Type::from_keyword(Keyword::Int), Some(Type::Int));
        assert_eq!(Type::from_keyword(Keyword::Float), Some(Type::Float));
        assert_eq!(Type::from_keyword(Keyword::Bool), Some(Type::Bool));
        assert_eq!(Type::from_keyword(Keyword::Bytes), Some(Type::Bytes));
        assert_eq!(Type::from_keyword(Keyword::If), None);
    }

    #[test]
    fn test_type_display() {
        assert_eq!(Type::Int.to_string(), "int");
        assert_eq!(Type::Float.to_string(), "float");
        assert_eq!(Type::Bytes.to_string(), "bytes");
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
