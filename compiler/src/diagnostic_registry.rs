//! Diagnostic code registry for documentation and lookup.
//!
//! This module provides a registry of all diagnostic codes with their
//! descriptions, examples, and suggestions for fixing errors.

#![allow(dead_code)]

use crate::diagnostics::DiagnosticCode;
use std::collections::HashMap;

/// Detailed information about a diagnostic code.
#[derive(Debug, Clone)]
pub struct DiagnosticInfo {
    pub code: DiagnosticCode,
    pub title: &'static str,
    pub description: &'static str,
    pub example: Option<&'static str>,
    pub suggestion: Option<&'static str>,
}

/// Registry of all diagnostic codes with their documentation.
pub struct DiagnosticRegistry {
    entries: HashMap<&'static str, DiagnosticInfo>,
}

impl DiagnosticRegistry {
    /// Create a new diagnostic registry with all codes.
    pub fn new() -> Self {
        let mut registry = Self {
            entries: HashMap::new(),
        };
        registry.register_all();
        registry
    }

    /// Register a diagnostic code with its information.
    fn register(&mut self, info: DiagnosticInfo) {
        self.entries.insert(info.code.code(), info);
    }

    /// Look up a diagnostic code by its string representation.
    pub fn lookup(&self, code: &str) -> Option<&DiagnosticInfo> {
        self.entries.get(code)
    }

    /// Get all registered diagnostic codes.
    pub fn all_codes(&self) -> Vec<&DiagnosticInfo> {
        self.entries.values().collect()
    }

    /// Register all diagnostic codes.
    fn register_all(&mut self) {
        // Lexer errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0001_UnexpectedCharacter,
            title: "Unexpected Character",
            description:
                "The lexer encountered a character that is not valid in Kraken source code.",
            example: Some("let x = @;  // '@' is not a valid character"),
            suggestion: Some("Remove or replace the unexpected character with valid syntax."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0002_UnterminatedStringLiteral,
            title: "Unterminated String Literal",
            description: "A string literal was started but never closed with a closing quote.",
            example: Some("let s = \"hello;  // Missing closing quote"),
            suggestion: Some("Add a closing quote to terminate the string literal."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0004_InvalidNumberFormat,
            title: "Invalid Number Format",
            description: "The number literal has an invalid format that cannot be parsed.",
            example: Some("let x = 123abc;  // Invalid number format"),
            suggestion: Some("Ensure the number follows valid integer or floating-point syntax."),
        });

        // Parser errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1000_UnexpectedToken,
            title: "Unexpected Token",
            description: "The parser encountered a token that was not expected in this context.",
            example: Some("fn foo() { let x = ; }  // Unexpected semicolon"),
            suggestion: Some("Check the syntax and ensure all expressions are complete."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1001_ExpectedToken,
            title: "Expected Token",
            description: "The parser expected a specific token but found something else.",
            example: Some("fn foo() { let x = 5  // Missing semicolon"),
            suggestion: Some("Add the expected token to complete the syntax."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1010_MissingClosingBrace,
            title: "Missing Closing Brace",
            description: "A block was opened with '{' but never closed with '}'.",
            example: Some("fn foo() { let x = 5;  // Missing closing brace"),
            suggestion: Some("Add a closing brace '}' to complete the block."),
        });

        // Type errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2000_TypeMismatch,
            title: "Type Mismatch",
            description: "The type of an expression does not match the expected type.",
            example: Some("let x: int = \"hello\";  // Expected int, found string"),
            suggestion: Some(
                "Ensure the expression has the correct type or add a type conversion.",
            ),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2001_UndefinedType,
            title: "Undefined Type",
            description: "A type name was used but has not been defined.",
            example: Some("let x: MyType = 5;  // MyType is not defined"),
            suggestion: Some("Define the type or check for typos in the type name."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2005_ArityMismatch,
            title: "Arity Mismatch",
            description: "A function was called with the wrong number of arguments.",
            example: Some("fn foo(x: int) { }\nfoo(1, 2);  // Expected 1 argument, found 2"),
            suggestion: Some(
                "Provide the correct number of arguments to match the function signature.",
            ),
        });

        // Resolution errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3000_UndefinedVariable,
            title: "Undefined Variable",
            description: "A variable was used but has not been defined in the current scope.",
            example: Some("let y = x + 1;  // x is not defined"),
            suggestion: Some("Define the variable before using it or check for typos."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3001_UndefinedFunction,
            title: "Undefined Function",
            description: "A function was called but has not been defined.",
            example: Some("let result = foo();  // foo is not defined"),
            suggestion: Some("Define the function or import it from a module."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3008_DuplicateDefinition,
            title: "Duplicate Definition",
            description: "A name was defined more than once in the same scope.",
            example: Some("let x = 5;\nlet x = 10;  // x is already defined"),
            suggestion: Some("Use a different name or remove the duplicate definition."),
        });

        // Borrow checking errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4000_UseAfterMove,
            title: "Use After Move",
            description: "A value was used after it was moved to another location.",
            example: Some("let x = vec![1, 2, 3];\nlet y = x;\nprintln(x);  // x was moved"),
            suggestion: Some("Clone the value before moving it or use a reference instead."),
        });

        // Codegen errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA5000_CodegenFailure,
            title: "Code Generation Failure",
            description: "The code generator failed to produce output for this code.",
            example: None,
            suggestion: Some("This is likely a compiler bug. Please report it."),
        });

        // I/O errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9000_FileNotFound,
            title: "File Not Found",
            description: "The specified file could not be found.",
            example: Some("kraken compile missing.kr"),
            suggestion: Some("Check that the file path is correct and the file exists."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9001_InvalidFileExtension,
            title: "Invalid File Extension",
            description: "The file has an invalid extension. Kraken files must use .kr or .krak.",
            example: Some("kraken compile file.txt"),
            suggestion: Some("Rename the file to use .kr or .krak extension."),
        });

        // Additional lexer errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0003_UnterminatedCharLiteral,
            title: "Unterminated Character Literal",
            description: "A character literal was started but never closed.",
            example: Some("let c = 'a;  // Missing closing quote"),
            suggestion: Some("Add a closing quote to terminate the character literal."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0005_InvalidEscapeSequence,
            title: "Invalid Escape Sequence",
            description: "An escape sequence in a string or character literal is not valid.",
            example: Some(r#"let s = "hello\x";"#),
            suggestion: Some("Use a valid escape sequence like \\n, \\t, \\r, \\\\, or \\\"."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0006_InvalidUnicodeEscape,
            title: "Invalid Unicode Escape",
            description: "A Unicode escape sequence has an invalid format.",
            example: Some(r#"let s = "\u{GGGG}";"#),
            suggestion: Some("Use valid hexadecimal digits in Unicode escape sequences."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0007_UnterminatedBlockComment,
            title: "Unterminated Block Comment",
            description: "A block comment was started with /* but never closed with */.",
            example: Some("/* This comment is not closed"),
            suggestion: Some("Add */ to close the block comment."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0008_InvalidFloatLiteral,
            title: "Invalid Float Literal",
            description: "A floating-point literal has an invalid format.",
            example: Some("let x = 3.14.15;"),
            suggestion: Some("Ensure the float follows valid syntax like 3.14 or 1.5e10."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0009_IntegerLiteralTooLarge,
            title: "Integer Literal Too Large",
            description: "An integer literal exceeds the maximum value for its type.",
            example: Some("let x = 99999999999999999999999999999;"),
            suggestion: Some("Use a smaller value or a larger integer type."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0010_InvalidBinaryLiteral,
            title: "Invalid Binary Literal",
            description: "A binary literal contains invalid digits.",
            example: Some("let x = 0b1012;  // '2' is not a binary digit"),
            suggestion: Some("Binary literals can only contain 0 and 1."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0011_InvalidOctalLiteral,
            title: "Invalid Octal Literal",
            description: "An octal literal contains invalid digits.",
            example: Some("let x = 0o789;  // '8' and '9' are not octal digits"),
            suggestion: Some("Octal literals can only contain digits 0-7."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0012_InvalidHexLiteral,
            title: "Invalid Hexadecimal Literal",
            description: "A hexadecimal literal contains invalid characters.",
            example: Some("let x = 0xGHI;  // 'G', 'H', 'I' are not hex digits"),
            suggestion: Some("Hexadecimal literals can only contain 0-9 and A-F."),
        });

        // Additional parser errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1002_UnexpectedEof,
            title: "Unexpected End of File",
            description: "The parser reached the end of the file unexpectedly.",
            example: Some("fn foo() {"),
            suggestion: Some("Complete the syntax before the end of the file."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1003_InvalidSyntax,
            title: "Invalid Syntax",
            description: "The syntax is not valid in this context.",
            example: Some("let = 5;  // Missing variable name"),
            suggestion: Some("Check the language syntax and fix the invalid construct."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1004_MissingExpression,
            title: "Missing Expression",
            description: "An expression was expected but not found.",
            example: Some("let x = ;"),
            suggestion: Some("Provide an expression after the assignment operator."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1005_MissingStatement,
            title: "Missing Statement",
            description: "A statement was expected but not found.",
            example: Some("fn foo() { }  // Empty function body"),
            suggestion: Some("Add statements to the function body or use an expression."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1006_InvalidPattern,
            title: "Invalid Pattern",
            description: "The pattern in a match or let binding is not valid.",
            example: Some("match x { 5 + 5 => {} }  // Cannot use expressions in patterns"),
            suggestion: Some(
                "Use valid pattern syntax like literals, identifiers, or destructuring.",
            ),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1007_InvalidTypeAnnotation,
            title: "Invalid Type Annotation",
            description: "The type annotation has invalid syntax.",
            example: Some("let x: int + string = 5;"),
            suggestion: Some("Use valid type syntax."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1008_MissingFunctionBody,
            title: "Missing Function Body",
            description: "A function declaration is missing its body.",
            example: Some("fn foo();  // Missing function body"),
            suggestion: Some(
                "Add a function body with braces { } or use a semicolon for extern functions.",
            ),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1009_InvalidFunctionSignature,
            title: "Invalid Function Signature",
            description: "The function signature has invalid syntax.",
            example: Some("fn foo(x) { }  // Missing type annotation"),
            suggestion: Some("Provide type annotations for all parameters."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1011_MissingClosingParen,
            title: "Missing Closing Parenthesis",
            description: "An opening parenthesis was not matched with a closing parenthesis.",
            example: Some("let x = (1 + 2;"),
            suggestion: Some("Add a closing parenthesis ')' to match the opening one."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1012_MissingClosingBracket,
            title: "Missing Closing Bracket",
            description: "An opening bracket was not matched with a closing bracket.",
            example: Some("let arr = [1, 2, 3;"),
            suggestion: Some("Add a closing bracket ']' to match the opening one."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1013_InvalidAttribute,
            title: "Invalid Attribute",
            description: "An attribute has invalid syntax or is not recognized.",
            example: Some("#[unknown_attr] fn foo() { }"),
            suggestion: Some("Check the attribute name and syntax."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1014_DuplicateParameter,
            title: "Duplicate Parameter",
            description: "A function has multiple parameters with the same name.",
            example: Some("fn foo(x: int, x: string) { }"),
            suggestion: Some("Use unique names for all parameters."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1015_InvalidVisibility,
            title: "Invalid Visibility",
            description: "A visibility modifier is used in an invalid context.",
            example: Some("pub let x = 5;  // Cannot use pub on local variables"),
            suggestion: Some("Remove the visibility modifier or use it on a valid item."),
        });

        // Additional type errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2002_TypeNotInferred,
            title: "Type Not Inferred",
            description: "The compiler could not infer the type of this expression.",
            example: Some("let x = [];  // Cannot infer element type"),
            suggestion: Some("Add an explicit type annotation."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2003_RecursiveType,
            title: "Recursive Type",
            description: "A type definition is recursive without indirection.",
            example: Some("struct Node { next: Node }"),
            suggestion: Some("Use a pointer or reference to break the recursion."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2004_InvalidCast,
            title: "Invalid Cast",
            description: "A type cast is not valid between these types.",
            example: Some("let x = \"hello\" as int;"),
            suggestion: Some("Use a valid conversion method or check if the cast is supported."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2006_MissingTypeParameter,
            title: "Missing Type Parameter",
            description: "A generic type is missing required type parameters.",
            example: Some("let x: Vec = vec![];  // Vec requires a type parameter"),
            suggestion: Some("Provide all required type parameters like Vec<int>."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2007_TooManyTypeParameters,
            title: "Too Many Type Parameters",
            description: "Too many type parameters were provided for this generic type.",
            example: Some("let x: Option<int, string> = None;  // Option takes one parameter"),
            suggestion: Some("Remove the extra type parameters."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2008_TypeParameterBoundNotSatisfied,
            title: "Type Parameter Bound Not Satisfied",
            description: "A type parameter does not satisfy its trait bounds.",
            example: Some(
                "fn foo<T: Display>(x: T) { }\nfoo(vec![]);  // Vec doesn't implement Display",
            ),
            suggestion: Some("Use a type that implements the required traits."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2009_TraitNotImplemented,
            title: "Trait Not Implemented",
            description: "A type does not implement a required trait.",
            example: Some("let x: dyn Display = 5;  // int doesn't implement Display"),
            suggestion: Some("Implement the trait for this type or use a different type."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2010_AmbiguousType,
            title: "Ambiguous Type",
            description: "The type cannot be determined due to ambiguity.",
            example: Some("let x = Default::default();  // Which type?"),
            suggestion: Some("Add an explicit type annotation to resolve the ambiguity."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2011_InvalidTypeInContext,
            title: "Invalid Type in Context",
            description: "This type cannot be used in this context.",
            example: Some("fn foo() -> ! { return 5; }  // Cannot return from never type"),
            suggestion: Some("Use a valid type for this context."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2012_CannotInferType,
            title: "Cannot Infer Type",
            description: "The compiler cannot infer the type without more information.",
            example: Some("let x = None;  // Cannot infer T in Option<T>"),
            suggestion: Some("Provide a type annotation or more context."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2013_CyclicTypeAlias,
            title: "Cyclic Type Alias",
            description: "A type alias refers to itself directly or indirectly.",
            example: Some("type A = B;\ntype B = A;"),
            suggestion: Some("Break the cycle by using a concrete type."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2014_InvalidSelfType,
            title: "Invalid Self Type",
            description: "The Self type is used in an invalid context.",
            example: Some("fn foo() -> Self { }  // Self outside of impl block"),
            suggestion: Some("Use Self only inside impl blocks."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2015_MismatchedReturnType,
            title: "Mismatched Return Type",
            description: "The return type does not match the function signature.",
            example: Some("fn foo() -> int { \"hello\" }"),
            suggestion: Some("Return a value of the correct type."),
        });

        // Additional resolution errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3002_UndefinedModule,
            title: "Undefined Module",
            description: "A module was referenced but has not been defined.",
            example: Some("use my_module::foo;  // my_module not found"),
            suggestion: Some("Define the module or check the module path."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3003_UndefinedStruct,
            title: "Undefined Struct",
            description: "A struct was referenced but has not been defined.",
            example: Some("let x = MyStruct { };  // MyStruct not found"),
            suggestion: Some("Define the struct or import it from a module."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3004_UndefinedEnum,
            title: "Undefined Enum",
            description: "An enum was referenced but has not been defined.",
            example: Some("let x = MyEnum::Variant;  // MyEnum not found"),
            suggestion: Some("Define the enum or import it from a module."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3005_UndefinedTrait,
            title: "Undefined Trait",
            description: "A trait was referenced but has not been defined.",
            example: Some("impl MyTrait for Foo { }  // MyTrait not found"),
            suggestion: Some("Define the trait or import it from a module."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3006_AmbiguousName,
            title: "Ambiguous Name",
            description: "A name could refer to multiple items.",
            example: Some("use mod1::foo;\nuse mod2::foo;\nfoo();  // Which foo?"),
            suggestion: Some("Use a fully qualified path to disambiguate."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3007_PrivateAccess,
            title: "Private Access",
            description: "Attempted to access a private item from outside its module.",
            example: Some("mod foo { fn bar() { } }\nfoo::bar();  // bar is private"),
            suggestion: Some(
                "Make the item public with 'pub' or access it from within the module.",
            ),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3009_CircularDependency,
            title: "Circular Dependency",
            description: "Modules have a circular dependency.",
            example: Some("mod a { use super::b; }\nmod b { use super::a; }"),
            suggestion: Some("Refactor to remove the circular dependency."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3010_InvalidImport,
            title: "Invalid Import",
            description: "An import statement has invalid syntax or refers to a non-existent item.",
            example: Some("use std::nonexistent;"),
            suggestion: Some("Check the import path and ensure the item exists."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3011_ModuleNotFound,
            title: "Module Not Found",
            description: "The specified module file could not be found.",
            example: Some("mod missing;  // missing.kr not found"),
            suggestion: Some("Create the module file or check the module path."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3012_CannotResolveSymbol,
            title: "Cannot Resolve Symbol",
            description: "A symbol could not be resolved to any definition.",
            example: Some("let x = unknown_symbol;"),
            suggestion: Some("Check for typos or ensure the symbol is defined."),
        });

        // Additional borrow checking errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4001_UseAfterFree,
            title: "Use After Free",
            description: "A value was used after it was freed.",
            example: Some("let x = Box::new(5);\ndrop(x);\nprintln(x);"),
            suggestion: Some("Do not use values after they have been freed."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4002_DoubleFree,
            title: "Double Free",
            description: "A value was freed more than once.",
            example: Some("let x = Box::new(5);\ndrop(x);\ndrop(x);"),
            suggestion: Some("Ensure values are only freed once."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4003_BorrowWhileMutable,
            title: "Borrow While Mutable",
            description: "Cannot borrow a value while it is mutably borrowed.",
            example: Some("let mut x = 5;\nlet y = &mut x;\nlet z = &x;  // Cannot borrow"),
            suggestion: Some("Ensure mutable borrows do not overlap with other borrows."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4004_MutableBorrowWhileBorrowed,
            title: "Mutable Borrow While Borrowed",
            description: "Cannot mutably borrow a value while it is borrowed.",
            example: Some("let mut x = 5;\nlet y = &x;\nlet z = &mut x;  // Cannot borrow mutably"),
            suggestion: Some("Ensure immutable borrows end before creating mutable borrows."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4005_CannotMoveOutOfBorrow,
            title: "Cannot Move Out of Borrow",
            description: "Cannot move a value out of a borrowed reference.",
            example: Some("let x = &vec![1, 2, 3];\nlet y = *x;  // Cannot move"),
            suggestion: Some("Clone the value or use a reference instead of moving."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4006_LifetimeTooShort,
            title: "Lifetime Too Short",
            description: "A reference does not live long enough.",
            example: Some("fn foo() -> &int { let x = 5; &x }  // x dropped"),
            suggestion: Some("Ensure references outlive their usage."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4007_CannotReturnReference,
            title: "Cannot Return Reference",
            description: "Cannot return a reference to a local variable.",
            example: Some("fn foo() -> &int { let x = 5; &x }"),
            suggestion: Some("Return an owned value or use a different lifetime."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4008_DanglingReference,
            title: "Dangling Reference",
            description: "A reference points to memory that has been freed.",
            example: Some("let r = { let x = 5; &x };  // x dropped"),
            suggestion: Some("Ensure references do not outlive the values they point to."),
        });

        // Additional codegen errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA5001_LlvmError,
            title: "LLVM Error",
            description: "An error occurred in the LLVM backend.",
            example: None,
            suggestion: Some("This is likely a compiler bug. Please report it with your code."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA5002_LinkError,
            title: "Link Error",
            description: "The linker failed to link the program.",
            example: None,
            suggestion: Some("Check for missing libraries or linker flags."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA5003_InvalidTarget,
            title: "Invalid Target",
            description: "The specified compilation target is not valid.",
            example: Some("kraken build --target unknown-unknown-unknown"),
            suggestion: Some("Use a valid target triple like x86_64-unknown-linux-gnu."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA5004_OptimizationError,
            title: "Optimization Error",
            description: "An error occurred during optimization.",
            example: None,
            suggestion: Some("Try compiling without optimizations or report this bug."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA5005_AssemblyError,
            title: "Assembly Error",
            description: "Failed to generate assembly code.",
            example: None,
            suggestion: Some("This is likely a compiler bug. Please report it."),
        });

        // Module system errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA6000_ModuleNotFound,
            title: "Module Not Found",
            description: "The specified module could not be found.",
            example: Some("mod missing;"),
            suggestion: Some("Create the module file or check the module path."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA6001_CyclicModuleDependency,
            title: "Cyclic Module Dependency",
            description: "Modules have a circular dependency.",
            example: Some("mod a { use super::b; }\nmod b { use super::a; }"),
            suggestion: Some("Refactor to remove the circular dependency."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA6002_InvalidModulePath,
            title: "Invalid Module Path",
            description: "The module path is not valid.",
            example: Some("use ::invalid::path;"),
            suggestion: Some(
                "Use a valid module path starting from the crate root or a relative path.",
            ),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA6003_DuplicateModule,
            title: "Duplicate Module",
            description: "A module with this name is already defined.",
            example: Some("mod foo { }\nmod foo { }  // Duplicate"),
            suggestion: Some("Use a different name for the module."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA6004_InvalidModuleStructure,
            title: "Invalid Module Structure",
            description: "The module structure is not valid.",
            example: None,
            suggestion: Some("Ensure modules follow the correct file structure."),
        });

        // Macro errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA7000_MacroExpansionError,
            title: "Macro Expansion Error",
            description: "An error occurred while expanding a macro.",
            example: Some("my_macro!(invalid syntax)"),
            suggestion: Some("Check the macro invocation syntax."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA7001_InvalidMacroInvocation,
            title: "Invalid Macro Invocation",
            description: "The macro was invoked with invalid syntax.",
            example: Some("println!"),
            suggestion: Some("Provide the required arguments to the macro."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA7002_MacroNotFound,
            title: "Macro Not Found",
            description: "The specified macro has not been defined.",
            example: Some("unknown_macro!()"),
            suggestion: Some("Define the macro or import it from a module."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA7003_RecursiveMacroExpansion,
            title: "Recursive Macro Expansion",
            description: "A macro expands to itself, causing infinite recursion.",
            example: Some("macro_rules! foo { () => { foo!() } }"),
            suggestion: Some("Add a base case to prevent infinite recursion."),
        });

        // Attribute errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA8000_UnknownAttribute,
            title: "Unknown Attribute",
            description: "The attribute is not recognized.",
            example: Some("#[unknown] fn foo() { }"),
            suggestion: Some("Check the attribute name or remove it if not needed."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA8001_InvalidAttributeArgument,
            title: "Invalid Attribute Argument",
            description: "An attribute argument has invalid syntax or value.",
            example: Some("#[repr(invalid)] struct Foo { }"),
            suggestion: Some("Use valid attribute arguments."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA8002_MissingAttributeArgument,
            title: "Missing Attribute Argument",
            description: "An attribute is missing required arguments.",
            example: Some("#[repr] struct Foo { }  // Missing argument"),
            suggestion: Some("Provide the required arguments to the attribute."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA8003_DuplicateAttribute,
            title: "Duplicate Attribute",
            description: "The same attribute was applied multiple times.",
            example: Some("#[inline]\n#[inline]\nfn foo() { }"),
            suggestion: Some("Remove the duplicate attribute."),
        });

        // Additional I/O errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9002_IoError,
            title: "I/O Error",
            description: "An I/O operation failed.",
            example: None,
            suggestion: Some("Check file permissions and disk space."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9003_PermissionDenied,
            title: "Permission Denied",
            description: "Permission was denied for the requested operation.",
            example: None,
            suggestion: Some("Check file permissions or run with appropriate privileges."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9004_InvalidPath,
            title: "Invalid Path",
            description: "The specified path is not valid.",
            example: None,
            suggestion: Some("Use a valid file system path."),
        });

        // Internal errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9999_InternalCompilerError,
            title: "Internal Compiler Error",
            description: "An unexpected error occurred inside the compiler.",
            example: None,
            suggestion: Some(
                "This is a compiler bug. Please report it with a minimal reproduction.",
            ),
        });
    }
}

impl Default for DiagnosticRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_creation() {
        let registry = DiagnosticRegistry::new();
        assert!(!registry.entries.is_empty());
    }

    #[test]
    fn test_lookup_existing_code() {
        let registry = DiagnosticRegistry::new();
        let info = registry.lookup("KRA0001");
        assert!(info.is_some());
        assert_eq!(info.unwrap().title, "Unexpected Character");
    }

    #[test]
    fn test_lookup_nonexistent_code() {
        let registry = DiagnosticRegistry::new();
        let info = registry.lookup("KRA9998");
        assert!(info.is_none());
    }

    #[test]
    fn test_all_codes() {
        let registry = DiagnosticRegistry::new();
        let codes = registry.all_codes();
        assert!(!codes.is_empty());
    }

    #[test]
    fn test_diagnostic_info_fields() {
        let registry = DiagnosticRegistry::new();
        let info = registry.lookup("KRA2000").unwrap();
        assert_eq!(info.code, DiagnosticCode::KRA2000_TypeMismatch);
        assert!(!info.title.is_empty());
        assert!(!info.description.is_empty());
    }
}
