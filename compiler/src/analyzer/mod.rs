/// Attribute processing: `#[repr(...)]`, `#[inline]`, `#[derive(...)]`, `#[test]`.
pub mod attributes;
/// Closure capture analysis: determines captured variables and environment layout.
pub mod closure_analysis;
/// Compile-time constant evaluation: arithmetic, comparisons, and const function calls.
pub mod const_eval;
/// Derive macro code generation: automatic trait implementations (Clone, Debug, PartialEq, etc.).
pub mod derive_generator;
/// AST desugaring: operator and syntax transformations before type checking.
pub mod desugar;
/// Drop trait checking: RAII cleanup ordering, scope tracking, and Drop impl validation.
pub mod drop_checker;
/// Future trait infrastructure: Waker, Context, Pin, and async task tracking.
pub mod future_trait;
/// Declarative macro expansion: pattern matching, argument binding, and hygienic names.
pub mod macro_expander;
/// Generic monomorphization: type specialization, inference, and where-clause validation.
pub mod monomorphization;
#[cfg(test)]
mod monomorphization_tests;
#[cfg(test)]
mod trait_tests;
#[cfg(test)]
mod type_checker_tests;
/// Type checker: validates types, resolves symbols, checks trait bounds, and enforces semantics.
pub mod type_checker;
/// Type environment: type definitions, function signatures, trait registry, and scope management.
pub mod types;

#[allow(unused_imports)]
pub use attributes::{AttributeProcessor, AttributeValue};
#[allow(unused_imports)]
pub use closure_analysis::{CapturedVariable, ClosureAnalyzer, ClosureEnvironment};
#[allow(unused_imports)]
pub use const_eval::{ConstEvaluator, ConstValue};
#[allow(unused_imports)]
pub use derive_generator::DeriveGenerator;
#[allow(unused_imports)]
pub use drop_checker::{DropChecker, DropImpl, VariableInfo};
#[allow(unused_imports)]
pub use future_trait::{Context, FutureTracker, Pin, Waker};
#[allow(unused_imports)]
pub use macro_expander::MacroExpander;
pub use monomorphization::monomorphize_program;
pub use type_checker::TypeChecker;
