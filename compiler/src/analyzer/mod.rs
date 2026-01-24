pub mod attributes;
pub mod closure_analysis;
pub mod const_eval;
pub mod derive_generator;
pub mod desugar;
pub mod macro_expander;
pub mod monomorphization;
pub mod type_checker;
pub mod types;

// Re-exported for future use when closure runtime is implemented
#[allow(unused_imports)]
pub use attributes::{AttributeProcessor, AttributeValue};
#[allow(unused_imports)]
pub use closure_analysis::{CapturedVariable, ClosureAnalyzer, ClosureEnvironment};
#[allow(unused_imports)]
pub use const_eval::{ConstEvaluator, ConstValue};
#[allow(unused_imports)]
pub use derive_generator::DeriveGenerator;
#[allow(unused_imports)]
pub use macro_expander::MacroExpander;
pub use monomorphization::monomorphize_program;
pub use type_checker::TypeChecker;
