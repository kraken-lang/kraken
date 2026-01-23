pub mod closure_analysis;
pub mod desugar;
pub mod monomorphization;
pub mod type_checker;
pub mod types;

// Re-exported for future use when closure runtime is implemented
#[allow(unused_imports)]
pub use closure_analysis::{ClosureAnalyzer, ClosureEnvironment, CapturedVariable};
pub use monomorphization::monomorphize_program;
pub use type_checker::TypeChecker;
