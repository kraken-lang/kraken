pub mod desugar;
pub mod monomorphization;
pub mod type_checker;
pub mod types;

pub use monomorphization::monomorphize_program;
pub use type_checker::TypeChecker;
