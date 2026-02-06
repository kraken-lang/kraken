//! DocGraph metadata generator — produces JSON conforming to `docgraph-v1.schema.json`.

mod builder;
/// LSIF (Language Server Index Format) dump generator.
pub mod lsif;
mod types;

pub use builder::generate;
pub use types::*;
