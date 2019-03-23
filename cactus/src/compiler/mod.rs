//!
//!
//!

// public modules
pub mod compiler;
pub mod error;

// internal modules
mod analyser;
mod bytecode_generator;
mod lexer;
mod parser;

pub use compiler::Compiler;
pub use error::CompilationError;
