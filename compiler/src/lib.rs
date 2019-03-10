//! A compiler for Cactus.
//!
//! The compiler takes the Cactus source code and produces bytecode.

// top-level modules
#[macro_use]
pub mod error;
pub mod location;

// sub-modules
pub mod lexer;
pub mod parser;
pub mod analyser;
pub mod flowgraph;
