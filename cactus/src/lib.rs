//! An implementation of a Cactus compiler and a Maude interpreter.
//!
//! - The compiler takes the Cactus source code and produces bytecode. It can also optionally
//!   evaluate the bytecode.
//! - The interpreter evaluates the produced bytecode.

// public modules
pub mod bytecode;
pub mod compiler;

// internal modules
#[macro_use]
mod error;
mod location;

pub use compiler::Compiler;
