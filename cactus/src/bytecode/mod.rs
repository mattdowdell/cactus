//!
//!
//!

pub mod eval;
pub mod instruction;

mod lexer;
mod parser;

pub use instruction::{Instruction, Literal, Symbol};
