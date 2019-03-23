//!
//!
//!

//pub mod eval;
pub mod instruction;

mod error;
mod lexer;
//mod parser;

pub use instruction::{Instruction, Literal, Symbol};
