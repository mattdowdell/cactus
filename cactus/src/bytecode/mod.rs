//! The public interface of the bytecode interpreter.

mod error;
mod lexer;
mod parser;
mod eval;
mod interpreter;

pub use parser::{Instruction, Literal, Symbol};
pub use interpreter::Interpreter;
