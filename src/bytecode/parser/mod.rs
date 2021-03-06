//! The bytecode parser and AST.

mod ast;
mod parser;

pub use parser::Parser;
pub use ast::{Module, Instruction, Literal, Symbol};
