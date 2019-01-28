//! A compiler for the high-level language.
//!
//! The compiler takes the high-level language and produces bytecode.

extern crate itertools;

pub mod lexer;
pub mod parser;
pub mod code_gen;
