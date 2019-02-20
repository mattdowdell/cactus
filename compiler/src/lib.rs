//! A compiler for the high-level language.
//!
//! The compiler takes the high-level language and produces bytecode.
#![feature(trace_macros)]

extern crate itertools;

pub mod lexer;
pub mod parser;
pub mod analyser;
//pub mod code_gen;
