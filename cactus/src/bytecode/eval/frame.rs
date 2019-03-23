//!
//!
//!

use std::fmt;

use crate::bytecode::Symbol;

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct StackFrame {
	args: Vec<StackItem>,
	locals: Vec<StackItem>,
}

impl StackFrame {
	///
	///
	///
	pub fn new() -> StackFrame {
		StackFrame {
			args: Vec::new(),
			locals: Vec::new(),
		}
	}

	///
	///
	///
	pub fn store_arg(&mut self, index: usize, item: StackItem) {
		unimplemented!()
	}

	///
	///
	///
	pub fn load_arg(&mut self, index: usize) -> StackItem {
		unimplemented!()
	}

	///
	///
	///
	pub fn store_local(&mut self, index: usize, item: StackItem) {
		unimplemented!()
	}

	///
	///
	///
	pub fn load_local(&mut self, index: usize) -> StackItem {
		unimplemented!()
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum StackItem {
	Integer(i32),
	Float(f32),
	String(String),
	Symbol(Symbol),
	Address(usize),
}

impl fmt::Display for StackItem {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			StackItem::Integer(val) => write!(f, "{}", val),
			StackItem::Float(val)   => write!(f, "{}", val),
			StackItem::String(val)  => write!(f, "{}", val),
			StackItem::Symbol(val)  => write!(f, "{}", val),
			StackItem::Address(val) => write!(f, "0x{:X}", val),
		}
	}
}
