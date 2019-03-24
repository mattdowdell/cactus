//!
//!
//!

use std::fmt;

use crate::location::Location;
use crate::bytecode::error::{BytecodeError, ErrorType, ErrorCode};
use crate::bytecode::Symbol;

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct StackFrame {
	args: Vec<StackItem>,
	locals: Vec<StackItem>,
	instruction_pointer: usize,
}

impl StackFrame {
	///
	///
	///
	pub fn new() -> StackFrame {
		StackFrame {
			args: Vec::new(),
			locals: Vec::new(),
			instruction_pointer: 0,
		}
	}

	///
	///
	///
	pub fn set_instruction_pointer(&mut self, pointer: usize) {
		self.instruction_pointer = pointer;
	}

	///
	///
	///
	pub fn get_instruction_pointer(&mut self) -> usize {
		self.instruction_pointer
	}

	///
	///
	///
	pub fn push_arg(&mut self, item: StackItem) {
		self.args.push(item);
	}

	///
	///
	///
	pub fn load_arg(&mut self, index: usize) -> Result<StackItem, BytecodeError> {
		match self.args.get(index) {
			Some(item) => Ok(item.clone()),
			None => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0202,
					Location::end(),
					format!("Unable to load argument from index: {}",
						index)))
			}
		}
	}

	///
	///
	///
	pub fn store_local(&mut self, index: usize, item: StackItem) -> Result<(), BytecodeError> {
		if index < self.locals.len() {
			self.locals[index] = item;
			Ok(())

		} else if index == self.locals.len() {
			self.locals.push(item);
			Ok(())

		} else {
			Err(BytecodeError::new(ErrorType::LookupError,
				ErrorCode::E0203,
				Location::end(),
				format!("Unable to store local at index: {}, as it would introduce gaps",
					index)))
		}
	}

	///
	///
	///
	pub fn load_local(&mut self, index: usize) -> Result<StackItem, BytecodeError> {
		match self.locals.get(index) {
			Some(item) => Ok(item.clone()),
			None => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0204,
					Location::end(),
					format!("Unable to load local from index: {}",
						index)))
			}
		}
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
