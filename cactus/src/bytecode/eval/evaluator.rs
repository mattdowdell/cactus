//!
//!
//!

use std::io;

use crate::bytecode::error::BytecodeError;
use crate::bytecode::parser::{Module, Instruction, Symbol};

use super::frame::{StackFrame, StackItem};

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Evaluator {
	frames: Vec<StackFrame>,
	frame_pointer: usize,

	module: Module,
	instruction_pointer: usize,

	stack: Vec<StackItem>,
	return_register: Option<StackItem>,
}

impl Evaluator {
	///
	///
	///
	pub fn new(module: Module) -> Evaluator {
		Evaluator {
			frames: Vec::new(),
			frame_pointer: 0,

			module: module,
			instruction_pointer: 0,

			stack: Vec::new(),
			return_register: None,
		}
	}

	///
	///
	///
	pub fn eval(&mut self) -> Result<(), BytecodeError> {
		let frame = StackFrame::new();
		self.frames.push(frame);

		self.frame_pointer = 0;
		self.eval_from_label("main")?;

		Ok(())
	}

	///
	///
	///
	fn eval_from_label(&mut self, label: &str) -> Result<(), BytecodeError> {
		self.instruction_pointer = self.module.lookup_label(label)?;

		loop {
			match self.module.get(self.instruction_pointer)? {
				Instruction::Add => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left + right;
					self.push(StackItem::Integer(result));
				},
				Instruction::Alloca => {
					unimplemented!()
				},
				Instruction::And => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left & right;
					self.push(StackItem::Integer(result));
				},
				Instruction::Compl => {
					let val = self.pop_integer()?;
					let result = !val;

					self.push(StackItem::Integer(result));
				},
				Instruction::Div => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left / right;
					self.push(StackItem::Integer(result));
				},
				Instruction::Dumpstack => {
					println!("{:?}", self.stack.clone());
				},
				Instruction::Dumpframe => {
					println!("{:?}", self.frames[self.frame_pointer].clone());
				},
				Instruction::Dup => {
					let item = self.pop()?;

					self.push(item.clone());
					self.push(item.clone());
				},
				Instruction::Eq => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					if left == right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}
				},
				Instruction::Geq => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					if left >= right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}
				},
				Instruction::Gt => {let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					if left > right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}},
				Instruction::Halt => {
					// TODO: return a code saying what this was
					return Ok(());
				},
				Instruction::In => {
					unimplemented!()
				},
				Instruction::Jmp => {
					let addr = self.pop_address()?;
					self.instruction_pointer = addr;

					continue;
				},
				Instruction::Jmpnz => {
					let addr = self.pop_address()?;
					let value = self.pop_integer()?;

					if value != 0 {
						self.instruction_pointer = addr;
						continue;
					}
				},
				Instruction::Labeldef(label) => {
					// nothing to do
				},
				Instruction::Leq => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					if left <= right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}
				},
				Instruction::Load => {},
				Instruction::Loadidx => {},
				Instruction::Lt => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					if left < right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}
				},
				Instruction::Minus => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left - right;
					self.push(StackItem::Integer(result));
				},
				Instruction::Movret => {
					let item = self.pop()?;
					self.return_register = Some(item);
				},
				Instruction::Mul => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left * right;
					self.push(StackItem::Integer(result));
				},
				Instruction::Neq => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					if left != right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}
				},
				Instruction::Nop => {
					// nothing to do
				},
				Instruction::Not => {},
				Instruction::Or => {},
				Instruction::Out => {
					let item = self.pop()?;
					print!("{}", item);

					// print! does not flush stdout after it's called
					// see <https://github.com/rust-lang/rust/issues/23818>
					io::stdout().flush().unwrap();
				},
				Instruction::Outln => {
					let item = self.pop()?;
					println!("{}", item);
				},
				Instruction::Pop => {
					self.pop()?;
				},
				Instruction::Push(literal) => {},
				Instruction::Pushaddr(label) => {
					let addr = self.module.lookup_label(&label)?;
					self.push(StackItem::Address(addr));
				},
				Instruction::Pusharg => {

				},
				Instruction::Pushret => {
					match &self.return_register {
						Some(item) => self.push(item.clone()),
						None => unimplemented!()
					}
				},
				Instruction::Rem => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left % right;
					self.push(StackItem::Integer(result));

				},
				Instruction::Return => {},
				Instruction::Store => {},
				Instruction::Storeidx => {},
				Instruction::Subcall => {},
				Instruction::Swap => {
					let left = self.pop()?;
					let right = self.pop()?;

					self.push(left);
					self.push(right);
				},
			}

			self.instruction_pointer += 1;
		}

		// TODO: this should be an error as it's not a HALT
		Ok(())
	}

	//
	//
	//
	fn push(&mut self, item: StackItem) {
		self.stack.push(item);
	}

	//
	//
	//
	fn pop(&mut self) -> Result<StackItem, BytecodeError> {
		match self.stack.pop() {
			Some(item) => Ok(item),
			None => {
				unimplemented!()
			}
		}
	}

	//
	//
	//
	fn pop_integer(&mut self) -> Result<i32, BytecodeError> {
		match self.pop()? {
			StackItem::Integer(val) => Ok(val),
			_ => {
				unimplemented!()
			}
		}
	}

	//
	//
	//
	fn pop_float(&mut self) -> Result<f32, BytecodeError> {
		match self.pop()? {
			StackItem::Float(val) => Ok(val),
			_ => {
				unimplemented!()
			}
		}
	}

	//
	//
	//
	fn pop_string(&mut self) -> Result<String, BytecodeError> {
		match self.pop()? {
			StackItem::String(val) => Ok(val),
			_ => {
				unimplemented!()
			}
		}
	}

	//
	//
	//
	fn pop_symbol(&mut self) -> Result<Symbol, BytecodeError> {
		match self.pop()? {
			StackItem::Symbol(val) => Ok(val),
			_ => {
				unimplemented!()
			}
		}
	}

	//
	//
	//
	fn pop_address(&mut self) -> Result<usize, BytecodeError> {
		match self.pop()? {
			StackItem::Address(val) => Ok(val),
			_ => {
				unimplemented!()
			}
		}
	}
}
