//!
//!
//!

use std::io;
use std::io::Write;

use crate::location::Location;
use crate::bytecode::error::{BytecodeError, ErrorType, ErrorCode};
use crate::bytecode::parser::{Module, Instruction, Literal, Symbol};

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
			frame_pointer: 0,
			frames: vec![
				StackFrame::new(),
			],

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
		self.eval_from_label("main")?;
		Ok(())
	}

	//
	//
	//
	fn get_frame(&mut self) -> &mut StackFrame {
		&mut self.frames[self.frame_pointer]
	}

	//
	//
	//
	fn get_next_frame(&mut self) -> &mut StackFrame {
		if self.frames.len() <= (self.frame_pointer + 1) {
			self.frames.push(StackFrame::new());
		}

		&mut self.frames[self.frame_pointer + 1]
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
				Instruction::Gt => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					if left > right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}},
				Instruction::Halt => {
					// TODO: return a code saying what this was?
					return Ok(());
				},
				Instruction::In => {
					let mut input = String::new();

					match io::stdin().read_line(&mut input) {
						Ok(_) => {
							if !input.is_empty() {
								// trim trailing newline
								let len = input.len() - 1;
								input.truncate(len);

								match input.parse::<i32>() {
									Ok(value) => self.push(StackItem::Integer(value)),
									Err(_) => {
										return Err(BytecodeError::new(ErrorType::RuntimeError,
											ErrorCode::E0400,
											Location::end(),
											format!("Unable to convert value in integer: {}",
												input)));
									}
								}
							} else {
								return Err(BytecodeError::new(ErrorType::RuntimeError,
									ErrorCode::E0401,
									Location::end(),
									"No input given".to_string()));
							}
						}
						Err(error) => {
							// read failed
							return Err(BytecodeError::new(ErrorType::RuntimeError,
								ErrorCode::E0402,
								Location::end(),
								format!("{}", error)));
						}
					}
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
				Instruction::Labeldef(_) => {
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
				Instruction::Load => {
					let symbol = self.pop_symbol()?;

					match symbol {
						Symbol::Args => {
							let item = self.get_frame().load_arg(0)?;
							self.push(item.clone());
						},
						Symbol::Locals => {
							let item = self.get_frame().load_local(0)?;
							self.push(item.clone());
						},
					}
				},
				Instruction::Loadidx => {
					let index = self.pop_integer()? as usize;
					let symbol = self.pop_symbol()?;

					match symbol {
						Symbol::Args => {
							let item = self.get_frame().load_arg(index)?;
							self.push(item.clone());
						},
						Symbol::Locals => {
							let item = self.get_frame().load_local(index)?;
							self.push(item.clone());
						},
					}
				},
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
				Instruction::Not => {
					let value = self.pop_integer()?;

					if value == 0 {
						self.push(StackItem::Integer(1));
					} else {
						self.push(StackItem::Integer(0));
					}
				},
				Instruction::Or => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left | right;
					self.push(StackItem::Integer(result));
				},
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
				Instruction::Push(literal) => {
					match literal {
						Literal::Integer(val) => {
							let integer = val.parse::<i32>().unwrap();
							self.push(StackItem::Integer(integer));
						},
						Literal::Float(val) => {
							let float = val.parse::<f32>().unwrap();
							self.push(StackItem::Float(float));
						},
						Literal::String(val) => {
							self.push(StackItem::String(val.clone()));
						},
						Literal::Symbol(val) => {
							self.push(StackItem::Symbol(val));
						},
					}
				},
				Instruction::Pushaddr(label) => {
					let addr = self.module.lookup_label(&label)?;
					self.push(StackItem::Address(addr));
				},
				Instruction::Pusharg => {
					let item = self.pop()?;
					self.get_next_frame().push_arg(item);
				},
				Instruction::Pushret => {
					match &self.return_register {
						Some(item) => self.push(item.clone()),
						None => {
							return Err(BytecodeError::new(ErrorType::LookupError,
								ErrorCode::E0206,
								Location::end(),
								"PUSHRET cannot be used with an empty return register".to_string()));
						}
					}
				},
				Instruction::Rem => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left % right;
					self.push(StackItem::Integer(result));

				},
				Instruction::Return => {
					self.frame_pointer -= 1;
					self.frames.pop();

					let ip = self.get_frame().get_instruction_pointer();
					self.instruction_pointer = ip;

					continue;
				},
				Instruction::Store => {
					let item = self.pop()?;
					let symbol = self.pop_symbol()?;

					match symbol {
						Symbol::Args => {
							return Err(BytecodeError::new(ErrorType::LookupError,
								ErrorCode::E0205,
								Location::end(),
								"STORE cannot be used to manipulate the ARGS array".to_string()));
						},
						Symbol::Locals => {
							self.get_frame().store_local(0, item)?;
						},
					}
				},
				Instruction::Storeidx => {
					let item = self.pop()?;
					let index = self.pop_integer()? as usize;
					let symbol = self.pop_symbol()?;

					match symbol {
						Symbol::Args => {
							return Err(BytecodeError::new(ErrorType::LookupError,
								ErrorCode::E0205,
								Location::end(),
								"STOREIDX cannot be used to manipulate the ARGS array".to_string()));
						},
						Symbol::Locals => {
							self.get_frame().store_local(index, item)?;
						},
					}
				},
				Instruction::Subcall => {
					// save the current instruction pointer
					let ip = self.instruction_pointer + 1;
					self.get_frame().set_instruction_pointer(ip);

					// creates the next frame if it doesn't already exist
					self.get_next_frame();

					// move to the next frame
					self.frame_pointer += 1;

					let addr = self.pop_address()?;
					self.instruction_pointer = addr;

					continue;
				},
				Instruction::Swap => {
					let left = self.pop()?;
					let right = self.pop()?;

					self.push(left);
					self.push(right);
				},
			}

			self.instruction_pointer += 1;
		}
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
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0207,
					Location::end(),
					"Unable to pop from the stack: stack is empty".to_string()))
			}
		}
	}

	//
	//
	//
	fn pop_integer(&mut self) -> Result<i32, BytecodeError> {
		let item = self.pop()?;

		match item {
			StackItem::Integer(val) => Ok(val),
			_ => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0208,
					Location::end(),
					format!("Expected Integer on the stack, found: {:?}",
						item)))
			}
		}
	}

	//
	//
	//
	fn pop_symbol(&mut self) -> Result<Symbol, BytecodeError> {
		let item = self.pop()?;

		match item {
			StackItem::Symbol(val) => Ok(val),
			_ => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0209,
					Location::end(),
					format!("Expected Symbol on the stack, found: {:?}",
						item)))
			}
		}
	}

	//
	//
	//
	fn pop_address(&mut self) -> Result<usize, BytecodeError> {
		let item = self.pop()?;

		match item {
			StackItem::Address(val) => Ok(val),
			_ => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0210,
					Location::end(),
					format!("Expected Address on the stack, found: {:?}",
						item)))
			}
		}
	}
}
