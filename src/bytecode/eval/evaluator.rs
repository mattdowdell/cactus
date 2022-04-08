//! The bytecode evaluator.

use std::io;
use std::io::Write;

use crate::location::Location;
use crate::bytecode::error::{BytecodeError, ErrorType, ErrorCode};
use crate::bytecode::parser::{Module, Instruction, Literal, Symbol};

use super::frame::{StackFrame, StackItem};

/// A representation of the bytecode evaluator.
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
	/// Create a new instance of `Evaluator`.
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

	/// Evaluate the given module.
	pub fn eval(&mut self) -> Result<(), BytecodeError> {
		self.eval_from_label("main")?;
		Ok(())
	}

	// Get the current stack frame.
	fn get_frame(&mut self) -> &mut StackFrame {
		&mut self.frames[self.frame_pointer]
	}

	// Get the next stack frame.
	//
	// If one does not yet exist, a new one will be created first.
	fn get_next_frame(&mut self) -> &mut StackFrame {
		if self.frames.len() <= (self.frame_pointer + 1) {
			self.frames.push(StackFrame::new());
		}

		&mut self.frames[self.frame_pointer + 1]
	}

	/// Evaluate the module from the given label.
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
				Instruction::Addf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

					let result = left + right;
					self.push(StackItem::Float(result));
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
				Instruction::Divf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

					let result = left / right;
					self.push(StackItem::Float(result));
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
				Instruction::Geqf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

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
					}
				},
				Instruction::Gtf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

					if left > right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}
				},
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
				Instruction::Leqf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

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
				Instruction::Ltf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

					if left < right {
						self.push(StackItem::Integer(0));
					} else {
						self.push(StackItem::Integer(1));
					}
				},
				Instruction::Lshift => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left << right;
					self.push(StackItem::Integer(result));
				},
				Instruction::Minus => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left - right;
					self.push(StackItem::Integer(result));
				},
				Instruction::Minusf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

					let result = left - right;
					self.push(StackItem::Float(result));
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
				Instruction::Mulf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

					let result = left * right;
					self.push(StackItem::Float(result));
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
						Some(item) => {
							self.push(item.clone());
							self.return_register = None;
						},
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
				Instruction::Remf => {
					let left = self.pop_float()?;
					let right = self.pop_float()?;

					let result = left % right;
					self.push(StackItem::Float(result));

				},
				Instruction::Return => {
					self.frame_pointer -= 1;
					self.frames.pop();

					let ip = self.get_frame().get_instruction_pointer();
					self.instruction_pointer = ip;

					continue;
				},
				Instruction::Rshift => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left >> right;
					self.push(StackItem::Integer(result));
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
				Instruction::Xor => {
					let left = self.pop_integer()?;
					let right = self.pop_integer()?;

					let result = left ^ right;
					self.push(StackItem::Integer(result));
				},
			}

			self.instruction_pointer += 1;
		}
	}

	// Push a value onto the top of the stack.
	fn push(&mut self, item: StackItem) {
		self.stack.push(item);
	}

	// Remove a value from the top of the stack.
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

	// Remove an integer from the top of the stack.
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

	// Remove a float from the top of the stack.
	fn pop_float(&mut self) -> Result<f32, BytecodeError> {
		let item = self.pop()?;

		match item {
			StackItem::Float(val) => Ok(val),
			_ => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0211,
					Location::end(),
					format!("Expected Float on the stack, found: {:?}",
						item)))
			}
		}
	}

	// Remove a symbol from the top of the stack.
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

	// Remove an address from the top of the stack.
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


#[cfg(test)]
mod test {
	use std::{env, fs::File, io::Read};

	use crate::bytecode::parser::Parser;
	use super::*;

	fn read_file(filename: &str) -> String {
		let mut path = env::current_dir().unwrap();

		path.push("tests/unit_files");
		path.push(filename);

		match File::open(path.to_str().unwrap()) {
			Ok(mut file) => {
				let mut contents = String::new();
				file.read_to_string(&mut contents).expect("Error while reading from file");

				contents
			}
			Err(msg) => {
				println!("Error while opening file: {}: {}", filename, msg);
				panic!("ERROR");
			}
		}
	}

	macro_rules! evaluate_file {
		($filename:tt) => (
			match Parser::new(&read_file($filename)).parse() {
				Ok(module) => {
					let mut eval = Evaluator::new(module);

					match eval.eval() {
						Ok(_) => {
							eval.clone()
						},
						Err(error) => {
							println!("{}", error);
							panic!("Unexpected error during evaluation");
						}
					}
				},
				Err(errors) => {
					for error in errors.iter() {
						println!("{}", error);
					}

					panic!("Unexpected errors during parsing");
				}
			}
		)
	}

	macro_rules! evaluate_file_fail {
		($exc:ident, $filename:tt) => {
			let input = read_file($filename);
			let mut eval = Evaluator::new(&input);

			let $exc = match eval.eval() {
				Ok(_) => panic!("Unexpected success during evaluation"),
				Err(exc) => exc,
			};
		}
	}

	#[test]
	fn test_halt() {
		evaluate_file!("halt.smac");
	}

	#[test]
	fn test_push() {
		let eval = evaluate_file!("push.smac");
		let expected = vec![
			StackItem::Integer(1),
			StackItem::Float(1.0),
			StackItem::String("foo".to_string()),
		];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_push_args() {
		let eval = evaluate_file!("push_args.smac");
		let expected = vec![StackItem::Symbol(Symbol::Args)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_push_locals() {
		let eval = evaluate_file!("push_locals.smac");
		let expected = vec![StackItem::Symbol(Symbol::Locals)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_pop() {
		let eval = evaluate_file!("pop.smac");
		let expected = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_dup() {
		let eval = evaluate_file!("dup.smac");
		let expected = vec![StackItem::Integer(1), StackItem::Integer(1)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_swap() {
		let eval = evaluate_file!("swap.smac");
		let expected = vec![StackItem::Integer(5), StackItem::Integer(1)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_movret() {
		let eval = evaluate_file!("movret.smac");
		let expected = Some(StackItem::Integer(1));

		assert_eq!(eval.return_register, expected);
	}

	#[test]
	fn test_pushret() {
		let eval = evaluate_file!("pushret.smac");
		let expected = vec![StackItem::Integer(1)];

		assert!(eval.return_register.is_none());
		assert_eq!(eval.stack, expected);
	}

/*
	#[test]
	fn test_pushret_fail() {
		evaluate_file_fail!(exc, "pushret_fail.smac");

		assert_eq!(exc.header(), "ERROR (E2005) found on line: 2, column: 2");
		assert_eq!(exc.footer(), "Return register is not defined");
	}
*/


	#[test]
	fn test_pushaddr() {
		let eval = evaluate_file!("pushaddr.smac");
		let expected = vec![StackItem::Address(0)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_store_locals() {
		let mut eval = evaluate_file!("store_locals.smac");
		let expected = vec![StackItem::Integer(0)];

		assert!(eval.stack.is_empty());
		assert_eq!(eval.get_frame().locals, expected);
	}

	#[test]
	fn test_store_locals_twice() {
		let mut eval = evaluate_file!("store_locals_twice.smac");
		let expected = vec![StackItem::Integer(0)];

		assert!(eval.stack.is_empty());
		assert_eq!(eval.get_frame().locals, expected);
	}
/*
	// the below are tests copied from the original interpreter
	// however, restricting where data can be loaded and stored to locals and args
	// means that many of these no longer work


	#[test]
	fn test_load_locals() {
		evaluate_file!(eval, "load_locals.smac");
		let expected_stack = vec![StackItem::Integer(0)];
		let expected_locals = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
		assert_eq!(eval.cur_frame.locals, expected_locals);
	}

	#[test]
	fn test_load_locals_fail() {
		evaluate_file_fail!(exc, "load_locals_fail.smac");

		assert_eq!(exc.header(), "ERROR (E2013) found on line: 3, column: 2");
		assert_eq!(exc.footer(), "LOCALS array does not have enough elements to perform this operation - Requested element: 1, array has 0 elements");
	}


	#[test]
	fn test_loadidx() {
		evaluate_file!(eval, "loadidx.smac");
		let expected_stack = vec![
			StackItem::Integer(1),
			StackItem::Integer(2),
		];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_loadidx_non_symbol() {
		evaluate_file_fail!(exc, "error_e2015_2.smac");

		assert_eq!(exc.header(), "ERROR (E2015) found on line: 4, column: 2");
		assert_eq!(exc.footer(), "A symbol was requested from the stack but found: 0x4");
	}

	#[test]
	fn test_eq_true() {
		evaluate_file!(eval, "eq_true.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_eq_false() {
		evaluate_file!(eval, "eq_false.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_neq_true() {
		evaluate_file!(eval, "neq_true.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_neq_false() {
		evaluate_file!(eval, "neq_false.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_leq_true_1() {
		evaluate_file!(eval, "leq_true_1.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_leq_true_2() {
		evaluate_file!(eval, "leq_true_2.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_leq_false() {
		evaluate_file!(eval, "leq_false.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_geq_true_1() {
		evaluate_file!(eval, "geq_true_1.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_geq_true_2() {
		evaluate_file!(eval, "geq_true_2.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_geq_false() {
		evaluate_file!(eval, "geq_false.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_lt_true() {
		evaluate_file!(eval, "lt_true.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_lt_false_1() {
		evaluate_file!(eval, "lt_false_1.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_lt_false_2() {
		evaluate_file!(eval, "lt_false_2.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_gt_true() {
		evaluate_file!(eval, "gt_true.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_gt_false_1() {
		evaluate_file!(eval, "gt_false_1.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_gt_false_2() {
		evaluate_file!(eval, "gt_false_2.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_not_1() {
		evaluate_file!(eval, "not_1.smac");
		let expected = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_not_2() {
		evaluate_file!(eval, "not_2.smac");
		let expected = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_minus() {
		evaluate_file!(eval, "minus.smac");
		let expected_stack = vec![StackItem::Integer(4)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_add() {
		evaluate_file!(eval, "add.smac");
		let expected_stack = vec![StackItem::Integer(6)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_div() {
		evaluate_file!(eval, "div.smac");
		let expected_stack = vec![StackItem::Integer(3)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_rem() {
		evaluate_file!(eval, "rem.smac");
		let expected_stack = vec![StackItem::Integer(1)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_mul() {
		evaluate_file!(eval, "mul.smac");
		let expected_stack = vec![StackItem::Integer(30)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_and_1() {
		evaluate_file!(eval, "and_1.smac");
		let expected_stack = vec![StackItem::Integer(0)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_and_2() {
		evaluate_file!(eval, "and_2.smac");
		let expected = vec![StackItem::Integer(10)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_or_1() {
		evaluate_file!(eval, "or_1.smac");
		let expected_stack = vec![StackItem::Integer(3)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_or_2() {
		evaluate_file!(eval, "or_2.smac");
		let expected_stack = vec![StackItem::Integer(15)];

		assert_eq!(eval.stack, expected_stack);
	}

	#[test]
	fn test_compl() {
		evaluate_file!(eval, "compl.smac");
		let expected = vec![StackItem::Integer(-8)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_jmp() {
		evaluate_file!(eval, "jmp.smac");
		let expected = vec![StackItem::Integer(5)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_jmpnz_1() {
		evaluate_file!(eval, "jmpnz_1.smac");

		assert!(eval.stack.is_empty());
	}

	#[test]
	fn test_jmpnz_2() {
		evaluate_file!(eval, "jmpnz_2.smac");
		let expected = vec![StackItem::Integer(5)];

		assert_eq!(eval.stack, expected);
	}

	#[test]
	fn test_unreachable_instruction() {
		evaluate_file_fail!(exc, "error_e2000.smac");

		assert_eq!(exc.header(), "ERROR (E2000) found on line: 1, column: 1");
		assert_eq!(exc.footer(), "Unreachable instruction found: NOP");
	}

	#[test]
	fn test_missing_halt() {
		evaluate_file_fail!(exc, "error_e2001.smac");

		assert_eq!(exc.header(), "ERROR (E2001)");
		assert_eq!(exc.footer(), "Did not find HALT before the end of \"main\"");
	}

	#[test]
	fn test_missing_main() {
		evaluate_file_fail!(exc, "error_e2002_1.smac");

		assert_eq!(exc.header(), "ERROR (E2002)");
		assert_eq!(exc.footer(), "Unable to find label \"main\" in module");
	}

	#[test]
	fn test_missing_label() {
		evaluate_file_fail!(exc, "error_e2002_2.smac");

		assert_eq!(exc.header(), "ERROR (E2002)");
		assert_eq!(
			exc.footer(),
			"Unable to find label \"does_not_exist\" in module"
		);
	}

	#[test]
	fn test_cannot_load_value_from_address() {
		evaluate_file_fail!(exc, "error_e2007.smac");

		assert_eq!(exc.header(), "ERROR (E2007) found on line: 6, column: 2");
		assert_eq!(exc.footer(), "Unable to load from address: 0x0");
	}

	#[test]
	fn test_cannot_pop_from_empty_stack() {
		evaluate_file_fail!(exc, "error_e2009.smac");

		assert_eq!(exc.header(), "ERROR (E2009) found on line: 2, column: 2");
		assert_eq!(exc.footer(), "Unable to pop from empty stack");
	}

	#[test]
	fn test_non_integer_on_stack() {
		evaluate_file_fail!(exc, "error_e2010.smac");

		assert_eq!(exc.header(), "ERROR (E2010) found on line: 4, column: 2");
		assert_eq!(
			exc.footer(),
			"An integer was requested from the stack but found: 1.0"
		);
	}

	#[test]
	fn test_non_address_on_stack() {
		evaluate_file_fail!(exc, "error_e2011.smac");

		assert_eq!(exc.header(), "ERROR (E2011) found on line: 3, column: 2");
		assert_eq!(
			exc.footer(),
			"An address was requested from the stack but found: 1.0"
		);
	}

	#[test]
	fn test_non_address_and_non_symbol_on_stack() {
		evaluate_file_fail!(exc, "error_e2012.smac");

		assert_eq!(exc.header(), "ERROR (E2012) found on line: 3, column: 2");
		assert_eq!(
			exc.footer(),
			"An address or symbol was requested from the stack but found: 2"
		);
	}

	#[test]
	fn test_storeidx_with_gaps() {
		evaluate_file_fail!(exc, "error_e2014.smac");

		assert_eq!(exc.header(), "ERROR (E2014) found on line: 5, column: 2");
		assert_eq!(
			exc.footer(),
			"Inserting into ARGS array at index: 6 would introduce gaps in the array - Current length: 0"
		);
	}

	#[test]
	fn test_find_label() {
		evaluate_file!(eval, "halt.smac");
		let res = eval.find_label("main");

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), 0)
	}

	#[test]
	fn test_find_label_error() {
		evaluate_file!(eval, "halt.smac");
		let res = eval.find_label("does_not_exist");

		assert!(res.is_err());

		let exc = res.err().unwrap();

		assert_eq!(exc.header(), "ERROR (E2002)");
		assert_eq!(
			exc.footer(),
			"Unable to find label \"does_not_exist\" in module"
		);
	}

	#[test]
	fn test_push_stack() {
		let mut eval = Evaluator::new("main:\nNOP;");

		eval.push_stack(StackItem::Integer(5));

		assert_eq!(eval.stack.len(), 1);
	}

	#[test]
	fn test_pop_stack() {
		let mut eval = Evaluator::new("main:\nNOP;");
		let instr = InstructionData::new(Instruction::Nop, 2, 1);

		eval.push_stack(StackItem::Integer(5));

		let res = eval.pop_stack(&instr);

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), StackItem::Integer(5));
	}

	#[test]
	fn test_pop_stack_empty() {
		let mut eval = Evaluator::new("main:\nNOP;");
		let instr = InstructionData::new(Instruction::Nop, 2, 1);
		let res = eval.pop_stack(&instr);

		assert!(res.is_err());

		let exc = res.err().unwrap();
		assert_eq!(exc.header(), "ERROR (E2009) found on line: 2, column: 1");
		assert_eq!(exc.footer(), "Unable to pop from empty stack");
	}

	#[test]
	fn test_pop_stack_int() {
		let mut eval = Evaluator::new("main:\nNOP;");
		let instr = InstructionData::new(Instruction::Nop, 2, 1);

		eval.push_stack(StackItem::Integer(5));

		let res = eval.pop_stack_int(&instr);

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), 5);
	}

	#[test]
	fn test_pop_stack_int_not_found() {
		let mut eval = Evaluator::new("main:\nNOP;");
		let instr = InstructionData::new(Instruction::Nop, 2, 1);

		eval.push_stack(StackItem::Double(1.0));

		let res = eval.pop_stack_int(&instr);

		assert!(res.is_err());

		let exc = res.err().unwrap();
		assert_eq!(exc.header(), "ERROR (E2010) found on line: 2, column: 1");
		assert_eq!(
			exc.footer(),
			"An integer was requested from the stack but found: 1.0"
		);
	}

	#[test]
	fn test_pop_stack_addr() {
		let mut eval = Evaluator::new("main:\nNOP;");
		let instr = InstructionData::new(Instruction::Nop, 2, 1);

		eval.push_stack(StackItem::Address(0));

		let res = eval.pop_stack_addr(&instr);

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), 0);
	}

	#[test]
	fn test_pop_stack_addr_not_found() {
		let mut eval = Evaluator::new("main:\nNOP;");
		let instr = InstructionData::new(Instruction::Nop, 2, 1);

		eval.push_stack(StackItem::Double(1.0));

		let res = eval.pop_stack_addr(&instr);

		assert!(res.is_err());

		let exc = res.err().unwrap();
		assert_eq!(exc.header(), "ERROR (E2011) found on line: 2, column: 1");
		assert_eq!(
			exc.footer(),
			"An address was requested from the stack but found: 1.0"
		);
	}

	#[test]
	fn test_frame_pusharg() {
		evaluate_file!(eval, "frame_pusharg.smac");
		let expected = vec![
			StackItem::Integer(5),
		];

		assert_eq!(eval.cur_frame.args, expected);
		assert!(eval.cur_frame.callee_args.is_empty());
	}

	#[test]
	fn test_frame_clear_callee_args() {
		evaluate_file!(eval, "frame_clear_callee_args.smac");

		assert!(eval.cur_frame.callee_args.is_empty());
	}
*/
}
