//! The bytecode interpreter.

use super::error::BytecodeError;
use super::parser::Parser;
use super::eval::Evaluator;

/// A representation of the bytecode interpreter.
pub struct Interpreter {}

impl Interpreter {
	/// Create a new instance of `Interpreter`.
	pub fn new() -> Interpreter {
		Interpreter {}
	}

	/// Interpret the given input.
	pub fn interpret(&self, input: &str) -> Result<(), Vec<BytecodeError>> {
		let mut parser = Parser::new(input);
		let module = parser.parse()?;

		let mut eval = Evaluator::new(module);
		match eval.eval() {
			Ok(_) => Ok(()),
			Err(error) => {
				let errors = vec![error];
				Err(errors)
			}
		}

	}
}
