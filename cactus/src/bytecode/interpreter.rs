//!
//!
//!

use super::error::BytecodeError;
use super::parser::Parser;
use super::eval::Evaluator;

///
///
///
pub struct Interpreter {}

impl Interpreter {
	///
	///
	///
	pub fn new() -> Interpreter {
		Interpreter {}
	}

	///
	///
	///
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
