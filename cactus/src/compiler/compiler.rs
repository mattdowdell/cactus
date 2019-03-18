//!
//!
//!

use crate::error::CompilationError;
use crate::compiler::parser::Parser;
use crate::compiler::analyser::Analyser;


pub struct Compiler {}

impl Compiler {
	///
	///
	///
	pub fn new() -> Compiler {
		Compiler {}
	}

	///
	///
	///
	pub fn compile(&self, input: &str) -> Result<(), Vec<CompilationError>> {
		let mut parser = Parser::new(input);
		let mut ast = parser.parse()?;

		let mut analyser = Analyser::new();
		analyser.analyse_ast(&mut ast)?;

		Ok(())
	}
}
