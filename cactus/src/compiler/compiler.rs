//!
//!
//!

use crate::error::CompilationError;
use crate::bytecode::Instruction;
use crate::compiler::parser::Parser;
use crate::compiler::analyser::Analyser;
use crate::compiler::bytecode_generator::BytecodeGenerator;


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
	pub fn compile(&self, input: &str) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut parser = Parser::new(input);
		let mut ast = parser.parse()?;

		let mut analyser = Analyser::new();
		analyser.analyse_ast(&mut ast)?;

		let bytecode_generator = BytecodeGenerator::new();
		let instructions = bytecode_generator.convert_ast(ast)?;

		Ok(instructions)
	}
}
