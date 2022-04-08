//! Top-level interface for the Cactus compiler.

use crate::compiler::error::CompilationError;
use crate::compiler::parser::Parser;
use crate::compiler::analyser::Analyser;
use crate::compiler::bytecode_generator::BytecodeGenerator;

use crate::bytecode::Instruction;

/// A representation of the Cactus compiler.
pub struct Compiler {}

impl Compiler {
	/// Create a new instance of `Compiler`.
	pub fn new() -> Compiler {
		Compiler {}
	}

	/// Compile the given input to bytecode.
	///
	/// If any errors occur, they will be returned, otherwise bytecode will be returned.
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
