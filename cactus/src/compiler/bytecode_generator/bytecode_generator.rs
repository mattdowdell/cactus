//!
//!
//!

use crate::bytecode::Instruction;
use crate::compiler::error::CompilationError;
use crate::compiler::parser::Ast;

use super::ast::TToBytecode;

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct BytecodeGenerator {}

impl BytecodeGenerator {
	///
	///
	///
	pub fn new() -> BytecodeGenerator {
		BytecodeGenerator {}
	}

	///
	///
	///
	pub fn convert_ast(&self, ast: Ast) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		ast.to_bytecode()
	}
}

#[cfg(test)]
mod test {
	use crate::compiler::parser::Parser;
	use crate::compiler::analyser::Analyser;

	use super::*;

	fn generate(input: &str) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut parser = Parser::new(input);
		let mut analyser = Analyser::new();
		let generator = BytecodeGenerator::new();

		let mut ast = parser.parse()?;
		analyser.analyse_ast(&mut ast)?;

		let instructions = generator.convert_ast(ast)?;
		Ok(instructions)
	}

	#[test]
	#[ignore]
	fn test_let_statement() {
		let res = generate("fn x() { let a: i32 = 1; let b: i32 = 2; let c: i32 = 3; }");
		let expected = vec![
			Instruction::Labeldef("x".to_string()),
		];

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), expected);
	}
}

