//!
//!
//!

use crate::error::CompilationError;
use crate::bytecode::Instruction;
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

