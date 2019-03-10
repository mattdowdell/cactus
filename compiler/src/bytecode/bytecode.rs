//!
//!
//!

use crate::{
	error::{
		CompilationError,
		ErrorCode,
		ErrorType,
	},
	parser::ast::{
		Ast,
		Module,
	},
	bytecode::instruction::Instruction,
};

pub trait Bytecode {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, CompilationError>;
}


impl Bytecode for Ast {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, CompilationError> {
		let mut instructions: Vec<Instruction> = Vec::new();

		for module in self.modules.iter() {
			let module_instructions = module.to_bytecode()?;
			instructions.extend(module_instructions);
		}

		Ok(instructions)
	}
}


impl Bytecode for Module {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, CompilationError> {
		let mut instructions: Vec<Instruction> = Vec::new();

		for definition in self.definitions.iter() {
			let definition_instructions = definition.to_bytecode()?;
			instructions.extend(definition_instructions);
		}

		Ok(instructions)
	}
}


impl Bytecode for Definition {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, CompilationError> {
		let mut instructions: Vec<Instruction> = Vec::new();

		match self {
			Definition::Import => {},
			Definition::Enum => {},
			Definition::Struct => {},
			Definition::Function => {

			},
		}

		Ok(instructions)
	}
}
