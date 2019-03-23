//!
//!
//!

use std::collections::HashMap;

use crate::bytecode::instruction::Instruction;

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
	label_map: HashMap<String, usize>,
	instructions: Vec<Instruction>,
}

impl Module {
	///
	///
	///
	pub fn new() -> Module {
		Module {
			label_map: HashMap::new(),
			instructions: Vec::new(),
		}
	}

	///
	///
	///
	pub fn push_instruction(&mut self, instruction: Instruction) {
		self.instructions.push(instruction);
	}
}
