//!
//!
//!

use crate::bytecode::Instruction;
use crate::compiler::parser::{
	Ast,
	Module,
	Definition,
	Function,
	Argument,
	Block,
	Statement,
	Let,
	If,
	Loop,
	LoopControl,
	Expression,
	Literal,
	Identifier,
	Prefix,
	Infix,
	Call,
	Operator,
};


///
///
///
pub trait TToBytecode {
	///
	///
	///
	fn to_bytecode(&self) -> Vec<Instruction>;
}
