//! Data structures used for the bytecode AST.

use std::collections::HashMap;
use std::fmt;

use crate::location::Location;
use crate::bytecode::error::{BytecodeError, ErrorType, ErrorCode};

/// A representation of a bytecode module.
///
/// The module is the root AST node in bytecode.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
	label_map: HashMap<String, usize>,
	instructions: Vec<Instruction>,
}

impl Module {
	/// Create a new instance of `Module`.
	pub fn new() -> Module {
		Module {
			label_map: HashMap::new(),
			instructions: Vec::new(),
		}
	}

	/// Add an instruction to the module.
	pub fn push_instruction(&mut self, instruction: Instruction) {
		match instruction.clone() {
			Instruction::Labeldef(label) => {
				let index = self.instructions.len();
				self.label_map.insert(label.clone(), index);


			},
			_ => {},
		}

		self.instructions.push(instruction);
	}

	/// Find a label in the module and return its index if found.
	pub fn lookup_label(&self, label: &str) -> Result<usize, BytecodeError> {
		match self.label_map.get(label) {
			Some(index) => Ok(*index),
			None => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0200,
					Location::end(),
					format!("Label does not exist: {}",
						label)))
			}
		}
	}

	/// Get an instruction at the given index.
	pub fn get(&self, index: usize) -> Result<Instruction, BytecodeError> {
		match self.instructions.get(index) {
			Some(instr) => Ok(instr.clone()),
			None => {
				Err(BytecodeError::new(ErrorType::LookupError,
					ErrorCode::E0201,
					Location::end(),
					format!("Instruction not found at index: {}",
						index)))
			}
		}
	}
}

/// The possible bytecode instructions.
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
	Add,
	Addf,
	And,
	Compl,
	Div,
	Divf,
	Dumpstack,
	Dumpframe,
	Dup,
	Eq,
	Geq,
	Geqf,
	Gt,
	Gtf,
	Halt,
	In,
	Jmp,
	Jmpnz,
	Labeldef(String),
	Leq,
	Leqf,
	Load,
	Loadidx,
	Lshift,
	Lt,
	Ltf,
	Minus,
	Minusf,
	Movret,
	Mul,
	Mulf,
	Neq,
	Nop,
	Not,
	Or,
	Out,
	Outln,
	Pop,
	Push(Literal),
	Pushaddr(String),
	Pusharg,
	Pushret,
	Rem,
	Remf,
	Return,
	Rshift,
	Store,
	Storeidx,
	Subcall,
	Swap,
	Xor,
}

impl Instruction {
	/// Create a new push instruction with a symbol argument.
	pub fn push_symbol(symbol: Symbol) -> Instruction {
		Instruction::Push(Literal::Symbol(symbol))
	}

	/// Create a new push instruction with an integer argument.
	pub fn push_offset(offset: usize) -> Instruction {
		Instruction::Push(Literal::Integer(offset.to_string()))
	}

	/// Create a new push instruction with an integer argument (stored as a string).
	pub fn push_integer(value: String) -> Instruction {
		Instruction::Push(Literal::Integer(value))
	}

	/// Create a new push instruction with a float argument (stored as a string).
	pub fn push_float(value: String) -> Instruction {
		Instruction::Push(Literal::Float(value))
	}
}

impl fmt::Display for Instruction {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Instruction::Add             => write!(f, "\tADD;"),
			Instruction::Addf            => write!(f, "\tADDF;"),
			Instruction::And             => write!(f, "\tAND;"),
			Instruction::Compl           => write!(f, "\tCOMPL;"),
			Instruction::Div             => write!(f, "\tDIV;"),
			Instruction::Divf            => write!(f, "\tDIVF;"),
			Instruction::Dumpstack       => write!(f, "\tDUMPSTACK;"),
			Instruction::Dumpframe       => write!(f, "\tDUMPFRAME;"),
			Instruction::Dup             => write!(f, "\tDUP;"),
			Instruction::Eq              => write!(f, "\tEQ;"),
			Instruction::Geq             => write!(f, "\tGEQ;"),
			Instruction::Geqf            => write!(f, "\tGEQF;"),
			Instruction::Gt              => write!(f, "\tGT;"),
			Instruction::Gtf             => write!(f, "\tGTF;"),
			Instruction::Halt            => write!(f, "\tHALT;"),
			Instruction::In              => write!(f, "\tIN;"),
			Instruction::Jmp             => write!(f, "\tJMP;"),
			Instruction::Jmpnz           => write!(f, "\tJMPNZ;"),
			Instruction::Labeldef(label) => write!(f, "{}:", label),
			Instruction::Leq             => write!(f, "\tLEQ;"),
			Instruction::Leqf            => write!(f, "\tLEQF;"),
			Instruction::Load            => write!(f, "\tLOAD;"),
			Instruction::Loadidx         => write!(f, "\tLOADIDX;"),
			Instruction::Lt              => write!(f, "\tLT;"),
			Instruction::Ltf             => write!(f, "\tLTF;"),
			Instruction::Lshift          => write!(f, "\tLSHIFT;"),
			Instruction::Minus           => write!(f, "\tMINUS;"),
			Instruction::Minusf          => write!(f, "\tMINUSF;"),
			Instruction::Movret          => write!(f, "\tMOVRET;"),
			Instruction::Mul             => write!(f, "\tMUL;"),
			Instruction::Mulf            => write!(f, "\tMULF;"),
			Instruction::Neq             => write!(f, "\tNEQ;"),
			Instruction::Nop             => write!(f, "\tNOP;"),
			Instruction::Not             => write!(f, "\tNOT;"),
			Instruction::Or              => write!(f, "\tOR;"),
			Instruction::Out             => write!(f, "\tOUT;"),
			Instruction::Outln           => write!(f, "\tOUTLN;"),
			Instruction::Pop             => write!(f, "\tPOP;"),
			Instruction::Push(literal)   => write!(f, "\tPUSH {};", literal),
			Instruction::Pushaddr(addr)  => write!(f, "\t&{};", addr),
			Instruction::Pusharg         => write!(f, "\tPUSHARG;"),
			Instruction::Pushret         => write!(f, "\tPUSHRET;"),
			Instruction::Rem             => write!(f, "\tREM;"),
			Instruction::Remf            => write!(f, "\tREMF;"),
			Instruction::Return          => write!(f, "\tRETURN;"),
			Instruction::Rshift          => write!(f, "\tRSHIFT;"),
			Instruction::Store           => write!(f, "\tSTORE;"),
			Instruction::Storeidx        => write!(f, "\tSTOREIDX;"),
			Instruction::Subcall         => write!(f, "\tSUBCALL;"),
			Instruction::Swap            => write!(f, "\tSWAP;"),
			Instruction::Xor             => write!(f, "\tXOR;"),
		}
	}
}

/// The possible literals found in the bytecode.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
	Integer(String),
	Float(String),
	String(String),
	Symbol(Symbol),
}

impl fmt::Display for Literal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Literal::Integer(value) => write!(f, "{}", value),
			Literal::Float(value)   => write!(f, "{}", value),
			Literal::String(value)  => write!(f, "\"{}\"", value),
			Literal::Symbol(symbol) => write!(f, "{}", symbol),
		}
	}
}

/// The possible symbols in the bytecode.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Symbol {
	Args,
	Locals,
}

impl fmt::Display for Symbol {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Symbol::Args   => write!(f, "ARGS"),
			Symbol::Locals => write!(f, "LOCALS"),
		}
	}
}
