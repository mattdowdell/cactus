//!
//!
//!

use std::fmt;

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
	Add,
	Alloca,
	And,
	Compl,
	Div,
	Dumpstack,
	Dumpframe,
	Dup,
	Eq,
	Geq,
	Gt,
	Halt,
	In,
	Jmp,
	Jmpnz,
	Labeldef(String),
	Leq,
	Load,
	Loadidx,
	Lt,
	Minus,
	Movret,
	Mul,
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
	Return,
	Store,
	Storeidx,
	Subcall,
	Swap,
}

impl Instruction {
	pub fn push_symbol(symbol: Symbol) -> Instruction {
		Instruction::Push(Literal::Symbol(symbol))
	}

	pub fn push_offset(offset: usize) -> Instruction {
		Instruction::Push(Literal::Integer(offset.to_string()))
	}

	pub fn push_integer(value: String) -> Instruction {
		Instruction::Push(Literal::Integer(value))
	}

	pub fn push_float(value: String) -> Instruction {
		Instruction::Push(Literal::Float(value))
	}
}

impl fmt::Display for Instruction {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Instruction::Add             => write!(f, "\tADD;"),
			Instruction::Alloca          => write!(f, "\tALLOCA;"),
			Instruction::And             => write!(f, "\tAND;"),
			Instruction::Compl           => write!(f, "\tCOMPL;"),
			Instruction::Div             => write!(f, "\tDIV;"),
			Instruction::Dumpstack       => write!(f, "\tDUMPSTACK;"),
			Instruction::Dumpframe       => write!(f, "\tDUMPFRAME;"),
			Instruction::Dup             => write!(f, "\tDUP;"),
			Instruction::Eq              => write!(f, "\tEQ;"),
			Instruction::Geq             => write!(f, "\tGEQ;"),
			Instruction::Gt              => write!(f, "\tGT;"),
			Instruction::Halt            => write!(f, "\tHALT;"),
			Instruction::In              => write!(f, "\tIN;"),
			Instruction::Jmp             => write!(f, "\tJMP;"),
			Instruction::Jmpnz           => write!(f, "\tJMPNZ;"),
			Instruction::Labeldef(label) => write!(f, "{}:", label),
			Instruction::Leq             => write!(f, "\tLEQ;"),
			Instruction::Load            => write!(f, "\tLOAD;"),
			Instruction::Loadidx         => write!(f, "\tLOADIDX;"),
			Instruction::Lt              => write!(f, "\tLT;"),
			Instruction::Minus           => write!(f, "\tMINUS;"),
			Instruction::Movret          => write!(f, "\tMOVRET;"),
			Instruction::Mul             => write!(f, "\tMUL;"),
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
			Instruction::Return          => write!(f, "\tRETURN;"),
			Instruction::Store           => write!(f, "\tSTORE;"),
			Instruction::Storeidx        => write!(f, "\tSTOREIDX;"),
			Instruction::Subcall         => write!(f, "\tSUBCALL;"),
			Instruction::Swap            => write!(f, "\tSWAP;"),
		}
	}
}

///
///
///
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
	Integer(String),
	Float(String),
	Symbol(Symbol),
}

impl fmt::Display for Literal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Literal::Integer(value) => write!(f, "{}", value),
			Literal::Float(value)   => write!(f, "{}", value),
			Literal::Symbol(symbol) => write!(f, "{}", symbol),
		}
	}
}

///
///
///
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
