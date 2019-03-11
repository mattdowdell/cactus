//!
//!
//!

use std::fmt;

use crate::parser::ast;


pub trait Label {
	fn loop_start(id: usize) -> Self;
	fn loop_end(id: usize) -> Self;

	fn if_start(id: usize) -> Self;
	fn if_body(id: usize) -> Self;
	fn if_end(id: usize) -> Self;
}

impl Label for String {
	fn loop_start(id: usize) -> String {
		format!("loop_start_{}", id)
	}

	fn loop_end(id: usize) -> String {
		format!("loop_end_{}", id)
	}

	fn if_start(id: usize) -> String {
		format!("if_start_{}", id)
	}

	fn if_body(id: usize) -> String {
		format!("if_body_{}", id)
	}

	fn if_end(id: usize) -> String {
		format!("if_end_{}", id)
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
	Add,
	And,
	Compl,
	Div,
	Eq,
	Geq,
	Gt,
	Halt,
	Jmp,
	Jmpnz,
	Labeldef(String),
	Leq,
	Loadidx,
	Lt,
	Minus,
	Movret,
	Mul,
	Neq,
	Not,
	Or,
	Push(Literal),
	Pushaddr(String),
	Pusharg,
	Pushret,
	Rem,
	Return,
	Storeidx,
	Subcall,
}


// instructions not in use yet
/*	Pop,
	Dup,       // use for optimisations?
	Swap,
	Dumpstack, // for debugging, no need for it
	Alloca,    // should be used to allocate space for locals
	Out,       // add  print statement for this
	Outln,     // add println statement for this
	In,
	Store,     // shortcut for storeidx
	Load,      // shortcut for loadidx
*/

impl Instruction {
	///
	///
	///
	pub fn from_operator(operator: ast::Operator) -> Instruction {
		match operator {
			// prefix operators
			ast::Operator::Not      => Instruction::Not,
			// TODO: unary minus
			ast::Operator::BitCompl => Instruction::Compl,

			// infix operators
			ast::Operator::Plus               => Instruction::Add,
			ast::Operator::Minus              => Instruction::Minus,
			ast::Operator::Multiply           => Instruction::Mul,
			ast::Operator::Divide             => Instruction::Div,
			ast::Operator::Modulo             => Instruction::Rem,
			ast::Operator::Equal              => Instruction::Eq,
			ast::Operator::NotEqual           => Instruction::Neq,
			ast::Operator::LessThan           => Instruction::Lt,
			ast::Operator::LessThanOrEqual    => Instruction::Leq,
			ast::Operator::GreaterThan        => Instruction::Gt,
			ast::Operator::GreaterThanOrEqual => Instruction::Geq,
			ast::Operator::And                => Instruction::And,
			ast::Operator::Or                 => Instruction::Or,
			// TODO: bitwise operators

			// assignment operators
			// TODO: add Assign back here

			// not yet implemented
			ast::Operator::Assign
			| ast::Operator::UnaryMinus
			| ast::Operator::BitAnd
			| ast::Operator::BitOr
			| ast::Operator::BitXor
			| ast::Operator::BitLeftShift
			| ast::Operator::BitRightShift => {
				panic!("{:?} is not yet implemented for instructions", operator);
			},

			ast::Operator::PlusAssign
			| ast::Operator::MinusAssign
			| ast::Operator::MultiplyAssign
			| ast::Operator::DivideAssign
			| ast::Operator::ModuloAssign
			| ast::Operator::BitAndAssign
			| ast::Operator::BitOrAssign
			| ast::Operator::BitXorAssign
			| ast::Operator::BitLeftShiftAssign
			| ast::Operator::BitRightShiftAssign => {
				panic!("Unexpected operator {:?}. This should have been replaced by the analyser")
			},
		}
	}

	///
	///
	///
	pub fn new_loop_label(block_id: usize, is_start: bool) -> Instruction {
		if is_start {
			Instruction::Labeldef(String::loop_start(block_id))
		} else {
			Instruction::Labeldef(String::loop_end(block_id))
		}
	}
}

impl fmt::Display for Instruction {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Instruction::Add             => write!(f, "\tADD;"),
			Instruction::And             => write!(f, "\tAND;"),
			Instruction::Compl           => write!(f, "\tCOMPL;"),
			Instruction::Div             => write!(f, "\tDIV;"),
			Instruction::Eq              => write!(f, "\tEQ;"),
			Instruction::Geq             => write!(f, "\tGEQ;"),
			Instruction::Gt              => write!(f, "\tGT;"),
			Instruction::Halt            => write!(f, "\tHALT;"),
			Instruction::Jmp             => write!(f, "\tJMP;"),
			Instruction::Jmpnz           => write!(f, "\tJMPNZ;"),
			Instruction::Labeldef(label) => write!(f, "{}:", label),
			Instruction::Leq             => write!(f, "\tLEQ;"),
			Instruction::Loadidx         => write!(f, "\tLOADIDX;"),
			Instruction::Lt              => write!(f, "\tLT;"),
			Instruction::Minus           => write!(f, "\tMINUS;"),
			Instruction::Movret          => write!(f, "\tMOVRET;"),
			Instruction::Mul             => write!(f, "\tMUL;"),
			Instruction::Neq             => write!(f, "\tNEQ;"),
			Instruction::Not             => write!(f, "\tNOT;"),
			Instruction::Or              => write!(f, "\tOR;"),
			Instruction::Push(literal)   => write!(f, "\tPUSH {};", literal),
			Instruction::Pushaddr(addr)  => write!(f, "\t&{};", addr),
			Instruction::Pusharg         => write!(f, "\tPUSHARG;"),
			Instruction::Pushret         => write!(f, "\tPUSHRET;"),
			Instruction::Rem             => write!(f, "\tREM;"),
			Instruction::Return          => write!(f, "\tRETURN;"),
			Instruction::Storeidx        => write!(f, "\tSTOREIDX;"),
			Instruction::Subcall         => write!(f, "\tSUBCALL;"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
	Integer(String),
	Float(String),
	Symbol(Symbol),
}

impl Literal {
	///
	///
	///
	pub fn from_ast_literal(literal: ast::Literal) -> Literal {
		match literal {
			ast::Literal::Integer(value) => {
				Literal::Integer(value)
			},
			ast::Literal::Float(value) => {
				Literal::Float(value)
			},
			ast::Literal::Boolean(value) => {
				if value {
					Literal::Integer("1".to_string())
				} else {
					Literal::Integer("0".to_string())
				}
			},
		}
	}
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
