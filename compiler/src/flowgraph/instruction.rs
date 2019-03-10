//!
//!
//!

use crate::parser::ast::{
	Literal,
	Operator,
};


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
	Leq,
	Lt,
	Minus,
	Movret,
	Mul,
	Neq,
	Not,
	Or,
	Push(Literal),
	Rem,
	Return,
}




impl Instruction {
	///
	///
	///
	pub fn push_from_literal(literal: Literal) -> Instruction {
		unimplemented!()
	}

	///
	///
	///
	pub fn from_operator(operator: Operator) -> Instruction {
		match operator {
			// prefix operators
			Operator::Not      => Instruction::Not,
			// TODO: unary minus
			Operator::BitCompl => Instruction::Compl,

			// infix operators
			Operator::Plus               => Instruction::Add,
			Operator::Minus              => Instruction::Minus,
			Operator::Multiply           => Instruction::Mul,
			Operator::Divide             => Instruction::Div,
			Operator::Modulo             => Instruction::Rem,
			Operator::Equal              => Instruction::Eq,
			Operator::NotEqual           => Instruction::Neq,
			Operator::LessThan           => Instruction::Lt,
			Operator::LessThanOrEqual    => Instruction::Leq,
			Operator::GreaterThan        => Instruction::Gt,
			Operator::GreaterThanOrEqual => Instruction::Geq,
			Operator::And                => Instruction::And,
			Operator::Or                 => Instruction::Or,
			// TODO: bitwise operators

			// assignment operators
			// TODO: add Assign back here

			// not yet implemented
			Operator::Assign
			| Operator::UnaryMinus
			| Operator::BitAnd
			| Operator::BitOr
			| Operator::BitXor
			| Operator::BitLeftShift
			| Operator::BitRightShift => {
				panic!("{:?} is not yet implemented for instructions", operator);
			},

			Operator::PlusAssign
			| Operator::MinusAssign
			| Operator::MultiplyAssign
			| Operator::DivideAssign
			| Operator::ModuloAssign
			| Operator::BitAndAssign
			| Operator::BitOrAssign
			| Operator::BitXorAssign
			| Operator::BitLeftShiftAssign
			| Operator::BitRightShiftAssign => {
				panic!("Unexpected operator {:?}. This should have been replaced by the analyser")
			},
		}
	}
}
