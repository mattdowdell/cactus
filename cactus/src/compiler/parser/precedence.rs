//! Operator precedences in Cactus.
//!
//! While prefix operators are assigned a precedence, they cannot be converted to any of the
//! variants of `Precedence` directly. The code for managing prefix operator precedence is baked
//! into the parser to avoid conflict between prefix and infix operators.

use crate::error::{CompilationError, ErrorCode, syntax_error};
use crate::compiler::lexer::{Token, TokenType};


/// Operator precedences for Cactus.
///
/// The following variants are available (in order from lowest to highest precedence):
/// - `Lowest`: Used as a default if no other precedence can be derived.
/// - `Assignment`: For basic assignment or assignment with side-effects, e.g. `+=`.
/// - `BoolOr`: Logical OR.
/// - `BoolAnd`: Logical AND.
/// - `BitOr`: Bitwise OR.
/// - `BitXor`: Bitwise XOR.
/// - `BitAnd`: Bitwise AND.
/// - `Equals`: For simple equality or inequality checking.
/// - `LessGreater`: For less than or greater than operations, including less than or equal to and
///    greater than or equal to.
/// - `Shift`: Bitwise shift operations.
/// - `Sum`: Addition or subtraction operations.
/// - `Product`: Multiplication, division and modulo/remainder operations.
/// - `Prefix`: For prefix operators.
/// - `Call`: For function calls.
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
	Lowest,
	Assignment,
	BoolOr,
	BoolAnd,
	BitOr,
	BitXor,
	BitAnd,
	Equals,
	LessGreater,
	Shift,
	Sum,
	Product,
	Prefix,
	Call,
}


impl Precedence {
	/// Try to convert a `Token` to a `Precedence`.
	pub fn from_token(token: Token) -> Result<Precedence, CompilationError> {
		match token.token_type {
			TokenType::Assign
			| TokenType::PlusAssign
			| TokenType::MinusAssign
			| TokenType::MultiplyAssign
			| TokenType::DivideAssign
			| TokenType::ModuloAssign
			| TokenType::BitAndAssign
			| TokenType::BitOrAssign
			| TokenType::BitXorAssign
			| TokenType::BitLeftShiftAssign
			| TokenType::BitRightShiftAssign => Ok(Precedence::Assignment),

			TokenType::And => Ok(Precedence::BoolAnd),
			TokenType::Or  => Ok(Precedence::BoolOr),

			TokenType::BitOr  => Ok(Precedence::BitOr),
			TokenType::BitXor => Ok(Precedence::BitXor),
			TokenType::BitAnd => Ok(Precedence::BitAnd),

			TokenType::Equal
			| TokenType::NotEqual => Ok(Precedence::Equals),


			TokenType::LessThan
			| TokenType::LessThanOrEqual
			| TokenType::GreaterThan
			| TokenType::GreaterThanOrEqual => Ok(Precedence::LessGreater),

			TokenType::BitLeftShift
			| TokenType::BitRightShift => Ok(Precedence::Shift),

			TokenType::Plus
			| TokenType::Minus => Ok(Precedence::Sum),

			TokenType::Multiply
			| TokenType::Divide
			| TokenType::Modulo => Ok(Precedence::Product),

			TokenType::LeftParen
			| TokenType::Dot => Ok(Precedence::Call),

			_ => {
				Err(syntax_error(ErrorCode::E0000,
					token.location,
					format!("Unable to convert TokenType to Precedence: {:?}",
						token.token_type)))
			},
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;
	use crate::location::Location;

	// An instance of `Location` used as a placeholder.
	const LOCATION: Location = Location {
		line: 1,
		column: 1,
	};

	// Helper macro for creating a new token.
	macro_rules! token {
		($tt:expr, $value:expr) => (
			Token::new($tt, $value.to_string(), LOCATION);
		);
		($tt:expr) => (
			Token::from_type($tt, LOCATION);
		)
	}

	// Test precedence ordering
	#[test]
	fn test_precedence_ordering() {
		assert!(Precedence::Lowest < Precedence::Assignment);
		assert!(Precedence::Assignment < Precedence::BoolOr);
		assert!(Precedence::BoolOr < Precedence::BoolAnd);
		assert!(Precedence::BoolAnd < Precedence::BitOr);
		assert!(Precedence::BitOr < Precedence::BitXor);
		assert!(Precedence::BitXor < Precedence::BitAnd);
		assert!(Precedence::BitAnd < Precedence::Equals);
		assert!(Precedence::Equals < Precedence::LessGreater);
		assert!(Precedence::LessGreater < Precedence::Shift);
		assert!(Precedence::Shift < Precedence::Sum);
		assert!(Precedence::Sum < Precedence::Product);
		assert!(Precedence::Product < Precedence::Prefix);
		assert!(Precedence::Prefix < Precedence::Call);
	}

	// Test conversion
	#[test]
	fn test_from_token() {
		// attempt to convert all token types to a precedence
		let data = vec![
			// we expect precedence operators to fail
			// as we handle that precedence in the parser itself
			(token!(TokenType::BitCompl), false),
			(token!(TokenType::Not), false),

			// infix operators should all pass
			(token!(TokenType::Plus), true),
			(token!(TokenType::Minus), true),
			(token!(TokenType::Multiply), true),
			(token!(TokenType::Divide), true),
			(token!(TokenType::Modulo), true),
			(token!(TokenType::BitAnd), true),
			(token!(TokenType::BitOr), true),
			(token!(TokenType::BitXor), true),
			(token!(TokenType::BitLeftShift), true),
			(token!(TokenType::BitRightShift), true),
			(token!(TokenType::Assign), true),
			(token!(TokenType::PlusAssign), true),
			(token!(TokenType::MinusAssign), true),
			(token!(TokenType::MultiplyAssign), true),
			(token!(TokenType::DivideAssign), true),
			(token!(TokenType::ModuloAssign), true),
			(token!(TokenType::BitAndAssign), true),
			(token!(TokenType::BitOrAssign), true),
			(token!(TokenType::BitXorAssign), true),
			(token!(TokenType::BitLeftShiftAssign), true),
			(token!(TokenType::BitRightShiftAssign), true),
			(token!(TokenType::LessThan), true),
			(token!(TokenType::LessThanOrEqual), true),
			(token!(TokenType::GreaterThan), true),
			(token!(TokenType::GreaterThanOrEqual), true),
			(token!(TokenType::Equal), true),
			(token!(TokenType::NotEqual), true),
			(token!(TokenType::Dot), true),
			(token!(TokenType::And), true),
			(token!(TokenType::Or), true),

			// not strictly an operator, but used for function calls
			(token!(TokenType::LeftParen), true),

			// everything else should fail
			(token!(TokenType::Illegal), false),
			(token!(TokenType::Identifier), false),
			(token!(TokenType::Integer), false),
			(token!(TokenType::Float), false),
			(token!(TokenType::Semicolon), false),
			(token!(TokenType::Colon), false),
			(token!(TokenType::Comma), false),
			(token!(TokenType::Arrow), false),
			(token!(TokenType::ImportJoin), false),
			(token!(TokenType::RightParen), false),
			(token!(TokenType::LeftBrace), false),
			(token!(TokenType::RightBrace), false),
			(token!(TokenType::Let), false),
			(token!(TokenType::Function), false),
			(token!(TokenType::Return), false),
			(token!(TokenType::Struct), false),
			(token!(TokenType::Enum), false),
			(token!(TokenType::Import), false),
			(token!(TokenType::True), false),
			(token!(TokenType::False), false),
			(token!(TokenType::If), false),
			(token!(TokenType::Elif), false),
			(token!(TokenType::Else), false),
			(token!(TokenType::Loop), false),
			(token!(TokenType::Continue), false),
			(token!(TokenType::Break), false),
			(token!(TokenType::TypeBool), false),
			(token!(TokenType::TypeInt32), false),
			(token!(TokenType::TypeFloat), false),
		];

		for (token, expected) in data.iter() {
			let result = Precedence::from_token(token.clone());
			assert_eq!(&result.is_ok(), expected,
				"Unexpected result for TokenType::{:?}", token.token_type);
		}
	}
}
