//!
//!
//!

use lexer::token::{Token, TokenType};
use parser::error::Error;


/// Operator precedences for Cactus.
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
	Lowest,
	Assignment,  // = or += or -=, etc.
	BoolOr,      // 'or'
	BoolAnd,     // 'and'
	BitOr,       // |
	BitXor,      // ^
	BitAnd,      // &
	Equals,      // == or !=
	LessGreater, // > or >= or < or <=
	Shift,       // << or >>
	Sum,         // + or -
	Product,     // * or / or %
	Prefix,      // -X or 'not X' or ~X
	Call,        // myFunction(X)
}


impl Precedence {
	/// Try to convert a `TokenType` to a `Precedence`.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
	/// use cactus::parser::precedence::Precedence;
	///
	/// let token = Token::from_type(TokenType::Equal, Location::new(1, 0));
	/// let precedence = Precedence::from_token(token);
	/// assert!(precedence.is_ok());
	/// assert_eq!(precedence.unwrap(), Precedence::Equals);
	///
	/// let token = Token::new(TokenType::Illegal, "@".to_string(), Location::new(1, 0));
	/// let precedence = Precedence::from_token(token);
	/// assert!(precedence.is_err());
	/// ```
	pub fn from_token(token: Token) -> Result<Precedence, Error> {
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

			_ => Err(Error::new(
				format!("Unable to convert TokenType to Precedence: {:?}.",
					token.token_type),
				token.location
			)),
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;

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
		unimplemented!()
	}
}
