//! Errors that can occur during parsing and the associated error codes.


use std::fmt;
use lexer::token::{Location, Token, TokenType};

/// Codes that map to errors that can occur during parsing.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorCode {
	E0000,
	E0001,
	E0002,
	E0003,
	E0004,
	E0005,

	E1000,
	E1001,
	E1002,
	E1003,
	E1004,
}


/// A representation of an error and it's location.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Error {
	pub location: Location,
	pub token_type: Option<TokenType>,
	pub code: ErrorCode,
}


impl Error {
	/// Create a new instance of `Error`
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::Location;
	/// use cactus::parser::error::{Error, ErrorCode};
	///
	/// let location = Location::new(5, 20);
	/// let error = Error::new(ErrorCode::E0001, location);
	///
	/// assert_eq!(error.location.line, 5);
	/// assert_eq!(error.location.column, 20);
	/// assert_eq!(error.code, ErrorCode::E0001);
	/// ```
	pub fn new(code: ErrorCode, location: Location) -> Error {
		Error {
			location: location,
			token_type: None,
			code: code,
		}
	}

	///
	///
	///
	pub fn from_token(code: ErrorCode, token: Option<Token>) -> Error {
		if token.is_none() {
			Error {
				location: Location::end(),
				token_type: None,
				code: code,
			}
		} else {
			let token = token.unwrap();
			Error {
				location: token.location,
				token_type: Some(token.token_type),
				code: code,
			}
		}
	}
}


impl fmt::Display for Error {
	// Convert errors to human readable messages.
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let reason = match self.code {
			ErrorCode::E0001 => format!("Internal error: Unable to convert {:?} to literal.", self.token_type),
			ErrorCode::E0002 => format!("Internal error: Unable to convert {:?} to identifier.", self.token_type),
			ErrorCode::E0003 => format!("Internal error: Unable to convert {:?} to prefix operator.", self.token_type),
			ErrorCode::E0004 => format!("Internal error: Unable to convert {:?} to infix operator.", self.token_type),
			ErrorCode::E0005 => format!("Internal error: Unable to derive precedence of {:?}.", self.token_type),

			ErrorCode::E1000 => format!("Unexpected token: {} ({:?}). Expected {} ({:?})",
				self.token_type, self.token_type, TokenType::RightParen, TokenType::RightParen),
			ErrorCode::E1001 => format!("Unexpected token: {} ({:?}). Expected one of: identifier, integer, float, true, false, !, - or (.",
				self.token_type, self.token_type),
			ErrorCode::E1002 => format!("Unexpected token: {} ({:?}). Expected {} ({:?})",
				self.token_type, self.token_type, TokenType::LeftBrace, TokenType::LeftBrace),
			ErrorCode::E1003 => format!("Unexpected token: {} ({:?}). Expected {} ({:?})",
				self.token_type, self.token_type, TokenType::RightBrace, TokenType::RightBrace),
			ErrorCode::E1004 => format!("Unexpected token: {} ({:?}). Expected identifier",
				self.token_type, self.token_type),
			ErrorCode::E1005 => format!("Unexpected token: {} ({:?}). Expected type",
				self.token_type, self.token_type),
			ErrorCode::E1006 => format!("Unexpected token: {} ({:?}). Expected type",
				self.token_type, self.token_type, TokenType::RightParen, TokenType::RightParen),
			ErrorCode::E1006 => format!("Unexpected token: {} ({:?}). Expected type",
				self.token_type, self.token_type, TokenType::Colon, TokenType::Colon

			_ => "Internal error: Unknown".to_string(),
		};

		write!(f, "Error on L{}: {:?}: {}", self.location.line, self.code, reason)
	}
}
