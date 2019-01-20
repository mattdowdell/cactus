//! Errors that can occur during parsing and the associated error codes.


use std::fmt;
use lexer::token::Location;

/// Codes that map to errors that can occur during parsing.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorCode {
	E0001,
}


impl fmt::Display for ErrorCode {
	// Convert error codes to human readable messages.
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ErrorCode::E0001 => write!(f, "Unknown"),
		}
	}
}

/// A representation of an error and it's location.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Error {
	pub location: Location,
	pub code: ErrorCode,
}


impl Error {
	/// Create a new instance of `Error`
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::Location;
	/// use compiler::parser::error::{Error, ErrorCode};
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
			code: code,
		}
	}
}


impl fmt::Display for Error {
	// Convert errors to human readable messages.
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Error on L{}: {:?}: {}", self.location.line, self.code, self.code)
	}
}
