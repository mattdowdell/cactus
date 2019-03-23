//!
//!
//!

use std::error;
use std::fmt;

use crate::location::Location;

#[derive(Clone, Debug, PartialEq)]
pub struct BytecodeError {
	error_type: ErrorType,
	error_code: ErrorCode,
	message: String,
	location: Location,
}

impl BytecodeError {
	///
	///
	///
	pub fn new(error_type: ErrorType, error_code: ErrorCode, location: Location, message: String) -> BytecodeError {
		BytecodeError {
			error_type: error_type,
			error_code: error_code,
			location: location,
			message: message,
		}
	}
}

impl error::Error for BytecodeError {
	fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl fmt::Display for BytecodeError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.location.line > 0 {
			write!(f, "{:?}: {} on L{}: {}",
				self.error_code,
				self.error_type,
				self.location.line,
				self.message)
		} else {
			write!(f, "{:?}: {}: {}",
				self.error_code,
				self.error_type,
				self.message)
		}
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorType {
	SyntaxError,
	LookupError,
}

impl fmt::Display for ErrorType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ErrorType::SyntaxError => write!(f, "Syntax Error"),
			ErrorType::LookupError => write!(f, "Lookup Error"),
		}
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorCode {
	// syntax errors
	E0001, // illegal character
	E0002, // unclosed string
	E0003, // illegal character in string
	E0004, // unclosed block comment
	E0005, // invalid floating point number, e.g. "123."
	E0006, // unexpected end of file
	E0007, // unexpected token

	// lookup errors
	E0200, // label not defined
	E0201, // invalid instruction index
}
