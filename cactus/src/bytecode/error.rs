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
	RuntimeError,
}

impl fmt::Display for ErrorType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ErrorType::SyntaxError => write!(f, "Syntax Error"),
			ErrorType::LookupError => write!(f, "Lookup Error"),
			ErrorType::RuntimeError => write!(f, "Runtime Error"),
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
	E0202, // unable to load argument
	E0203, // unable to store local
	E0204, // unable to load local
	E0205, // unable to manipulate arguments using STORE/STOREIDX
	E0206, // PUSHRET on empty return register
	E0207, // attempt to pop from empty stack
	E0208, // non-integer on the stack
	E0209, // non-symbol on the stack
	E0210, // non-address on the stack

	// runtime errors
	E0400, // unable to convert value to i32
	E0401, // no input given
	E0402, // error reading from stdin
}
