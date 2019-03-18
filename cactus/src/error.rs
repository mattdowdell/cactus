//!
//!
//!

use std::error;
use std::fmt;

use crate::location::Location;


///
///
///
pub fn syntax_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::SyntaxError,
		location,
		message)
}

///
///
///
pub fn type_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::TypeError,
		location,
		message)
}

///
///
///
pub fn lookup_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::LookupError,
		location,
		message)
}

///
///
///
pub fn internal_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::InternalError,
		location,
		message)
}

///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorCode {
	// syntax errors
	E0000, // unable to convert token type to precedence
	E0001, // unexpected end of file
	E0002, // unexpected token
	E0003, // non-identifier starting call expression
	E0004, // break outside of loop
	E0005, // continue outside of loop
	E0006, // assignment used in sub expression

	// type errors
	E0200,
	E0201,
	E0202, // not enough arguments in function call
	E0203, // incorrect type arguments in function call
	E0204, // non-boolean type for conditional
	E0205, // mismatched operand types for infix expression
	E0206, // incorrect type found on return statement

	// lookup errors
	E0400, // function not defined before usage
	E0401, // redefined function

	// internal errors
	E1000,
	E1001,
	E1002,
	E1003,
	E1004,
	E1005,

}

///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorType {
	SyntaxError,
	TypeError,
	LookupError,
	//NotImplementedError,
	InternalError,
}

impl fmt::Display for ErrorType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ErrorType::SyntaxError         => write!(f, "Syntax Error"),
			ErrorType::TypeError           => write!(f, "Type Error"),
			ErrorType::LookupError         => write!(f, "Lookup Error"),
			//ErrorType::ImportError         => write!(f, "Import Error"),
			//ErrorType::NotImplementedError => write!(f, "Not Implemented Error"),
			ErrorType::InternalError       => write!(f, "Internal Error"),
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct CompilationError {
	error_code: ErrorCode,
	error_type: ErrorType,
	location: Location,
	message: String,
}

impl CompilationError {
	///
	///
	///
	pub fn new(error_code: ErrorCode, error_type: ErrorType, location: Location, message: String) -> CompilationError {
		CompilationError {
			error_code: error_code,
			error_type: error_type,
			location: location,
			message: message,
		}
	}
}

impl error::Error for CompilationError {
	fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl fmt::Display for CompilationError {
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
