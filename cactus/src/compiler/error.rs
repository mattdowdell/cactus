//! Error handling for the Cactus compiler.

use std::error;
use std::fmt;

use crate::location::Location;


/// Create a new syntax error.
///
/// This is typically used during parsing.
pub fn syntax_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::SyntaxError,
		location,
		message)
}

/// Create a new type error.
///
/// This is typically used during semantic analysis.
pub fn type_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::TypeError,
		location,
		message)
}

/// Create a new lookup error.
///
/// This is typically used during semantic analysis.
pub fn lookup_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::LookupError,
		location,
		message)
}

/// Create a new internal error.
///
/// This is used when something is not in the expected state which usually indicates an error
/// somewhere else in the cmpiler.
pub fn internal_error(error_code: ErrorCode, location: Location, message: String) -> CompilationError {
	CompilationError::new(error_code,
		ErrorType::InternalError,
		location,
		message)
}

/// The possible error codes.
///
/// The error code ranges are as follows:
/// - 0000-0199: Syntax error
/// - 0200-0399: Type error
/// - 0400-0599: Lookup error
/// - 1000-0999: Internal error
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
	E0207, // incorrect type fund for let statement value
	E0208, // incorrect type for prefix operator
	E0209, // incorrect type for infix operator

	// lookup errors
	E0400, // function not defined before usage
	E0401, // redefined function
	E0402, // redefined argument
	E0403, // redefined local
	E0404, // unudefined local/argument

	// internal errors
	E1000,
	E1001,
	E1002,
	E1003,
	E1004,
	E1005,
	E1006,
	E1007, // non prefix operator given to check_prefix_operator
	E1008, // non infix operator given to check_infix_operator

}

/// The possible types of errors.
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

/// A representation of a compilation error.
#[derive(Clone, Debug, PartialEq)]
pub struct CompilationError {
	error_code: ErrorCode,
	error_type: ErrorType,
	location: Location,
	message: String,
}

impl CompilationError {
	/// Create a new instance of `CompilationError`.
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
