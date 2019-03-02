//!
//!
//!

use std::error;
use std::fmt;

use crate::location::Location;

///
///
///
macro_rules! syntax_error {
	($code:path, $location:expr, $message:tt) => (
		CompilationError::new($code,
			ErrorType::SyntaxError,
			$location,
			$message.to_string())
	);
	($code:path, $location:expr, $message:tt, $($arg:expr),+) => (
		CompilationError::new($code,
			ErrorType::NotImplementedError,
			$location,
			format!($message, $($arg),+))
	);
}

///
///
///
macro_rules! type_error {
	($code:path, $location:expr, $message:tt) => (
		CompilationError::new($code,
			ErrorType::TypeError,
			$location,
			$message.to_string())
	);
	($code:path, $location:expr, $message:tt, $($arg:expr),+) => (
		CompilationError::new($code,
			ErrorType::TypeError,
			$location,
			format!($message, $($arg),+))
	);
}

///
///
///
macro_rules! lookup_error {
	($code:path, $location:expr, $message:tt) => (
		CompilationError::new($code,
			ErrorType::LookupError,
			$location,
			$message.to_string())
	);
	($code:path, $location:expr, $message:tt, $($arg:expr),+) => (
		CompilationError::new($code,
			ErrorType::NotImplementedError,
			$location,
			format!($message, $($arg),+))
	);
}

///
///
///
macro_rules! not_implemented_error {
	($code:path, $location:expr, $message:tt) => (
		CompilationError::new($code,
			ErrorType::NotImplementedError,
			$location,
			$message.to_string())
	);
	($code:path, $location:expr, $message:tt, $($arg:expr),+) => (
		CompilationError::new($code,
			ErrorType::NotImplementedError,
			$location,
			format!($message, $($arg),+))
	);
}

///
///
///
macro_rules! internal_error {
	($code:path, $location:expr, $message:tt) => (
		CompilationError::new($code,
			ErrorType::InternalError,
			$location,
			$message.to_string())
	);
	($code:path, $location:expr, $message:tt, $($arg:expr),+) => (
		CompilationError::new($code,
			ErrorType::InternalError,
			$location,
			format!($message, $($arg),+))
	);
}

///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorCode {
	// syntax errors
	E0000, // break statement not within loop
	E0001, // continue statement not within loop

	// type errors
	E0200, // let statement value does not match type hint
	E0201, // non-boolean expression if condition expression

	// lookup errors
	E0400, // function already defined
	E0401, // argument already defined
	E0402, // local already defined

	// import errors
	// E0600,

	// not implemented errors
	E0800, // type checking for custom types
	E0801, // imports
	E0802, // elif in if statements

	// internal errors
	E1000, // type_checker::TypeHint::from_ast_type for ast::Type::Unknown
}

///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorType {
	SyntaxError,
	TypeError,
	LookupError,
	ImportError,
	NotImplementedError,
	InternalError,
}

impl fmt::Display for ErrorType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ErrorType::SyntaxError         => write!(f, "Syntax Error"),
			ErrorType::TypeError           => write!(f, "Type Error"),
			ErrorType::LookupError         => write!(f, "Lookup Error"),
			ErrorType::ImportError         => write!(f, "Import Error"),
			ErrorType::NotImplementedError => write!(f, "Not Implemented Error"),
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
