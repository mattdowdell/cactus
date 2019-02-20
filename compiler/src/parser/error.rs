//!
//!
//!

use std::fmt;

use lexer::location::Location;


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Error {
	message: String,
	location: Location,
}

impl Error {
	///
	///
	///
	pub fn new(message: String, location: Location) -> Error {
		Error {
			message: message,
			location: location,
		}
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Error on L{}: {}", self.location.line, self.message)
	}
}
