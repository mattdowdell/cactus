//! A representation of a location relative to the input source code.

/// To be used to store a location in an input string.
///
/// This might be the location of a specific character or token, or the current position of the
/// lexer in the input string.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}


impl Location {
	/// Create a new instance of `Location` with a specific line and column.
	pub fn new(line: usize, column: usize) -> Location {
		Location {
			line: line,
			column: column,
		}
	}

	/// Create a new instance of `Location` with a line and column for the start of an input.
	///
	/// The column is set to 0 as it is expected to be incremented when the first character is
	/// read in the input.
	pub fn start() -> Location {
		Location {
			line: 1,
			column: 0,
		}
	}

	/// Create a placeholder location used to indicate the end of the file.
	pub fn end() -> Location {
		Location {
			line: 0,
			column: 0,
		}
	}

	/// Increment the column count of the location.
	pub fn increment(&mut self) {
		self.column += 1;
	}

	/// Increment the line count of the location and reset the column count back to 0.
	pub fn newline(&mut self) {
		self.line += 1;
		self.column = 0;
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_location_new() {
		let location = Location::new(1, 1);

		assert_eq!(location.line, 1);
		assert_eq!(location.column, 1);
	}

	#[test]
	fn test_location_start() {
		let location = Location::start();

		assert_eq!(location.line, 1);
		assert_eq!(location.column, 0);
	}

	#[test]
	fn test_location_end() {
		let location = Location::end();

		assert_eq!(location.line, 0);
		assert_eq!(location.column, 0);
	}

	#[test]
	fn test_location_increment() {
		let mut location = Location::start();

		assert_eq!(location.line, 1);
		assert_eq!(location.column, 0);

		location.increment();

		assert_eq!(location.line, 1);
		assert_eq!(location.column, 1);
	}

	#[test]
	fn test_location_newline() {
		let mut location = Location::start();

		assert_eq!(location.line, 1);
		assert_eq!(location.column, 0);

		location.newline();

		assert_eq!(location.line, 2);
		assert_eq!(location.column, 0);
	}
}
