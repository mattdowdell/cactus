//! An implementation of a lexer as an iterator.
//!
//! # Usage
//! ```
//! use compiler::lexer::lexer::Lexer;
//!
//! let lexer = Lexer::new("let x: i32 = 1");
//!
//! for token in lexer {
//!     // do something with the tokens here
//!     println!("{:?}", token);
//! }
//! ```

use std::iter::Peekable;
use std::str::Chars;

use lexer::token::{
	Location,
	Token,
	TokenType,
};

// Alternative to `char.is_whitespace()` that excludes form feeds as it's rare you would want them
// in code (excluding strings).
//
// Should be used to test for whitespace outside of strings.
fn is_whitespace(c: char) -> bool {
	c == ' ' || c == '\t' || c == '\r' || c == '\n'
}

///
///
///
pub struct Lexer<'a> {
	input: Peekable<Chars<'a>>,
	location: Location,
}


impl<'a> Lexer<'a> {
	/// Create a new instance of `Lexer`.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::lexer::Lexer;
	///
	/// Lexer::new("let x: i32 = 1;");
	/// ```
	pub fn new(input: &'a str) -> Lexer<'a> {
		Lexer {
			input: input.chars().peekable(),
			location: Location::default(),
		}
	}

	// Get the next char from the input iterator.
	//
	// Broadly the same as calling `next`, but also tracks newlines and thus the position in the
	// input string in terms of lines and columns.
	fn next_char(&mut self) -> Option<char> {
		let next = self.input.next();

		if next.is_some() {
			if next == Some('\n') {
				self.location.newline();
			} else {
				self.location.increment();
			}
		}

		next
	}

	// Helper function for testing is the next character is a specific char with the option to
	// consume if a match is found.
	fn peek_char_is(&mut self, c: char, consume: bool) -> bool {
		if self.input.peek().is_some() {
			if *self.input.peek().unwrap() == c {
				if consume {
					self.next_char();
				}

				return true;
			}
		}

		return false;
	}

	// Move the input iterator to the next non-whitespace character.
	fn skip_whitespace(&mut self) {
		loop {
			// stop if the end of the input has been reached
			if self.input.peek().is_none() {
				break;
			}

			// stop if the char isn't whitespace
			if !is_whitespace(*self.input.peek().unwrap()) {
				break;
			}

			self.next_char();
		}
	}

	// Read an identifier from the input, using the given char as the first character.
	//
	// Identifiers must start with ASCII alphabetic characters or an underscore and then can be any
	// ASCII alphanumeric character or an underscore.
	fn read_identifier(&mut self, first: char) -> String {
		let mut ident = first.to_string();

		while self.input.peek().is_some() {
			let c = *self.input.peek().clone().unwrap();

			if c.is_ascii_alphabetic() || c.is_digit(10) || c == '_' {
				ident.push(c);
				self.next_char();

				continue;
			}

			break;
		}

		ident
	}

	// Read a number from the input, using the given char as the first character.
	//
	// Numbers can be integers or floats consisting of decimal digits.
	fn read_number(&mut self, first: char) -> (TokenType, String) {
		let mut number = first.to_string();
		let mut is_float = false;

		while self.input.peek().is_some() {
			let c = *self.input.peek().clone().unwrap();

			if c.is_digit(10) {
				number.push(c);
				self.next_char();

				continue;
			} else if c == '.' && !is_float {
				is_float = true;

				number.push(c);
				self.next_char();

				continue;
			}

			break;
		}

		match is_float {
			true  => (TokenType::Float, number),
			false => (TokenType::Integer, number),
		}
	}
}


impl<'a> Iterator for Lexer<'a> {
	// The return type of the iterator.
	type Item = Token;

	// Move the iterator forward by one.
	fn next(&mut self) -> Option<Self::Item> {
		// jump to the next non-whitespace character
		self.skip_whitespace();

		let next = self.next_char();
		let location = self.location;

		if next.is_none() {
			None
		} else {
			let c = next.unwrap();
			match c {
				// operators and comparisons
				'=' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::Equal, location))
					} else {
						Some(Token::from_type(TokenType::Assign, location))
					}
				}
				'+' => Some(Token::from_type(TokenType::Plus, location)),
				'-' => {
					if self.peek_char_is('>', true) {
						Some(Token::from_type(TokenType::Arrow, location))
					} else {
						Some(Token::from_type(TokenType::Minus, location))
					}
				},
				'*' => Some(Token::from_type(TokenType::Multiply, location)),
				'/' => Some(Token::from_type(TokenType::Divide, location)),
				'!' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::NotEqual, location))
					} else {
						Some(Token::from_type(TokenType::Bang, location))
					}
				},
				'<' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::LessThanOrEqual, location))
					} else {
						Some(Token::from_type(TokenType::LessThan, location))
					}
				},
				'>' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::GreaterThanOrEqual, location))
					} else {
						Some(Token::from_type(TokenType::GreaterThan, location))
					}
				},

				// delimiters
				';' => Some(Token::from_type(TokenType::Semicolon, location)),
				':' => Some(Token::from_type(TokenType::Colon, location)),
				',' => Some(Token::from_type(TokenType::Comma, location)),

				// brackets
				'(' => Some(Token::from_type(TokenType::LeftParen, location)),
				')' => Some(Token::from_type(TokenType::RightParen, location)),

				_ => {
					// integer
					if c.is_digit(10) {
						let (tt, value) = self.read_number(c);
						Some(Token::new(tt, value, location))

					// identifier or keyword
					} else if c.is_ascii_alphabetic() || c  == '_' {
						let value = self.read_identifier(c);
						Some(Token::from_ident(value, location))

					// illegal
					} else {
						let value = c.to_string();
						Some(Token::new(TokenType::Illegal, value, location))
					}
				}
			}
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;

	macro_rules! token {
		($tt:expr, $value:expr, $location:expr) => (
			Token::new($tt, $value.to_string(), $location);
		);
		($tt:expr, $location:expr) => (
			Token::from_type($tt, $location);
		);
	}

	macro_rules! location {
		($line:expr, $column:expr) => (
			Location::new($line, $column);
		);
		($column:expr) => (
			Location::new(1, $column);
		);
	}

	// Test illegal characters are correctly matched.
	#[test]
	fn test_illegal() {
		let lexer = Lexer::new("@ ~");
		let expected = vec![
			token!(TokenType::Illegal, "@", location!(1)),
			token!(TokenType::Illegal, "~", location!(3)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test operators are correctly matched.
	#[test]
	fn test_operators() {
		let lexer = Lexer::new("=+-*/!");
		let expected = vec![
			token!(TokenType::Assign, location!(1)),
			token!(TokenType::Plus, location!(2)),
			token!(TokenType::Minus, location!(3)),
			token!(TokenType::Multiply, location!(4)),
			token!(TokenType::Divide, location!(5)),
			token!(TokenType::Bang, location!(6)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test comparisons are correctly matched.
	#[test]
	fn test_comparisons() {
		let lexer = Lexer::new("< > <= >= == !=");
		let expected = vec![
			token!(TokenType::LessThan, location!(1)),
			token!(TokenType::GreaterThan, location!(3)),
			token!(TokenType::LessThanOrEqual, location!(5)),
			token!(TokenType::GreaterThanOrEqual, location!(8)),
			token!(TokenType::Equal, location!(11)),
			token!(TokenType::NotEqual, location!(14)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test delimiters are correctly matched.
	#[test]
	fn test_delimiters() {
		let lexer = Lexer::new(";:,->");
		let expected = vec![
			token!(TokenType::Semicolon, location!(1)),
			token!(TokenType::Colon, location!(2)),
			token!(TokenType::Comma, location!(3)),
			token!(TokenType::Arrow, location!(4)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test brackets are correctly matched.
	#[test]
	fn test_brackets() {
		let lexer = Lexer::new("()");
		let expected = vec![
			token!(TokenType::LeftParen, location!(1)),
			token!(TokenType::RightParen, location!(2)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test integers are correctly matched.
	#[test]
	fn test_integer() {
		let lexer = Lexer::new("12");
		let expected = vec![
			token!(TokenType::Integer, "12", location!(1)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test floats are correctly matched.
	#[test]
	fn test_float() {
		let lexer = Lexer::new("12.0");
		let expected = vec![
			token!(TokenType::Float, "12.0", location!(1)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test identifiers are correctly matched.
	#[test]
	fn test_identifier() {
		let lexer = Lexer::new("a A _ _a _A _0 a0");
		let expected = vec![
			token!(TokenType::Identifier, "a", location!(1)),
			token!(TokenType::Identifier, "A", location!(3)),
			token!(TokenType::Identifier, "_", location!(5)),
			token!(TokenType::Identifier, "_a", location!(7)),
			token!(TokenType::Identifier, "_A", location!(10)),
			token!(TokenType::Identifier, "_0", location!(13)),
			token!(TokenType::Identifier, "a0", location!(16)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test keywords are correctly matched.
	#[test]
	fn test_keywords() {
		let lexer = Lexer::new("let fn return true false");
		let expected = vec![
			token!(TokenType::Let, location!(1)),
			token!(TokenType::Function, location!(5)),
			token!(TokenType::Return, location!(8)),
			token!(TokenType::True, location!(15)),
			token!(TokenType::False, location!(20)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test primitive types are correctly matched.
	#[test]
	fn test_primitive_types() {
		let lexer = Lexer::new("bool i32 f32");
		let expected = vec![
			token!(TokenType::TypeBool, location!(1)),
			token!(TokenType::TypeInt32, location!(6)),
			token!(TokenType::TypeFloat, location!(10)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test that newlines correctly increment the location data.
	#[test]
	fn test_newline() {
		let lexer = Lexer::new("foo\nbar");
		let expected = vec![
			token!(TokenType::Identifier, "foo", location!(1)),
			token!(TokenType::Identifier, "bar", location!(2, 1)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test that prefix expressions are correctly broken into tokens
	#[test]
	fn test_prefix_expressions() {
		let lexer = Lexer::new("!x; -5;");
		let expected = vec![
			token!(TokenType::Bang, location!(1)),
			token!(TokenType::Identifier, "x", location!(2)),
			token!(TokenType::Semicolon, location!(3)),
			token!(TokenType::Minus, location!(5)),
			token!(TokenType::Integer, "5", location!(6)),
			token!(TokenType::Semicolon, location!(7)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}
}
