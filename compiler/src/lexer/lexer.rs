//!
//!
//!

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
	///
	///
	///
	pub fn new(input: &'a str) -> Lexer<'a> {
		Lexer {
			input: input.chars().peekable(),
			location: Location::default(),
		}
	}

	//
	//
	//
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

	//
	//
	//
	fn peek_is(&mut self, c: char, consume: bool) -> bool {
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

	//
	//
	//
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

	//
	//
	//
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

	//
	//
	//
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
	///
	///
	///
	type Item = Token;

	///
	///
	///
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
				// operators
				'+' => Some(Token::from_type(TokenType::Plus, location)),
				'-' => {
					if self.peek_is('>', true) {
						Some(Token::from_type(TokenType::Arrow, location))
					} else {
						Some(Token::from_type(TokenType::Minus, location))
					}
				},
				'*' => Some(Token::from_type(TokenType::Multiply, location)),
				'/' => Some(Token::from_type(TokenType::Divide, location)),

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

	#[test]
	fn test_operators() {
		let lexer = Lexer::new("+-*/");
		let expected = vec![
			token!(TokenType::Plus, location!(1)),
			token!(TokenType::Minus, location!(2)),
			token!(TokenType::Multiply, location!(3)),
			token!(TokenType::Divide, location!(4)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

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
}
