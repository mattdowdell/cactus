//! The Cactus lexer.

use std::iter::Peekable;
use std::str::Chars;

use crate::location::Location;
use crate::lexer::token::{Token, TokenType};

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
	pub fn new(input: &'a str) -> Lexer<'a> {
		Lexer {
			input: input.chars().peekable(),
			location: Location::start(),
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
				'+' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::PlusAssign, location))
					} else {
						Some(Token::from_type(TokenType::Plus, location))
					}
				},
				'-' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::MinusAssign, location))
					} else if self.peek_char_is('>', true) {
						Some(Token::from_type(TokenType::Arrow, location))
					} else {
						Some(Token::from_type(TokenType::Minus, location))
					}
				},
				'*' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::MultiplyAssign, location))
					} else {
						Some(Token::from_type(TokenType::Multiply, location))
					}
				},
				'/' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::DivideAssign, location))
					} else {
						Some(Token::from_type(TokenType::Divide, location))
					}
				},
				'%' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::ModuloAssign, location))
					} else {
						Some(Token::from_type(TokenType::Modulo, location))
					}
				},
				'!' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::NotEqual, location))
					} else {
						let value = c.to_string();
						Some(Token::new(TokenType::Illegal, value, location))
					}
				},
				'&' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::BitAndAssign, location))
					} else {
						Some(Token::from_type(TokenType::BitAnd, location))
					}
				},
				'|' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::BitOrAssign, location))
					} else {
						Some(Token::from_type(TokenType::BitOr, location))
					}
				},
				'^' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::BitXorAssign, location))
					} else {
						Some(Token::from_type(TokenType::BitXor, location))
					}
				},
				'~' => Some(Token::from_type(TokenType::BitCompl, location)),
				'<' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::LessThanOrEqual, location))
					} else if self.peek_char_is('<', true) {
						if self.peek_char_is('=', true) {
							Some(Token::from_type(TokenType::BitLeftShiftAssign, location))
						} else {
							Some(Token::from_type(TokenType::BitLeftShift, location))
						}
					} else {
						Some(Token::from_type(TokenType::LessThan, location))
					}
				},
				'>' => {
					if self.peek_char_is('=', true) {
						Some(Token::from_type(TokenType::GreaterThanOrEqual, location))
					} else if self.peek_char_is('>', true) {
						if self.peek_char_is('=', true) {
							Some(Token::from_type(TokenType::BitRightShiftAssign, location))
						} else {
							Some(Token::from_type(TokenType::BitRightShift, location))
						}
					} else {
						Some(Token::from_type(TokenType::GreaterThan, location))
					}
				},

				// delimiters (excluding arrow which is matched above)
				';' => Some(Token::from_type(TokenType::Semicolon, location)),
				':' => {
					if self.peek_char_is(':', true) {
						Some(Token::from_type(TokenType::ImportJoin, location))
					} else {
						Some(Token::from_type(TokenType::Colon, location))
					}
				},
				',' => Some(Token::from_type(TokenType::Comma, location)),
				'.' => Some(Token::from_type(TokenType::Dot, location)),

				// brackets
				'(' => Some(Token::from_type(TokenType::LeftParen, location)),
				')' => Some(Token::from_type(TokenType::RightParen, location)),
				'{' => Some(Token::from_type(TokenType::LeftBrace, location)),
				'}' => Some(Token::from_type(TokenType::RightBrace, location)),

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
		let lexer = Lexer::new("@#");
		let expected = vec![
			token!(TokenType::Illegal, "@", location!(1)),
			token!(TokenType::Illegal, "#", location!(2)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test operators are correctly matched.
	#[test]
	fn test_basic_operators() {
		let lexer = Lexer::new("+ - * / %");
		let expected = vec![
			token!(TokenType::Plus, location!(1)),
			token!(TokenType::Minus, location!(3)),
			token!(TokenType::Multiply, location!(5)),
			token!(TokenType::Divide, location!(7)),
			token!(TokenType::Modulo, location!(9)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test bitwise operators are correctly matched.
	#[test]
	fn test_bitwise_operators() {
		let lexer = Lexer::new("& | ^ ~ << >>");
		let expected = vec![
			token!(TokenType::BitAnd, location!(1)),
			token!(TokenType::BitOr, location!(3)),
			token!(TokenType::BitXor, location!(5)),
			token!(TokenType::BitCompl, location!(7)),
			token!(TokenType::BitLeftShift, location!(9)),
			token!(TokenType::BitRightShift, location!(12)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test assignment operators are correctly matched.
	#[test]
	fn test_assignment_operators() {
		let lexer = Lexer::new("= += -= *= /= %= &= |= ^= <<= >>=");
		let expected = vec![
			token!(TokenType::Assign, location!(1)),
			token!(TokenType::PlusAssign, location!(3)),
			token!(TokenType::MinusAssign, location!(6)),
			token!(TokenType::MultiplyAssign, location!(9)),
			token!(TokenType::DivideAssign, location!(12)),
			token!(TokenType::ModuloAssign, location!(15)),
			token!(TokenType::BitAndAssign, location!(18)),
			token!(TokenType::BitOrAssign, location!(21)),
			token!(TokenType::BitXorAssign, location!(24)),
			token!(TokenType::BitLeftShiftAssign, location!(27)),
			token!(TokenType::BitRightShiftAssign, location!(31)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test boolean operators are correctly matched.
	#[test]
	fn test_boolean_operators() {
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
		let lexer = Lexer::new("; : , -> :: .");
		let expected = vec![
			token!(TokenType::Semicolon, location!(1)),
			token!(TokenType::Colon, location!(3)),
			token!(TokenType::Comma, location!(5)),
			token!(TokenType::Arrow, location!(7)),
			token!(TokenType::ImportJoin, location!(10)),
			token!(TokenType::Dot, location!(13)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test brackets are correctly matched.
	#[test]
	fn test_brackets() {
		let lexer = Lexer::new("(){}");
		let expected = vec![
			token!(TokenType::LeftParen, location!(1)),
			token!(TokenType::RightParen, location!(2)),
			token!(TokenType::LeftBrace, location!(3)),
			token!(TokenType::RightBrace, location!(4)),
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
		let lexer = Lexer::new("let fn return struct enum import true false and or not if elif else loop continue break");
		let expected = vec![
			token!(TokenType::Let, location!(1)),
			token!(TokenType::Function, location!(5)),
			token!(TokenType::Return, location!(8)),
			token!(TokenType::Struct, location!(15)),
			token!(TokenType::Enum, location!(22)),
			token!(TokenType::Import, location!(27)),
			token!(TokenType::True, location!(34)),
			token!(TokenType::False, location!(39)),
			token!(TokenType::And, location!(45)),
			token!(TokenType::Or, location!(49)),
			token!(TokenType::Not, location!(52)),
			token!(TokenType::If, location!(56)),
			token!(TokenType::Elif, location!(59)),
			token!(TokenType::Else, location!(64)),
			token!(TokenType::Loop, location!(69)),
			token!(TokenType::Continue, location!(74)),
			token!(TokenType::Break, location!(83)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test primitive types are correctly matched.
	#[test]
	fn test_primitive_types() {
		let lexer = Lexer::new("bool i32 u32 f32");
		let expected = vec![
			token!(TokenType::TypeBool, location!(1)),
			token!(TokenType::TypeInt32, location!(6)),
			token!(TokenType::TypeUint32, location!(10)),
			token!(TokenType::TypeFloat, location!(14)),
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
		let lexer = Lexer::new("not x; -5; ~5;");
		let expected = vec![
			token!(TokenType::Not, location!(1)),
			token!(TokenType::Identifier, "x", location!(5)),
			token!(TokenType::Semicolon, location!(6)),
			token!(TokenType::Minus, location!(8)),
			token!(TokenType::Integer, "5", location!(9)),
			token!(TokenType::Semicolon, location!(10)),
			token!(TokenType::BitCompl, location!(12)),
			token!(TokenType::Integer, "5", location!(13)),
			token!(TokenType::Semicolon, location!(14)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test that infix expressions with basic operators are correctly broken into tokens
	#[test]
	fn test_basic_infix_expressions() {
		let lexer = Lexer::new("1 + 2; 1 - 2; 1 * 2; 1 / 2; 1 % 2;");
		let expected = vec![
			token!(TokenType::Integer, "1", location!(1)),
			token!(TokenType::Plus, location!(3)),
			token!(TokenType::Integer, "2", location!(5)),
			token!(TokenType::Semicolon, location!(6)),

			token!(TokenType::Integer, "1", location!(8)),
			token!(TokenType::Minus, location!(10)),
			token!(TokenType::Integer, "2", location!(12)),
			token!(TokenType::Semicolon, location!(13)),

			token!(TokenType::Integer, "1", location!(15)),
			token!(TokenType::Multiply, location!(17)),
			token!(TokenType::Integer, "2", location!(19)),
			token!(TokenType::Semicolon, location!(20)),

			token!(TokenType::Integer, "1", location!(22)),
			token!(TokenType::Divide, location!(24)),
			token!(TokenType::Integer, "2", location!(26)),
			token!(TokenType::Semicolon, location!(27)),

			token!(TokenType::Integer, "1", location!(29)),
			token!(TokenType::Modulo, location!(31)),
			token!(TokenType::Integer, "2", location!(33)),
			token!(TokenType::Semicolon, location!(34)),

		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test that infix expressions with bitwise operators are correctly broken into tokens
	#[test]
	fn test_bitwise_infix_expressions() {
		let lexer = Lexer::new("1 & 2; 1 | 2; 1 ^ 2; 1 << 2; 1 >> 2;");
		let expected = vec![
			token!(TokenType::Integer, "1", location!(1)),
			token!(TokenType::BitAnd, location!(3)),
			token!(TokenType::Integer, "2", location!(5)),
			token!(TokenType::Semicolon, location!(6)),

			token!(TokenType::Integer, "1", location!(8)),
			token!(TokenType::BitOr, location!(10)),
			token!(TokenType::Integer, "2", location!(12)),
			token!(TokenType::Semicolon, location!(13)),

			token!(TokenType::Integer, "1", location!(15)),
			token!(TokenType::BitXor, location!(17)),
			token!(TokenType::Integer, "2", location!(19)),
			token!(TokenType::Semicolon, location!(20)),

			token!(TokenType::Integer, "1", location!(22)),
			token!(TokenType::BitLeftShift, location!(24)),
			token!(TokenType::Integer, "2", location!(27)),
			token!(TokenType::Semicolon, location!(28)),

			token!(TokenType::Integer, "1", location!(30)),
			token!(TokenType::BitRightShift, location!(32)),
			token!(TokenType::Integer, "2", location!(35)),
			token!(TokenType::Semicolon, location!(36)),

		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	// Test that infix expressions with boolean operators are correctly broken into tokens
	#[test]
	fn test_boolean_infix_expressions() {
		let lexer = Lexer::new("1 == 2; 1 != 2; 1 < 2; 1 <= 2; 1 > 2; 1 >= 2; 1 and 2; 1 or 2;");
		let expected = vec![
			token!(TokenType::Integer, "1", location!(1)),
			token!(TokenType::Equal, location!(3)),
			token!(TokenType::Integer, "2", location!(6)),
			token!(TokenType::Semicolon, location!(7)),

			token!(TokenType::Integer, "1", location!(9)),
			token!(TokenType::NotEqual, location!(11)),
			token!(TokenType::Integer, "2", location!(14)),
			token!(TokenType::Semicolon, location!(15)),

			token!(TokenType::Integer, "1", location!(17)),
			token!(TokenType::LessThan, location!(19)),
			token!(TokenType::Integer, "2", location!(21)),
			token!(TokenType::Semicolon, location!(22)),

			token!(TokenType::Integer, "1", location!(24)),
			token!(TokenType::LessThanOrEqual, location!(26)),
			token!(TokenType::Integer, "2", location!(29)),
			token!(TokenType::Semicolon, location!(30)),

			token!(TokenType::Integer, "1", location!(32)),
			token!(TokenType::GreaterThan, location!(34)),
			token!(TokenType::Integer, "2", location!(36)),
			token!(TokenType::Semicolon, location!(37)),

			token!(TokenType::Integer, "1", location!(39)),
			token!(TokenType::GreaterThanOrEqual, location!(41)),
			token!(TokenType::Integer, "2", location!(44)),
			token!(TokenType::Semicolon, location!(45)),

			token!(TokenType::Integer, "1", location!(47)),
			token!(TokenType::And, location!(49)),
			token!(TokenType::Integer, "2", location!(53)),
			token!(TokenType::Semicolon, location!(54)),

			token!(TokenType::Integer, "1", location!(56)),
			token!(TokenType::Or, location!(58)),
			token!(TokenType::Integer, "2", location!(61)),
			token!(TokenType::Semicolon, location!(62)),

		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}

	#[test]
	fn test_assignment_statement() {
		let lexer = Lexer::new("x = 2; x += 2; x -= 2; x *= 2; x /= 2; x %= 2; x &= 2; x |= 2; x ^= 2; x <<= 2; x >>= 2;");
		let expected = vec![
			token!(TokenType::Identifier, "x", location!(1)),
			token!(TokenType::Assign, location!(3)),
			token!(TokenType::Integer, "2", location!(5)),
			token!(TokenType::Semicolon, location!(6)),

			token!(TokenType::Identifier, "x", location!(8)),
			token!(TokenType::PlusAssign, location!(10)),
			token!(TokenType::Integer, "2", location!(13)),
			token!(TokenType::Semicolon, location!(14)),

			token!(TokenType::Identifier, "x", location!(16)),
			token!(TokenType::MinusAssign, location!(18)),
			token!(TokenType::Integer, "2", location!(21)),
			token!(TokenType::Semicolon, location!(22)),

			token!(TokenType::Identifier, "x", location!(24)),
			token!(TokenType::MultiplyAssign, location!(26)),
			token!(TokenType::Integer, "2", location!(29)),
			token!(TokenType::Semicolon, location!(30)),

			token!(TokenType::Identifier, "x", location!(32)),
			token!(TokenType::DivideAssign, location!(34)),
			token!(TokenType::Integer, "2", location!(37)),
			token!(TokenType::Semicolon, location!(38)),

			token!(TokenType::Identifier, "x", location!(40)),
			token!(TokenType::ModuloAssign, location!(42)),
			token!(TokenType::Integer, "2", location!(45)),
			token!(TokenType::Semicolon, location!(46)),

			token!(TokenType::Identifier, "x", location!(48)),
			token!(TokenType::BitAndAssign, location!(50)),
			token!(TokenType::Integer, "2", location!(53)),
			token!(TokenType::Semicolon, location!(54)),

			token!(TokenType::Identifier, "x", location!(56)),
			token!(TokenType::BitOrAssign, location!(58)),
			token!(TokenType::Integer, "2", location!(61)),
			token!(TokenType::Semicolon, location!(62)),

			token!(TokenType::Identifier, "x", location!(64)),
			token!(TokenType::BitXorAssign, location!(66)),
			token!(TokenType::Integer, "2", location!(69)),
			token!(TokenType::Semicolon, location!(70)),

			token!(TokenType::Identifier, "x", location!(72)),
			token!(TokenType::BitLeftShiftAssign, location!(74)),
			token!(TokenType::Integer, "2", location!(78)),
			token!(TokenType::Semicolon, location!(79)),

			token!(TokenType::Identifier, "x", location!(81)),
			token!(TokenType::BitRightShiftAssign, location!(83)),
			token!(TokenType::Integer, "2", location!(87)),
			token!(TokenType::Semicolon, location!(88)),
		];

		for (i, token) in lexer.enumerate() {
			assert_eq!(token, expected[i]);
		}
	}
}
