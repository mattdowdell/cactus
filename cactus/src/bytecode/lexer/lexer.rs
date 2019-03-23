//! Lexer for Maude bytecode.

use std::iter::Peekable;
use std::str::Chars;

use crate::location::Location;
use crate::bytecode::error::{BytecodeError, ErrorType, ErrorCode};
use super::token::Token;
use super::token_type::TokenType;

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
	fn read_number(&mut self, first: char) -> Result<(TokenType, String), BytecodeError> {
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

		if number.ends_with(".") {
			Err(BytecodeError::new(ErrorType::SyntaxError,
				ErrorCode::E0005,
				self.location,
				format!("Invalid number found: \"{}\" - missing digits after the decimal point",
					number)))
		} else {
			match is_float {
				true  => Ok((TokenType::Float, number)),
				false => Ok((TokenType::Integer, number)),
			}
		}
	}
}


impl<'a> Iterator for Lexer<'a> {
	// The return type of the iterator.
	type Item = Result<Token, BytecodeError>;

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
				':' => Some(Ok(Token::new_from_type(TokenType::Colon, location))),
				';' => Some(Ok(Token::new_from_type(TokenType::Semicolon, location))),
				'&' => Some(Ok(Token::new_from_type(TokenType::Ampersand, location))),
				'{' => {
					if self.peek_char_is('-', true) {
						let mut value = String::from("");

						loop {
							let next = self.next_char();

							if next.is_none() {
								return Some(Err(BytecodeError::new(ErrorType::SyntaxError,
									ErrorCode::E0004,
									Location::end(),
									"Block comment was not closed before the end of the file".to_string())))
							}

							let next = next.unwrap();

							if next == '-' && self.peek_char_is('}', true) {
								let token = Token::new_with_value(TokenType::BlockComment,
									value,
									location);
								return Some(Ok(token));
							}

							value.push(next);
						}
					} else {
						Some(Err(BytecodeError::new(ErrorType::SyntaxError,
							ErrorCode::E0001,
							self.location,
							format!("Invalid character found: {}",
								c))))
					}
				},
				'-' => {
					// inline comment
					if self.peek_char_is('-', true) {
						let mut value = String::from("");

						loop {
							let next = self.next_char();

							if next.is_none() {
								return Some(Ok(Token::new_with_value(TokenType::InlineComment, value, location)));
							}

							let next = next.unwrap();

							if next == '\n' {
								value.push(next);

								return Some(Ok(Token::new_with_value(TokenType::InlineComment, value, location)));
							}

							value.push(next);
						}


					} else {
						let next = self.next_char();

						if next.is_none() {
							Some(Err(BytecodeError::new(ErrorType::SyntaxError,
								ErrorCode::E0001,
								self.location,
								format!("Invalid character found: {}",
									c))))

						} else {
							let next = next.unwrap();

							// negative integer or float
							if next.is_digit(10) {
								match self.read_number(next) {
									Ok((tt, value)) => {
										let value = format!("-{}", value);
										Some(Ok(Token::new_with_value(tt, value, location)))
									},
									Err(error) => Some(Err(error))
								}

							// error
							} else {
								Some(Err(BytecodeError::new(ErrorType::SyntaxError,
									ErrorCode::E0001,
									self.location,
									format!("Invalid character found: {}",
										c))))
							}
						}
					}
				},
				'"' => {
					let mut value = String::from("");

					loop {
						if self.peek_char_is('"', true) {
							break;
						}

						let next = self.next_char();

						if next.is_none() {
							return Some(Err(BytecodeError::new(ErrorType::SyntaxError,
								ErrorCode::E0002,
								Location::end(),
								"String was not closed before the end of the file".to_string())))
						}

						let next = next.unwrap();

						if is_printable_ascii(next) {
							value.push(next);
							continue;
						}

						return Some(Err(BytecodeError::new(ErrorType::SyntaxError,
							ErrorCode::E0003,
							self.location,
							format!("Invalid character found in string: {}",
								next))))
					}

					Some(Ok(Token::new_with_value(TokenType::String, value, location)))
				},
				_ => {
					// integer or float
					if c.is_digit(10) {
						match self.read_number(c) {
							Ok((tt, value)) => Some(Ok(Token::new_with_value(tt, value, location))),
							Err(error) => Some(Err(error))
						}

					// identifier or keyword
					} else if c.is_ascii_alphabetic() || c  == '_' {
						let value = self.read_identifier(c);
						Some(Ok(Token::new_from_ident(value, location)))

					// illegal
					} else {
						Some(Err(BytecodeError::new(ErrorType::SyntaxError,
							ErrorCode::E0001,
							self.location,
							format!("Invalid character found: {}",
								c))))
					}
				},
			}
		}
	}
}

// Check if a character is considered printable ASCII with the exceptions of `"` and `\` to avoid
// having to support escape characters.
//
// Based on https://en.wikipedia.org/wiki/ASCII#Character_set.
fn is_printable_ascii(c: char) -> bool {
	c >= ' ' && c <= '~' && c != '"' && c != '\\'
}


#[cfg(test)]
mod test {
	use super::*;

	// Macro for creating a `Location`.
	macro_rules! loc {
		($line: expr, $column: expr) => {
			Location {
				line: $line,
				column: $column,
			}
		};
	}

	// Macro for creating a `Token`.
	macro_rules! token {
		($tt: expr, $loc: expr) => {
			Token {
				token_type: $tt,
				value: None,
				location: $loc,
			}
		};
		($tt: expr, $value: expr, $loc: expr) => {
			Token {
				token_type: $tt,
				value: Some($value.to_string()),
				location: $loc,
			}
		};
	}

	// Test that delimiter tokens are correctly tokenised.
	#[test]
	fn test_delimiters() {
		let lexer = Lexer::new(":;&");
		let expected = vec![
			token!(TokenType::Colon, loc!(1, 1)),
			token!(TokenType::Semicolon, loc!(1, 2)),
			token!(TokenType::Ampersand, loc!(1, 3)),
		];

		for (index, res) in lexer.enumerate() {
			assert!(res.is_ok());

			let token = res.unwrap();
			let exp = &expected[index];

			assert_eq!(&token, exp);
		}
	}

	// Test that inline comments are correctly tokenised.
	#[test]
	fn test_inline_comment() {
		let mut lexer = Lexer::new("-- comment");
		let expected = token!(TokenType::InlineComment, " comment", loc!(1, 1));

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_ok());

		let token = res.unwrap();
		assert_eq!(token, expected);
	}

	// Test that block comments are correctly tokenised.
	#[test]
	fn test_block_comment() {
		let mut lexer = Lexer::new("{- comment -}");
		let expected = token!(TokenType::BlockComment, " comment ", loc!(1, 1));

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_ok());

		let token = res.unwrap();
		assert_eq!(token, expected);
	}

	// Test that an unclosed block comments is rejected.
	#[test]
	fn test_block_comment_unclosed() {
		let mut lexer = Lexer::new("{- comment");

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_err());
	}

	// Test that all printable ASCII characters are valid string characters.
	// Excludes `"` and `\`to avoid having to support escape characters.
	#[test]
	fn test_string() {
		let input = "\" !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~\"";
		let mut lexer = Lexer::new(input);
		let expected = token!(TokenType::String, input[1..input.len() - 1], loc!(1, 1));

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_ok());

		let token = res.unwrap();
		assert_eq!(token, expected);
	}

	// Test that unsupported string characters raise an error
	#[test]
	fn test_string_exception() {
		let input = "\"è\"";
		let mut lexer = Lexer::new(input);

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_err());
	}

	// Test that integers are correctly tokenised.
	#[test]
	fn test_integer() {
		let mut lexer = Lexer::new("123456");
		let expected = token!(TokenType::Integer, "123456", loc!(1, 1));

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_ok());

		let token = res.unwrap();
		assert_eq!(token, expected);
	}

	// Test that doubles are correctly tokenised.
	#[test]
	fn test_double() {
		let mut lexer = Lexer::new("123.456");
		let expected = token!(TokenType::Float, "123.456", loc!(1, 1));

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_ok());

		let token = res.unwrap();
		assert_eq!(token, expected);
	}

	// Test that negative integers are correctly tokenised.
	#[test]
	fn test_negative_integer() {
		let mut lexer = Lexer::new("-123456");
		let expected = token!(TokenType::Integer, "-123456", loc!(1, 1));

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_ok());

		let token = res.unwrap();
		assert_eq!(token, expected);
	}

	// Test that negative doubles are correctly tokenised.
	#[test]
	fn test_negative_double() {
		let mut lexer = Lexer::new("-123.456");
		let expected = token!(TokenType::Float, "-123.456", loc!(1, 1));

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_ok());

		let token = res.unwrap();
		assert_eq!(token, expected);
	}

	// Test that a double missing the fraction returns an error.
	#[test]
	fn test_double_no_fraction() {
		let mut lexer = Lexer::new("123.");

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_err());
	}

	#[test]
	fn test_unfinished_block_comment() {
		let mut lexer = Lexer::new("{ comment -}");

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_err());
	}

	#[test]
	fn test_unfinished_inline_comment() {
		let mut lexer = Lexer::new("- comment");

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_err());
	}

	#[test]
	fn test_illegal_char() {
		let mut lexer = Lexer::new("#");

		let opt = lexer.next();
		assert!(opt.is_some());

		let res = opt.unwrap();
		assert!(res.is_err());
	}
}

