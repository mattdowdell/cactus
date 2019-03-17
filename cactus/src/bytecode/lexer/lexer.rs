//! Lexer for Maude to convert an input string into a set of tokens.

use std::iter::Peekable;
use std::str::Chars;

use error::{ErrorCode, Exception};
use token::{CharLocation, Token, TokenType};

// Used when the end of the input string is detected.
const EOF: char = 0 as char;

/// Converts an input string into tokens to be used by the parser.
pub struct Lexer<'a> {
	chars: Peekable<Chars<'a>>,
	prev: char,
	line: usize,
	column: usize,
	peek_cl: Option<CharLocation>,
}

impl<'a> Lexer<'a> {
	/// Create a new instance of a `Lexer`.
	///
	/// # Example
	/// ```
	/// use maude::lexer::Lexer;
	///
	/// Lexer::new("HALT");
	/// ```
	pub fn new(input: &'a str) -> Lexer<'a> {
		Lexer {
			chars: input.chars().peekable(),
			prev: EOF,
			line: 1,
			column: 0,
			peek_cl: None,
		}
	}

	/// Read the next token from the input string.
	///
	/// # Example
	/// ```
	/// use maude::lexer::Lexer;
	/// use maude::token::TokenType;
	///
	/// let mut lexer = Lexer::new("HALT");
	/// let token = lexer.next_token();
	///
	/// assert_eq!(token.unwrap().token_type, TokenType::Halt);
	/// ```
	pub fn next_token(&mut self) -> Result<Token, Exception> {
		self.skip_whitespace();

		let cl = self.next_char();

		match cl.char {
			EOF => Ok(Token::new_from_type(TokenType::EndOfFile, cl)),
			':' => Ok(Token::new_from_type(TokenType::Colon, cl)),
			';' => Ok(Token::new_from_type(TokenType::Semicolon, cl)),
			'&' => Ok(Token::new_from_type(TokenType::Ampersand, cl)),
			'{' => {
				let peek = self.peek_char();

				if peek.char == '-' {
					let mut string = String::from("");
					// consume next char
					self.next_char();

					loop {
						let mut next = self.next_char();

						if next.char == '-' {
							let peek = self.peek_char();

							if peek.char == '}' {
								// consume next char
								self.next_char();
								return Ok(Token::new_with_value(
									TokenType::BlockComment,
									string,
									cl,
								));
							}
						} else if next.char == EOF {
							return Err(exc!(ErrorCode::E0001, cl.line, cl.column, 1));
						}

						string.push(next.char);
					}
				} else {
					Err(exc!(
						ErrorCode::E0000,
						peek.line,
						peek.column,
						1,
						debug!('-'),
						debug!(peek.char)
					))
				}
			}
			'-' => {
				let peek = self.peek_char();

				// inline comment
				if peek.char == '-' {
					let mut string = String::from("");
					// consume next character
					self.next_char();

					loop {
						let next = self.next_char();

						if next.char == '\n' || next.char == EOF {
							if next.char == '\n' {
								string.push(next.char);
							}

							return Ok(Token::new_with_value(TokenType::InlineComment, string, cl));
						}

						string.push(next.char);
					}

				// negatve double or integer
				} else if is_ascii_digit(peek.char) {
					let (tt, value) = self.read_number(cl)?;
					Ok(Token::new_with_value(tt, value, cl))

				// error
				} else {
					Err(exc!(
						ErrorCode::E0000,
						peek.line,
						peek.column,
						1,
						"'-' or digit",
						debug!(peek.char)
					))
				}
			}
			'"' => {
				let mut value = String::from("");

				loop {
					let next = self.next_char();

					if next.char == '"' {
						return Ok(Token::new_with_value(TokenType::String, value, cl));
					}

					if is_printable_ascii(next.char) {
						value.push(next.char);
						continue;
					}

					return Err(exc!(ErrorCode::E0003, next.line, next.column, 1, next.char));
				}
			}
			_ => {
				if is_ascii_alpha(cl.char) || cl.char == '_' {
					let ident = self.read_ident(cl);
					Ok(Token::new_from_ident(ident, cl))
				} else if is_ascii_digit(cl.char) {
					let (tt, value) = self.read_number(cl)?;
					Ok(Token::new_with_value(tt, value, cl))
				} else {
					Err(exc!(
						ErrorCode::E0002,
						cl.line,
						cl.column,
						1,
						debug!(cl.char)
					))
				}
			}
		}
	}

	// Read the next character from the input string.
	fn next_char(&mut self) -> CharLocation {
		if self.peek_cl.is_some() {
			let ret = self.peek_cl.unwrap();
			self.peek_cl = None;

			ret
		} else {
			self.read_char()
		}
	}

	// Read the next character from the input string without moving the iterator for the input
	// string forward.
	fn peek_char(&mut self) -> CharLocation {
		if self.peek_cl.is_some() {
			self.peek_cl.unwrap()
		} else {
			self.peek_cl = Some(self.read_char());
			self.peek_cl.unwrap()
		}
	}

	// Read the next character from the input string. Used by `Lexer::next_char` and
	// `Lexer::peek_char`.
	fn read_char(&mut self) -> CharLocation {
		let next = self.chars.next();

		if next.is_some() {
			let c = next.unwrap();

			if self.prev == '\n' {
				self.line += 1;
				self.column = 1;
			} else {
				self.column += 1;
			}

			self.prev = c;

			CharLocation {
				char: c,
				line: self.line,
				column: self.column,
			}
		} else {
			CharLocation {
				char: EOF,
				line: self.line,
				column: self.column,
			}
		}
	}

	// Skip over whitespace characters in the input if found.
	fn skip_whitespace(&mut self) {
		loop {
			let cl = self.peek_char();

			if is_whitespace(cl.char) {
				// consume whitespace character
				self.next_char();
				continue;
			}

			break;
		}
	}

	// Read an identifier from the input string.
	fn read_ident(&mut self, cl: CharLocation) -> String {
		let mut ident = String::from("");
		ident.push(cl.char);

		loop {
			let peek = self.peek_char();

			if is_ascii_alpha(peek.char) || is_ascii_digit(peek.char) || peek.char == '_' {
				ident.push(peek.char);
				// consume peeked character
				self.next_char();
				continue;
			}

			break;
		}

		ident
	}

	// Read an integer or double from the input string.
	fn read_number(&mut self, cl: CharLocation) -> Result<(TokenType, String), Exception> {
		let mut number = String::from("");
		let mut found_double = false;
		let tt: TokenType;

		number.push(cl.char);

		loop {
			let peek = self.peek_char();

			if is_ascii_digit(peek.char) {
				number.push(peek.char);
			} else if !found_double && peek.char == '.' {
				number.push(peek.char);
				found_double = true;
			} else {
				break;
			}

			// consume peeked char
			self.next_char();
		}

		if found_double {
			let last = number.chars().nth(number.len() - 1).unwrap();

			// check for "123."
			if !is_ascii_digit(last) {
				return Err(exc!(
					ErrorCode::E0004,
					cl.line,
					cl.column,
					number.len(),
					number
				));
			}

			tt = TokenType::Double
		} else {
			tt = TokenType::Integer
		}

		Ok((tt, number))
	}
}

// Check if a character is a whitespace character. Matches spaces, tabs, newlines and
// carriage returns.
fn is_whitespace(c: char) -> bool {
	c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

// Check if a character is an ASCII alphabetic character, e.g. A-Z or a-z.
fn is_ascii_alpha(c: char) -> bool {
	(c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

// Check if a character is an ASCII digit.
fn is_ascii_digit(c: char) -> bool {
	c >= '0' && c <= '9'
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

	// Macro for creating a `CharLocation`.
	macro_rules! char_loc {
		($char: expr, $line: expr, $column: expr) => {
			CharLocation {
				char: $char,
				line: $line,
				column: $column,
				}
		};
	}

	// Macro for creating a `Token`.
	macro_rules! token {
		($tt: expr, $line: expr, $column: expr) => {
			Token {
				token_type: $tt,
				value: None,
				line: $line,
				column: $column,
				}
		};
		($tt: expr, $value: expr, $line: expr, $column: expr) => {
			Token {
				token_type: $tt,
				value: Some($value.to_string()),
				line: $line,
				column: $column,
				}
		};
	}

	// Test that `Lexer::read_char` returns the correct line and column number for a
	// given input.
	#[test]
	fn test_read_char() {
		let mut lexer = Lexer::new("-- comment");
		let expected = vec![
			char_loc!('-', 1, 1),
			char_loc!('-', 1, 2),
			char_loc!(' ', 1, 3),
			char_loc!('c', 1, 4),
			char_loc!('o', 1, 5),
			char_loc!('m', 1, 6),
			char_loc!('m', 1, 7),
			char_loc!('e', 1, 8),
			char_loc!('n', 1, 9),
			char_loc!('t', 1, 10),
			char_loc!(EOF, 1, 10),
		];

		for exp in expected.iter() {
			let cl = lexer.read_char();

			assert_eq!(&cl, exp);
		}
	}

	// Test that `Lexer::next_char` returns the correct line and column number for a
	// given input.
	#[test]
	fn test_next_char() {
		let mut lexer = Lexer::new("-- comment");
		let expected = vec![
			char_loc!('-', 1, 1),
			char_loc!('-', 1, 2),
			char_loc!(' ', 1, 3),
			char_loc!('c', 1, 4),
			char_loc!('o', 1, 5),
			char_loc!('m', 1, 6),
			char_loc!('m', 1, 7),
			char_loc!('e', 1, 8),
			char_loc!('n', 1, 9),
			char_loc!('t', 1, 10),
			char_loc!(EOF, 1, 10),
		];

		for exp in expected.iter() {
			let cl = lexer.next_char();

			assert_eq!(&cl, exp);
		}
	}

	// Test that `Lexer::next_char` and `Lexer::peek_char` return the correct line and
	// column number for a given input.
	#[test]
	fn test_next_char_with_peek() {
		let mut lexer = Lexer::new("-- comment");
		let expected = vec![
			char_loc!('-', 1, 1),
			char_loc!('-', 1, 2),
			char_loc!(' ', 1, 3),
			char_loc!('c', 1, 4),
			char_loc!('o', 1, 5),
			char_loc!('m', 1, 6),
			char_loc!('m', 1, 7),
			char_loc!('e', 1, 8),
			char_loc!('n', 1, 9),
			char_loc!('t', 1, 10),
			char_loc!(EOF, 1, 10),
		];

		for exp in expected.iter() {
			let peek_cl = lexer.peek_char();
			assert_eq!(&peek_cl, exp);

			let next_cl = lexer.next_char();
			assert_eq!(&next_cl, exp);
		}
	}

	// Test that delimiter tokens are correctly tokenised.
	#[test]
	fn test_delimiters() {
		let mut lexer = Lexer::new(":;&");
		let expected = vec![
			token!(TokenType::Colon, 1, 1),
			token!(TokenType::Semicolon, 1, 2),
			token!(TokenType::Ampersand, 1, 3),
		];

		for exp in expected.iter() {
			let tok = lexer.next_token();

			assert_eq!(&tok.unwrap(), exp);
		}
	}

	// Test that inline comments are correctly tokenised.
	#[test]
	fn test_inline_comment() {
		let mut lexer = Lexer::new("-- comment");
		let expected = token!(TokenType::InlineComment, " comment", 1, 1);
		let token = lexer.next_token();

		assert_eq!(token.unwrap(), expected);
	}

	// Test that block comments are correctly tokenised.
	#[test]
	fn test_block_comment() {
		let mut lexer = Lexer::new("{- comment -}");
		let expected = token!(TokenType::BlockComment, " comment ", 1, 1);
		let token = lexer.next_token();

		assert_eq!(token.unwrap(), expected);
	}

	// Test that an unclosed block comments is rejected.
	#[test]
	fn test_block_comment_unclosed() {
		let mut lexer = Lexer::new("{- comment");
		let token = lexer.next_token();

		assert!(token.is_err());

		let exc = token.err().unwrap();

		assert_eq!(exc.header(), "ERROR (E0001) found on line: 1, column: 1");
		assert_eq!(exc.footer(), "Unclosed block comment");
	}

	// Test that all printable ASCII characters are valid string characters.
	// Excludes `"` and `\`to avoid having to support escape characters.
	#[test]
	fn test_string() {
		let input = "\" !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~\"";
		let mut lexer = Lexer::new(input);
		let expected = token!(TokenType::String, input[1..input.len() - 1], 1, 1);
		let token = lexer.next_token();

		assert_eq!(token.unwrap(), expected);
	}

	// Test that unsupported string characters raise an error
	#[test]
	fn test_string_exception() {
		let input = "\"è\"";
		let mut lexer = Lexer::new(input);
		let token = lexer.next_token();

		assert!(token.is_err());

		let exc = token.err().unwrap();
		assert_eq!(exc.header(), "ERROR (E0003) found on line: 1, column: 2");
		assert_eq!(exc.footer(), "Illegal character found in string: è");
	}

	// Test that integers are correctly tokenised.
	#[test]
	fn test_integer() {
		let mut lexer = Lexer::new("123456");
		let expected = token!(TokenType::Integer, "123456", 1, 1);
		let token = lexer.next_token();

		assert_eq!(token.unwrap(), expected);
	}

	// Test that doubles are correctly tokenised.
	#[test]
	fn test_double() {
		let mut lexer = Lexer::new("123.456");
		let expected = token!(TokenType::Double, "123.456", 1, 1);
		let token = lexer.next_token();

		assert_eq!(token.unwrap(), expected);
	}

	// Test that negative integers are correctly tokenised.
	#[test]
	fn test_negative_integer() {
		let mut lexer = Lexer::new("-123456");
		let expected = token!(TokenType::Integer, "-123456", 1, 1);
		let token = lexer.next_token();

		assert_eq!(token.unwrap(), expected);
	}

	// Test that negative doubles are correctly tokenised.
	#[test]
	fn test_negative_double() {
		let mut lexer = Lexer::new("-123.456");
		let expected = token!(TokenType::Double, "-123.456", 1, 1);
		let token = lexer.next_token();

		assert_eq!(token.unwrap(), expected);
	}

	// Test that a double missing the fraction returns an error.
	#[test]
	fn test_double_no_fraction() {
		let mut lexer = Lexer::new("123.");

		let res = lexer.next_token();
		assert!(res.is_err());

		let exc = res.err().unwrap();
		assert_eq!(
			exc.header(),
			"ERROR (E0004) found on line: 1, column: 1"
		);
		assert_eq!(exc.footer(), "Invalid number: 123.");
	}

	#[test]
	fn test_unfinished_block_comment() {
		let mut lexer = Lexer::new("{ comment -}");

		let res = lexer.next_token();
		assert!(res.is_err());

		let exc = res.err().unwrap();
		assert_eq!(
			exc.header(),
			"ERROR (E0000) found on line: 1, column: 2"
		);
		assert_eq!(
			exc.footer(),
			"Unexpected character - expected: '-', found: ' '"
		);
	}

	#[test]
	fn test_unfinished_inline_comment() {
		let mut lexer = Lexer::new("- comment");

		let res = lexer.next_token();
		assert!(res.is_err());

		let exc = res.err().unwrap();
		assert_eq!(
			exc.header(),
			"ERROR (E0000) found on line: 1, column: 2"
		);
		assert_eq!(
			exc.footer(),
			"Unexpected character - expected: '-' or digit, found: ' '"
		);
	}

	#[test]
	fn test_illegal_char() {
		let mut lexer = Lexer::new("#");

		let res = lexer.next_token();
		assert!(res.is_err());

		let exc = res.err().unwrap();
		assert_eq!(
			exc.header(),
			"ERROR (E0002) found on line: 1, column: 1"
		);
		assert_eq!(exc.footer(), "Illegal character found: '#'");
	}
}
