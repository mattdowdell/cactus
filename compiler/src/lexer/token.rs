//! A representation of tokens generated from an input string and supporting structures.

use std::fmt;

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
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::Location;
	///
	/// let location = Location::new(1, 1);
	///
	/// assert_eq!(location.line, 1);
	/// assert_eq!(location.column, 1);
	/// ```
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
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::Location;
	///
	/// let location = Location::default();
	///
	/// assert_eq!(location.line, 1);
	/// assert_eq!(location.column, 0);
	/// ```
	pub fn default() -> Location {
		Location {
			line: 1,
			column: 0,
		}
	}

	/// Create a placeholder location used to indicate the end of the file.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::Location;
	///
	/// let location = Location::end();
	///
	/// assert_eq!(location.line, 0);
	/// assert_eq!(location.column, 0);
	/// ```
	pub fn end() -> Location {
		Location {
			line: 0,
			column: 0,
		}
	}

	/// Increment the column count of the location.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::Location;
	///
	/// let mut location = Location::default();
	///
	/// assert_eq!(location.line, 1);
	/// assert_eq!(location.column, 0);
	///
	/// location.increment();
	///
	/// assert_eq!(location.line, 1);
	/// assert_eq!(location.column, 1);
	/// ```
	pub fn increment(&mut self) {
		self.column += 1;
	}

	/// Increment the line count of the location and reset the column count back to 0.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::Location;
	///
	/// let mut location = Location::default();
	///
	/// assert_eq!(location.line, 1);
	/// assert_eq!(location.column, 0);
	///
	/// location.newline();
	///
	/// assert_eq!(location.line, 2);
	/// assert_eq!(location.column, 0);
	/// ```
	pub fn newline(&mut self) {
		self.line += 1;
		self.column = 0;
	}
}


/// A representation of a token type.
///
/// Can be grouped into the following categories:
/// - Specials
/// - Identifiers and Literals
/// - Operators
/// - Delimiters
/// - Brackets
/// - Keywords
/// - Primitive Types
///
/// Specials, Identifiers and Literals usually have a value associated with them when stored in a
/// `Token`.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
	// specials
	Eof,
	Illegal,

	// identifiers and literals
	Identifier,
	Integer,
	Float,

	// operators
	Assign,
	Plus,
	Minus,
	Multiply,
	Divide,
	Bang,

	// comparisons
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
	Equal,
	NotEqual,


	// delimiters
	Semicolon,
	Colon,
	Comma,
	Arrow,

	// brackets
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,

	// keywords
	Let,
	Function,
	Return,
	True,
	False,

	// primitive types
	TypeBool,
	TypeInt32,
	TypeFloat,
}


impl fmt::Display for TokenType {
	// Convert a token type to what it would look like in an input string.
	//
	// Tokens that hold values cannot be represented by any single value, so these default to the
	// debug representation instead.
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			// these don't have a single string representation
			// so if we get this far just output the debug value
			TokenType::Eof
			| TokenType::Illegal
			| TokenType::Identifier
			| TokenType::Integer
			| TokenType::Float => write!(f, "{:?}", self),

			// operators
			TokenType::Assign   => write!(f, "="),
			TokenType::Plus     => write!(f, "+"),
			TokenType::Minus    => write!(f, "-"),
			TokenType::Multiply => write!(f, "*"),
			TokenType::Divide   => write!(f, "/"),
			TokenType::Bang     => write!(f, "!"),

			// comparisons
			TokenType::LessThan           => write!(f, "<"),
			TokenType::LessThanOrEqual    => write!(f, "<="),
			TokenType::GreaterThan        => write!(f, ">"),
			TokenType::GreaterThanOrEqual => write!(f, ">="),
			TokenType::Equal              => write!(f, "=="),
			TokenType::NotEqual           => write!(f, "!="),

			// delimiters
			TokenType::Semicolon => write!(f, ";"),
			TokenType::Colon     => write!(f, ":"),
			TokenType::Comma     => write!(f, ","),
			TokenType::Arrow     => write!(f, "->"),

			// brackets
			TokenType::LeftParen  => write!(f, "("),
			TokenType::RightParen => write!(f, ")"),
			TokenType::LeftBrace  => write!(f, "{{"),
			TokenType::RightBrace => write!(f, "}}"),

			// keywords
			TokenType::Let      => write!(f, "let"),
			TokenType::Function => write!(f, "fn"),
			TokenType::Return   => write!(f, "return"),
			TokenType::True     => write!(f, "true"),
			TokenType::False    => write!(f, "false"),

			// primitive types
			TokenType::TypeBool  => write!(f, "bool"),
			TokenType::TypeInt32 => write!(f, "i32"),
			TokenType::TypeFloat   => write!(f, "float"),
		}
	}
}


/// A representation of a token.
///
/// A token is produced by the lexer when creaking the input into 'words' it can understand.
///
/// Each token has a type, e.g. "+" would become a plus token type, an optional value and a
/// location. The value is required for token types that can have different values, such as
/// numbers. If the token value can only be expressed in one way, such as an operator, then the
/// value will be omitted and is ignored if it is present. The location is the line and column
/// where the token started in the input.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub value: Option<String>,
	pub location: Location,
}


impl Token {
	/// Create a new instance of `Token`.
	///
	/// The should only be used for tokens that have a value. If this is not the case, consider
	/// using `Token::from_type` for `Token::from_ident`.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::{Location, Token, TokenType};
	///
	/// let location = Location::new(1, 1);
	/// let value = "10".to_string();
	/// let token = Token::new(TokenType::Integer, value, location);
	///
	/// assert_eq!(token.token_type, TokenType::Integer);
	/// assert_eq!(token.value.unwrap(), "10");
	/// assert_eq!(token.location.line, 1);
	/// assert_eq!(token.location.column, 1);
	/// ```
	pub fn new(token_type: TokenType, value: String, location: Location) -> Token {
		Token {
			token_type: token_type,
			value: Some(value),
			location: location,
		}
	}

	/// Create a new instance of `Token` from a known `TokenType`.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::{Location, Token, TokenType};
	///
	/// let location = Location::new(1, 1);
	/// let token = Token::from_type(TokenType::Plus, location);
	///
	/// assert_eq!(token.token_type, TokenType::Plus);
	/// assert!(token.value.is_none());
	/// assert_eq!(token.location.line, 1);
	/// assert_eq!(token.location.column, 1);
	/// ```
	pub fn from_type(token_type: TokenType, location: Location) -> Token {
		Token {
			token_type: token_type,
			value: None,
			location: location,
		}
	}

	/// Create a new instance of `Token` from a given value.
	///
	/// If the value matches a known keyword, the token produced will be for the keyword,
	/// otherwise the token will be for an identifier.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::{Location, Token, TokenType};
	///
	/// let location = Location::new(1, 1);
	/// let value = "true".to_string();
	/// let token = Token::from_ident(value, location);
	///
	/// assert_eq!(token.token_type, TokenType::True);
	/// assert!(token.value.is_none());
	/// assert_eq!(token.location.line, 1);
	/// assert_eq!(token.location.column, 1);
	/// ```
	pub fn from_ident(value: String, location: Location) -> Token {
		let tt = match value.as_ref() {
			// keywords
			"let"    => TokenType::Let,
			"fn"     => TokenType::Function,
			"return" => TokenType::Return,
			"true"   => TokenType::True,
			"false"  => TokenType::False,

			// primitive types
			"bool" => TokenType::TypeBool,
			"i32"  => TokenType::TypeInt32,
			"f32"  => TokenType::TypeFloat,

			// identifier (default)
			_ => TokenType::Identifier,
		};

		if tt == TokenType::Identifier {
			Token {
				token_type: tt,
				value: Some(value),
				location: location,
			}
		} else {
			Token {
				token_type: tt,
				value: None,
				location: location,
			}
		}
	}

	/// Create a new instance of `Token` representing the end of the file.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::{Token, TokenType};
	///
	/// let token = Token::eof();
	///
	/// assert_eq!(token.token_type, TokenType::Eof);
	/// assert!(token.value.is_none());
	/// assert_eq!(token.location.line, 0);
	/// assert_eq!(token.location.column, 0);
	/// ```
	pub fn eof() -> Token {
		Token {
			token_type: TokenType::Eof,
			value: None,
			location: Location::end(),
		}
	}
}


impl fmt::Display for Token {
	// Convert a token to what it would look like in an input string.
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.token_type {
			// tokens with values
			TokenType::Illegal
			| TokenType::Identifier
			| TokenType::Integer
			| TokenType::Float => write!(f, "{}", self.value.as_ref().unwrap().clone()),

			// tokens without values
			_ => write!(f, "{}", self.token_type),
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;

	// An instance of `Location` used as a placeholder as tokens aren't responsible for
	// discovering where they are relative to an input string.
	//
	// Tests for checking the location of tokens are correct can be found in the lexer tests.
	const LOCATION: Location = Location {
		line: 1,
		column: 1,
	};

	// Helper macro for creating a new token.
	macro_rules! token {
		($tt:expr, $value:expr) => (
			Token::new($tt, $value.to_string(), LOCATION);
		);
		($tt:expr) => (
			Token::from_type($tt, LOCATION);
		)
	}

	// Test that keywords are correctly matched.
	#[test]
	fn test_keywords() {
		assert_eq!(
			Token::from_ident("let".to_string(), LOCATION),
			token!(TokenType::Let)
		);

		assert_eq!(
			Token::from_ident("fn".to_string(), LOCATION),
			token!(TokenType::Function)
		);

		assert_eq!(
			Token::from_ident("return".to_string(), LOCATION),
			token!(TokenType::Return)
		);

		assert_eq!(
			Token::from_ident("true".to_string(), LOCATION),
			token!(TokenType::True)
		);

		assert_eq!(
			Token::from_ident("false".to_string(), LOCATION),
			token!(TokenType::False)
		);

		assert_eq!(
			Token::from_ident("bool".to_string(), LOCATION),
			token!(TokenType::TypeBool)
		);

		assert_eq!(
			Token::from_ident("i32".to_string(), LOCATION),
			token!(TokenType::TypeInt32)
		);

		assert_eq!(
			Token::from_ident("f32".to_string(), LOCATION),
			token!(TokenType::TypeFloat)
		);

	}

	// Test that a non-keyword produces and identifier.
	#[test]
	fn test_identifier() {
		assert_eq!(
			Token::from_ident("example".to_string(), LOCATION),
			token!(TokenType::Identifier, "example")
		);
	}
}
