//!
//!
//!

use std::fmt;

///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Location {
	pub line: usize,
	pub column: usize,
}


impl Location {
	///
	///
	///
	pub fn new(line: usize, column: usize) -> Location {
		Location {
			line: line,
			column: column,
		}
	}

	///
	///
	///
	pub fn default() -> Location {
		Location {
			line: 1,
			column: 0,
		}
	}

	///
	///
	///
	pub fn increment(&mut self) {
		self.column += 1;
	}

	///
	///
	///
	pub fn newline(&mut self) {
		self.line += 1;
		self.column = 0;
	}
}


///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
	// specials
	Illegal,

	// identifiers and literals
	Identifier,
	Integer,

	// operators
	Plus,
	Minus,
	Multiply,
	Divide,

	// delimiters
	Semicolon,
	Comma,

	// brackets
	LeftParen,
	RightParen,

	// keywords
	Let,
	Function,
	Return,
	True,
	False,
}


impl fmt::Display for TokenType {
	//
	//
	//
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			// these have values, so if we get this far just output the debug value
			TokenType::Illegal
			| TokenType::Identifier
			| TokenType::Integer => write!(f, "{:?}", self),

			// operators
			TokenType::Plus     => write!(f, "+"),
			TokenType::Minus    => write!(f, "-"),
			TokenType::Multiply => write!(f, "*"),
			TokenType::Divide   => write!(f, "/"),

			// delimiters
			TokenType::Semicolon => write!(f, ";"),
			TokenType::Comma     => write!(f, ","),

			// brackets
			TokenType::LeftParen  => write!(f, "("),
			TokenType::RightParen => write!(f, ")"),

			// keywords
			TokenType::Let      => write!(f, "let"),
			TokenType::Function => write!(f, "fn"),
			TokenType::Return   => write!(f, "return"),
			TokenType::True     => write!(f, "true"),
			TokenType::False    => write!(f, "false"),
		}
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub value: Option<String>,
	pub location: Location,
}


impl Token {
	///
	///
	///
	pub fn new(token_type: TokenType, value: String, location: Location) -> Token {
		Token {
			token_type: token_type,
			value: Some(value),
			location: location,
		}
	}

	///
	///
	///
	pub fn from_type(token_type: TokenType, location: Location) -> Token {
		Token {
			token_type: token_type,
			value: None,
			location: location,
		}
	}

	///
	///
	///
	pub fn from_ident(value: String, location: Location) -> Token {
		let tt = match value.as_ref() {
			// keywords
			"let"    => TokenType::Let,
			"fn"     => TokenType::Function,
			"return" => TokenType::Return,
			"true"   => TokenType::True,
			"false"  => TokenType::False,

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
}


impl fmt::Display for Token {
	//
	//
	//
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.token_type {
			// tokens with values
			TokenType::Illegal
			| TokenType::Identifier
			| TokenType::Integer => write!(f, "{}", self.value.as_ref().unwrap().clone()),

			// tokens without values
			_ => write!(f, "{}", self.token_type),
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;

	const LOCATION: Location = Location {
		line: 1,
		column: 1,
	};

	macro_rules! token {
		($tt:expr, $value:expr) => (
			// The macro will expand into the contents of this block.
			Token::new($tt, $value.to_string(), LOCATION);
		);
		($tt:expr) => (
			// The macro will expand into the contents of this block.
			Token::from_type($tt, LOCATION);
		)
	}

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

	}

	#[test]
	fn test_identifier() {
		assert_eq!(
			Token::from_ident("example".to_string(), LOCATION),
			token!(TokenType::Identifier, "example")
		);
	}
}
