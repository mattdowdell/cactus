//! A representation of tokens generated from an input string and supporting structures.

use std::fmt;

use crate::location::Location;
use super::token_type::TokenType;


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
	pub fn new(token_type: TokenType, value: String, location: Location) -> Token {
		Token {
			token_type: token_type,
			value: Some(value),
			location: location,
		}
	}

	/// Create a new instance of `Token` from a known `TokenType`.
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
	pub fn from_ident(value: String, location: Location) -> Token {
		let tt = match value.as_ref() {
			// keywords
			"let"      => TokenType::Let,
			"fn"       => TokenType::Function,
			"return"   => TokenType::Return,
			"struct"   => TokenType::Struct,
			"enum"     => TokenType::Enum,
			"import"   => TokenType::Import,
			"true"     => TokenType::True,
			"false"    => TokenType::False,
			"and"      => TokenType::And,
			"or"       => TokenType::Or,
			"not"      => TokenType::Not,
			"if"       => TokenType::If,
			"elif"     => TokenType::Elif,
			"else"     => TokenType::Else,
			"loop"     => TokenType::Loop,
			"continue" => TokenType::Continue,
			"break"    => TokenType::Break,

			// primitive types
			"bool" => TokenType::TypeBool,
			"i32"  => TokenType::TypeInt32,
			"u32"  => TokenType::TypeUint32,
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

	// An instance of `Location` used as a placeholder.
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
		let pairs = vec![
			("let", token!(TokenType::Let)),
			("fn", token!(TokenType::Function)),
			("return", token!(TokenType::Return)),
			("struct", token!(TokenType::Struct)),
			("enum", token!(TokenType::Enum)),
			("import", token!(TokenType::Import)),
			("continue", token!(TokenType::Continue)),
			("break", token!(TokenType::Break)),
			("true", token!(TokenType::True)),
			("false", token!(TokenType::False)),
			("and", token!(TokenType::And)),
			("or", token!(TokenType::Or)),
			("not", token!(TokenType::Not)),
			("if", token!(TokenType::If)),
			("elif", token!(TokenType::Elif)),
			("else", token!(TokenType::Else)),
			("loop", token!(TokenType::Loop)),
			("bool", token!(TokenType::TypeBool)),
			("i32", token!(TokenType::TypeInt32)),
			("u32", token!(TokenType::TypeUint32)),
			("f32", token!(TokenType::TypeFloat)),
		];

		for (input, expected) in pairs {
			assert_eq!(Token::from_ident(input.to_string(), LOCATION), expected);
		}
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
