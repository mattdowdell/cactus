//! A representation of tokens generated from an input string and supporting structures.

use std::fmt;

use lexer::location::Location;


/// A representation of a token type.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
	// specials
	Eof,
	Illegal,

	// identifiers and literals
	Identifier,
	Integer,
	Float,

	// basic operators
	Plus,
	Minus,
	Multiply,
	Divide,
	Modulo,

	// bitwise operators
	BitAnd,
	BitOr,
	BitXor,
	BitCompl,
	BitLeftShift,
	BitRightShift,

	// assignment operators
	Assign,
	PlusAssign,
	MinusAssign,
	MultiplyAssign,
	DivideAssign,
	ModuloAssign,
	BitAndAssign,
	BitOrAssign,
	BitXorAssign,
	BitLeftShiftAssign,
	BitRightShiftAssign,

	// boolean operators
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
	ImportJoin,
	Dot,

	// brackets
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,

	// keywords
	Let,
	Function,
	Return,
	Struct,
	Enum,
	Import,
	True,
	False,
	And,
	Or,
	Not,
	If,
	Elif,
	Else,
	Loop,
	Continue,
	Break,

	// primitive types
	TypeBool,
	TypeInt32,
	TypeUint32,
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

			// basic operators
			TokenType::Plus     => write!(f, "+"),
			TokenType::Minus    => write!(f, "-"),
			TokenType::Multiply => write!(f, "*"),
			TokenType::Divide   => write!(f, "/"),
			TokenType::Modulo   => write!(f, "%"),

			// bitwise operators
			TokenType::BitAnd        => write!(f, "&"),
			TokenType::BitOr         => write!(f, "|"),
			TokenType::BitXor        => write!(f, "^"),
			TokenType::BitCompl      => write!(f, "~"),
			TokenType::BitLeftShift  => write!(f, "<<"),
			TokenType::BitRightShift => write!(f, ">>"),

			// assignment operators
			TokenType::Assign              => write!(f, "="),
			TokenType::PlusAssign          => write!(f, "+="),
			TokenType::MinusAssign         => write!(f, "-="),
			TokenType::MultiplyAssign      => write!(f, "*="),
			TokenType::DivideAssign        => write!(f, "/="),
			TokenType::ModuloAssign        => write!(f, "%="),
			TokenType::BitAndAssign        => write!(f, "&="),
			TokenType::BitOrAssign         => write!(f, "|="),
			TokenType::BitXorAssign        => write!(f, "^="),
			TokenType::BitLeftShiftAssign  => write!(f, "<<="),
			TokenType::BitRightShiftAssign => write!(f, ">>="),

			// boolean operators
			TokenType::LessThan           => write!(f, "<"),
			TokenType::LessThanOrEqual    => write!(f, "<="),
			TokenType::GreaterThan        => write!(f, ">"),
			TokenType::GreaterThanOrEqual => write!(f, ">="),
			TokenType::Equal              => write!(f, "=="),
			TokenType::NotEqual           => write!(f, "!="),

			// delimiters
			TokenType::Semicolon  => write!(f, ";"),
			TokenType::Colon      => write!(f, ":"),
			TokenType::Comma      => write!(f, ","),
			TokenType::Arrow      => write!(f, "->"),
			TokenType::ImportJoin => write!(f, "::"),
			TokenType::Dot        => write!(f, "."),

			// brackets
			TokenType::LeftParen  => write!(f, "("),
			TokenType::RightParen => write!(f, ")"),
			TokenType::LeftBrace  => write!(f, "{{"),
			TokenType::RightBrace => write!(f, "}}"),

			// keywords
			TokenType::Let      => write!(f, "let"),
			TokenType::Function => write!(f, "fn"),
			TokenType::Return   => write!(f, "return"),
			TokenType::Struct   => write!(f, "struct"),
			TokenType::Enum     => write!(f, "enum"),
			TokenType::Import   => write!(f, "import"),
			TokenType::True     => write!(f, "true"),
			TokenType::False    => write!(f, "false"),
			TokenType::And      => write!(f, "and"),
			TokenType::Or       => write!(f, "or"),
			TokenType::Not      => write!(f, "not"),
			TokenType::If       => write!(f, "if"),
			TokenType::Elif     => write!(f, "elif"),
			TokenType::Else     => write!(f, "else"),
			TokenType::Loop     => write!(f, "loop"),
			TokenType::Continue => write!(f, "coninue"),
			TokenType::Break    => write!(f, "break"),

			// primitive types
			TokenType::TypeBool   => write!(f, "bool"),
			TokenType::TypeInt32  => write!(f, "i32"),
			TokenType::TypeUint32 => write!(f, "u32"),
			TokenType::TypeFloat  => write!(f, "float"),
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
	/// use cactus::lexer::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
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
	/// use cactus::lexer::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
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
	/// use cactus::lexer::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
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

	/// Create a new instance of `Token` representing the end of the file.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::{Token, TokenType};
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
