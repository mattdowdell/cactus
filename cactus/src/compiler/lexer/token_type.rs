//! A representation of tokens generated from an input string and supporting structures.

use std::fmt;


/// A representation of a token type.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
	// specials
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
			TokenType::Illegal
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
