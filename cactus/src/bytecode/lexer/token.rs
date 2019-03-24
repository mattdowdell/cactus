//! Token objects to be used when parsing Maude.

use std::fmt;

use crate::location::Location;
use super::token_type::TokenType;


/// A representation of a token and it's location relative to an input string.
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
	pub fn new_from_type(token_type: TokenType, location: Location) -> Token {
		Token {
			token_type: token_type,
			value: None,
			location: location,
		}
	}

	///
	///
	///
	pub fn new_with_value(token_type: TokenType, value: String, location: Location) -> Token {
		Token {
			token_type: token_type,
			value: Some(value),
			location: location,
		}
	}

	///
	///
	///
	pub fn new_from_ident(value: String, location: Location) -> Token {
		let tt = match value.as_ref() {
			"ARGS"      => TokenType::Args,
			"LOCALS"    => TokenType::Locals,
			"NOP"       => TokenType::Nop,
			"HALT"      => TokenType::Halt,
			"PUSH"      => TokenType::Push,
			"POP"       => TokenType::Pop,
			"DUP"       => TokenType::Dup,
			"SWAP"      => TokenType::Swap,
			"MOVRET"    => TokenType::Movret,
			"PUSHRET"   => TokenType::Pushret,
			"ALLOCA"    => TokenType::Alloca,
			"PUSHARG"   => TokenType::Pusharg,
			"DUMPSTACK" => TokenType::Dumpstack,
			"DUMPFRAME" => TokenType::Dumpframe,
			"OUT"       => TokenType::Out,
			"OUTLN"     => TokenType::Outln,
			"IN"        => TokenType::In,
			"STORE"     => TokenType::Store,
			"STOREIDX"  => TokenType::Storeidx,
			"LOAD"      => TokenType::Load,
			"LOADIDX"   => TokenType::Loadidx,
			"EQ"        => TokenType::Eq,
			"NEQ"       => TokenType::Neq,
			"LEQ"       => TokenType::Leq,
			"GEQ"       => TokenType::Geq,
			"LT"        => TokenType::Lt,
			"GT"        => TokenType::Gt,
			"COMPL"     => TokenType::Compl,
			"MINUS"     => TokenType::Minus,
			"ADD"       => TokenType::Add,
			"DIV"       => TokenType::Div,
			"REM"       => TokenType::Rem,
			"MUL"       => TokenType::Mul,
			"AND"       => TokenType::And,
			"OR"        => TokenType::Or,
			"NOT"       => TokenType::Not,
			"JMPNZ"     => TokenType::Jmpnz,
			"JMP"       => TokenType::Jmp,
			"SUBCALL"   => TokenType::Subcall,
			"RETURN"    => TokenType::Return,
			_ => TokenType::Ident,
		};

		if tt == TokenType::Ident {
			Token::new_with_value(tt, value, location)
		} else {
			Token::new_from_type(tt, location)
		}
	}

	//
	pub fn is_comment(&self) -> bool {
		match self.token_type {
			TokenType::InlineComment
			| TokenType::BlockComment => true,
			_ => false,
		}
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.token_type {
			TokenType::InlineComment => write!(f, "--{}", self.value.as_ref().unwrap()),
			TokenType::BlockComment => write!(f, "{{-{}-}}", self.value.as_ref().unwrap()),

			TokenType::Ident
			| TokenType::Integer
			| TokenType::Float => write!(f, "{}", self.value.as_ref().unwrap()),

			TokenType::String => write!(f, "\"{}\"", self.value.as_ref().unwrap()),

			TokenType::Colon
			| TokenType::Semicolon
			| TokenType::Ampersand
			| TokenType::Args
			| TokenType::Locals
			| TokenType::Nop
			| TokenType::Halt
			| TokenType::Push
			| TokenType::Pop
			| TokenType::Dup
			| TokenType::Swap
			| TokenType::Dumpstack
			| TokenType::Dumpframe
			| TokenType::Movret
			| TokenType::Pushret
			| TokenType::Alloca
			| TokenType::Pusharg
			| TokenType::Out
			| TokenType::Outln
			| TokenType::In
			| TokenType::Store
			| TokenType::Storeidx
			| TokenType::Load
			| TokenType::Loadidx
			| TokenType::Eq
			| TokenType::Neq
			| TokenType::Leq
			| TokenType::Geq
			| TokenType::Lt
			| TokenType::Gt
			| TokenType::Compl
			| TokenType::Minus
			| TokenType::Add
			| TokenType::Div
			| TokenType::Rem
			| TokenType::Mul
			| TokenType::And
			| TokenType::Or
			| TokenType::Not
			| TokenType::Jmpnz
			| TokenType::Jmp
			| TokenType::Subcall
			| TokenType::Return => write!(f, "{}", self.token_type),
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	const LOCATION: Location = Location {
		line: 0,
		column: 0,
	};

	// Test `Token::new_from_type` with appropriate inputs.
	#[test]
	fn test_new_from_type() {
		let inputs = vec![
			(TokenType::Colon, ":"),
			(TokenType::Semicolon, ";"),
			(TokenType::Ampersand, "&"),
		];

		for (input, display) in inputs.iter() {
			let tok = Token::new_from_type(*input, LOCATION);

			assert_eq!(&tok.token_type, input);
			assert_eq!(format!("{}", tok), display.to_string());
		}
	}

	// Test `Token::new_with_value` with appropriate inputs.
	#[test]
	fn test_new_with_value() {
		let inputs = vec![
			(TokenType::BlockComment, " comment ", "{- comment -}"),
			(TokenType::InlineComment, " comment", "-- comment"),
			(TokenType::String, "string", "\"string\""),
			(TokenType::Integer, "12345", "12345"),
			(TokenType::Float, "12.345", "12.345"),
		];

		for (tt, value, display) in inputs.iter() {
			let tok = Token::new_with_value(*tt, value.to_string(), LOCATION);

			assert_eq!(&tok.token_type, tt);
			assert_eq!(tok.value, Some(value.to_string()));
			assert_eq!(format!("{}", tok), display.to_string());
		}
	}

	// Test `Token::new_from_ident` with all instructions.
	#[test]
	fn test_new_from_ident_instructions() {
		let input_expected = vec![
			("ARGS", TokenType::Args),
			("LOCALS", TokenType::Locals),
			("NOP", TokenType::Nop),
			("HALT", TokenType::Halt),
			("PUSH", TokenType::Push),
			("POP", TokenType::Pop),
			("DUP", TokenType::Dup),
			("SWAP", TokenType::Swap),
			("DUMPSTACK", TokenType::Dumpstack),
			("DUMPFRAME", TokenType::Dumpframe),
			("MOVRET", TokenType::Movret),
			("PUSHRET", TokenType::Pushret),
			("ALLOCA", TokenType::Alloca),
			("PUSHARG", TokenType::Pusharg),
			("OUT", TokenType::Out),
			("OUTLN", TokenType::Outln),
			("IN", TokenType::In),
			("STORE", TokenType::Store),
			("STOREIDX", TokenType::Storeidx),
			("LOAD", TokenType::Load),
			("LOADIDX", TokenType::Loadidx),
			("EQ", TokenType::Eq),
			("NEQ", TokenType::Neq),
			("LEQ", TokenType::Leq),
			("GEQ", TokenType::Geq),
			("LT", TokenType::Lt),
			("GT", TokenType::Gt),
			("COMPL", TokenType::Compl),
			("MINUS", TokenType::Minus),
			("ADD", TokenType::Add),
			("DIV", TokenType::Div),
			("REM", TokenType::Rem),
			("MUL", TokenType::Mul),
			("AND", TokenType::And),
			("OR", TokenType::Or),
			("NOT", TokenType::Not),
			("JMPNZ", TokenType::Jmpnz),
			("JMP", TokenType::Jmp),
			("SUBCALL", TokenType::Subcall),
			("RETURN", TokenType::Return),
		];

		for (input, expected) in input_expected.iter() {
			let tok = Token::new_from_ident(input.to_string(), LOCATION);

			assert_eq!(&tok.token_type, expected);
			assert_eq!(format!("{}", tok), input.to_string());
		}
	}

	// Test `Token::new_from_ident` with identifiers.
	#[test]
	fn test_new_from_ident_identifier() {
		let inputs = vec!["test", "test123", "nop", "halt"];

		for input in inputs.iter() {
			let tok = Token::new_from_ident(input.to_string(), LOCATION);

			assert_eq!(tok.token_type, TokenType::Ident);
			assert_eq!(tok.value, Some(input.to_string()));
			assert_eq!(format!("{}", tok), input.to_string());
		}
	}
}
