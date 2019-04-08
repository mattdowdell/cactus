//! The bytecode parser.

use std::iter::Peekable;

use crate::location::Location;
use crate::bytecode::error::{BytecodeError, ErrorType, ErrorCode};
use crate::bytecode::lexer::{Lexer, Token, TokenType};
use super::ast::{Module, Instruction, Literal, Symbol};

/// A representation of the parser.
pub struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
	/// Create a new instance of `Parser`.
	pub fn new(input: &str) -> Parser {
		Parser {
			lexer: Lexer::new(input).peekable(),
		}
	}

	// A helper method that checks if the next token is the given type and consumes it if so.
	// If the next token is not the given type an error will be returned instead.
	fn expect_peek(&mut self, token_type: TokenType) -> Result<Token, BytecodeError> {
		if self.lexer.peek().is_none() {
			Err(BytecodeError::new(ErrorType::SyntaxError,
				ErrorCode::E0006,
				Location::end(),
				format!("Unexpected end of file. Expected {} ({:?})",
					token_type,
					token_type)))
		} else {
			match self.lexer.peek().clone().unwrap() {
				Ok(token) => {
					if token.token_type == token_type {
						Ok(self.lexer.next().unwrap().unwrap())
					} else {
						let token = self.lexer.next().unwrap().unwrap();

						Err(BytecodeError::new(ErrorType::SyntaxError,
							ErrorCode::E0007,
							token.location,
							format!("Unexpected token: {} ({:?}). Expected: {} ({:?})",
								token, token.token_type,
								token_type, token_type)))
					}
				},
				Err(error) => {
					Err(error.clone())
				}
			}
		}
	}

	/// Parse the previously given input into an AST.
	pub fn parse(&mut self) -> Result<Module, Vec<BytecodeError>> {
		let mut module = Module::new();
		let mut errors = Vec::new();

		loop {
			match self.lexer.next() {
				Some(result) => {
					match result {
						Ok(token) => {
							if !token.is_comment() {
								match self.parse_statement(token) {
									Ok(instruction) => module.push_instruction(instruction),
									Err(error) => errors.push(error),
								}
							}
						},
						Err(error) => errors.push(error),
					}
				}
				None => break,
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(module)
		}
	}

	// Parse a bytecode statement.
	//
	// this is usually an instruction followed by a semicolon, but also includes label definitions.
	fn parse_statement(&mut self, token: Token) -> Result<Instruction, BytecodeError> {
		let instr = match token.token_type {

			TokenType::Ampersand => {
				let ident_tok = self.expect_peek(TokenType::Ident)?;
				Ok(Instruction::Pushaddr(ident_tok.value.unwrap().clone()))
			},

			TokenType::Ident => {
				self.expect_peek(TokenType::Colon)?;
				Ok(Instruction::Labeldef(token.value.unwrap().clone()))
			},

			TokenType::Push => {
				let next = self.lexer.next();

				if next.is_none() {
					Err(BytecodeError::new(ErrorType::SyntaxError,
						ErrorCode::E0006,
						Location::end(),
						"Unexpected end of file. Expected <literal>".to_string()))
				} else {
					let next = next.unwrap()?;
					let literal = match next.token_type {
						TokenType::Integer => {
							Literal::Integer(next.value.unwrap().clone())
						},
						TokenType::Float   => {
							Literal::Float(next.value.unwrap().clone())
						},
						TokenType::String  => {
							Literal::String(next.value.unwrap().clone())
						},
						TokenType::Args    => {
							Literal::Symbol(Symbol::Args)
						},
						TokenType::Locals  => {
							Literal::Symbol(Symbol::Locals)
						},
						_ => {
							return Err(BytecodeError::new(ErrorType::SyntaxError,
								ErrorCode::E0007,
								next.location,
								format!("Unexpected token: {} ({:?}). Expected: <literal>",
									next, next.token_type)));
						}
					};

					Ok(Instruction::Push(literal))
				}
			},

			// simple
			TokenType::Nop       => Ok(Instruction::Nop),
			TokenType::Halt      => Ok(Instruction::Halt),

			TokenType::Pop       => Ok(Instruction::Pop),
			TokenType::Dup       => Ok(Instruction::Dup),
			TokenType::Swap      => Ok(Instruction::Swap),
			TokenType::Movret    => Ok(Instruction::Movret),
			TokenType::Pushret   => Ok(Instruction::Pushret),
			TokenType::Pusharg   => Ok(Instruction::Pusharg),
			TokenType::Dumpstack => Ok(Instruction::Dumpstack),
			TokenType::Dumpframe => Ok(Instruction::Dumpframe),
			TokenType::Out       => Ok(Instruction::Out),
			TokenType::Outln     => Ok(Instruction::Outln),
			TokenType::In        => Ok(Instruction::In),
			TokenType::Store     => Ok(Instruction::Store),
			TokenType::Storeidx  => Ok(Instruction::Storeidx),
			TokenType::Load      => Ok(Instruction::Load),
			TokenType::Loadidx   => Ok(Instruction::Loadidx),
			TokenType::Eq        => Ok(Instruction::Eq),
			TokenType::Neq       => Ok(Instruction::Neq),
			TokenType::Leq       => Ok(Instruction::Leq),
			TokenType::Geq       => Ok(Instruction::Geq),
			TokenType::Lt        => Ok(Instruction::Lt),
			TokenType::Gt        => Ok(Instruction::Gt),
			TokenType::Compl     => Ok(Instruction::Compl),
			TokenType::Minus     => Ok(Instruction::Minus),
			TokenType::Add       => Ok(Instruction::Add),
			TokenType::Div       => Ok(Instruction::Div),
			TokenType::Rem       => Ok(Instruction::Rem),
			TokenType::Mul       => Ok(Instruction::Mul),
			TokenType::And       => Ok(Instruction::And),
			TokenType::Or        => Ok(Instruction::Or),
			TokenType::Not       => Ok(Instruction::Not),
			TokenType::Jmpnz     => Ok(Instruction::Jmpnz),
			TokenType::Jmp       => Ok(Instruction::Jmp),
			TokenType::Subcall   => Ok(Instruction::Subcall),
			TokenType::Return    => Ok(Instruction::Return),

			_ => Err(BytecodeError::new(ErrorType::SyntaxError,
				ErrorCode::E0007,
				token.location,
				format!("Unexpected token: {}",
					token.token_type)))
		}?;

		match instr {
			Instruction::Labeldef(_) => {},
			_ => {
				self.expect_peek(TokenType::Semicolon)?;
			}
		};

		Ok(instr)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_ignore_inline_comment() {
		let input = "main:\nNOP;\n-- comment\n";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_ok());
	}

	#[test]
	fn test_ignore_block_comment() {
		let input = "main:\nNOP;\n{- comment -}\n";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_ok());
	}

	#[test]
	fn test_unexpected_non_instruction() {
		let input = "main:\n;";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_err());
	}

	#[test]
	fn test_missing_ident_after_ampersand() {
		let input = "main:\n&;";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_err());
	}

	#[test]
	fn test_missing_literal_after_push() {
		let input = "main:\nPUSH;";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_err());
	}

	#[test]
	fn test_label_missing_colon() {
		let input = "main";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_err());
	}

	#[test]
	fn test_instruction_missing_semicolon() {
		let input = "main:NOP";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_err());
	}

	#[test]
	fn test_unexpected_token() {
		let input = "main:;";
		let mut parser = Parser::new(input);

		assert!(parser.parse().is_err());
	}
}
