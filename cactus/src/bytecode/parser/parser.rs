//!
//!
//!

use std::iter::Peekable;

use crate::location::Location;
use crate::bytecode::error::{BytecodeError, ErrorType, ErrorCode};
use crate::bytecode::lexer::{Lexer, Token, TokenType};
use crate::bytecode::Instruction;
use super::ast::Module;

///
///
///
pub struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
	///
	///
	///
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

	///
	///
	///
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

	//
	//
	//
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
				unimplemented!()
			},

			// simple
			TokenType::Nop       => Ok(Instruction::Nop),
			TokenType::Halt      => Ok(Instruction::Halt),

			TokenType::Pop       => Ok(Instruction::Pop),
			TokenType::Dup       => Ok(Instruction::Dup),
			TokenType::Swap      => Ok(Instruction::Swap),
			TokenType::Movret    => Ok(Instruction::Movret),
			TokenType::Pushret   => Ok(Instruction::Pushret),
			TokenType::Alloca    => Ok(Instruction::Alloca),
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
