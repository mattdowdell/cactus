//!
//!
//!

use std::iter::Peekable;

use lexer::lexer::Lexer;
use lexer::token::{Token, TokenType};
use parser::ast::{Ast, Module, Definition, Identifier, Parameter, Type, Block, Statement, Expression};

struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
	errors: Vec<String>
}

impl<'a> Parser<'a> {
	///
	///
	///
	pub fn new(input: &'a str) -> Parser<'a> {
		Parser {
			lexer: Lexer::new(input).peekable(),
			errors: Vec::new(),
		}
	}

	///
	///
	///
	pub fn parse(&mut self) -> Module {
		self.parse_module()
	}

	// A helper method that checks if the next token is the given type and consumes it if so.
	// If the next token is not the given type an error will be returned instead.
	fn expect_peek(&mut self, token_type: TokenType) -> Result<(), String> {
		if self.lexer.peek().is_none() {
			Err(format!(
				"Unexpected end of file. Expected: {:?}",
				token_type,
			))
		} else {
			if self.lexer.peek().unwrap().token_type == token_type {
				self.lexer.next();
				Ok(())
			} else {
				Err(format!(
					"Unexpected token type: {:?}. Expected {:?}.",
					self.lexer.peek().unwrap().token_type,
					token_type,
				))
			}
		}
	}

	// Parse a Cactus module.
	//
	//
	fn parse_module(&mut self) -> Module {
		let mut module = Module::new();

		loop {
			if self.lexer.peek().is_none() {
				break;
			}

			match self.parse_definition() {
				Ok(definition) => {
					module.push(definition);
				}
				Err(error) => {
					self.errors.push(error);
				}
			}
		}

		module
	}

	// Parse a definition from within a Cactus module.
	fn parse_definition(&mut self) -> Result<Definition, String> {
		let token = self.lexer.next().unwrap();

		match token.token_type {
			TokenType::Import   => self.parse_import_definition(token),
			TokenType::Struct   => self.parse_struct_definition(token),
			TokenType::Enum     => self.parse_enum_definition(token),
			TokenType::Function => self.parse_function_definition(token),

			_ => Err(format!(
				"Unexpected token type: {:?}. Expected import, struct, enum or function",
				token.token_type
			)),
		}
	}

	// Parse an import definition.
	fn parse_import_definition(&mut self, token: Token) -> Result<Definition, String> {
		let identifier = self.parse_identifier()?;
		Ok(Definition::Import(identifier))
	}

	// Parse a structure definition.
	fn parse_struct_definition(&mut self, token: Token) -> Result<Definition, String> {
		let identifier = self.parse_identifier()?;

		self.expect_peek(TokenType::LeftBrace)?;

		let parameters = self.parse_parameter_list()?;

		self.expect_peek(TokenType::RightBrace)?;

		Ok(Definition::Struct(identifier, parameters))
	}

	// Parse an enumeration definition.
	fn parse_enum_definition(&mut self, token: Token) -> Result<Definition, String> {
		let identifier = self.parse_identifier()?;

		self.expect_peek(TokenType::LeftBrace)?;

		let variants = self.parse_identifier_list()?;

		self.expect_peek(TokenType::RightBrace)?;

		Ok(Definition::Enum(identifier, variants))
	}

	// Parse a function definition.
	fn parse_function_definition(&mut self, token: Token) -> Result<Definition, String> {
		let identifier = self.parse_identifier()?;
		let parameters = self.parse_parameter_list()?;
		let type_hint: Option<Type>;

		if self.expect_peek(TokenType::Arrow).is_ok() {
			type_hint = Some(self.parse_type()?);
		} else {
			type_hint = None;
		}

		let block = self.parse_block()?;

		Ok(Definition::Function(identifier, parameters, type_hint, block))
	}

	// A helper for parsing multiple identifiers in one go.
	fn parse_identifier_list(&mut self) -> Result<Vec<Identifier>, String> {
		let mut identifiers: Vec<Identifier> = Vec::new();

		loop {
			if self.lexer.peek().is_none() {
				break;
			}

			if self.lexer.peek().unwrap().token_type == TokenType::Identifier {
				let ident = self.parse_identifier()?;
				identifiers.push(ident);

				// identifier lists are delimited by commas
				// so if we have one then we can continue looking for more
				// if not then there's either no more to find or an error has occurred
				// so we stop now and let the potential error be handled by the calling function
				if self.expect_peek(TokenType::Comma).is_ok() {
					continue;
				} else {
					break;
				}
			} else {
				break;
			}
		}

		Ok(identifiers)
	}

	//
	//
	//
	fn parse_identifier(&mut self) -> Result<Identifier, String> {
		let token = self.lexer.next();

		if token.is_none() {
			Err("Unexpected end of file. Expected: identifier.".to_string())
		} else {
			let token = token.unwrap();

			if token.token_type != TokenType::Identifier {
				Err(format!(
					"Unexpected token type: {:?}. Expected: identifier.",
					token.token_type,
				))
			} else {
				Ok(Identifier::new(token.value.unwrap()))
			}
		}
	}

	// A helper for parsing multiple parameters in one go.
	fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, String> {
		let mut parameters: Vec<Parameter> = Vec::new();

		loop {
			if self.lexer.peek().is_none() {
				break;
			}

			if self.lexer.peek().unwrap().token_type == TokenType::Identifier {
				let param = self.parse_parameter()?;
				parameters.push(param);

				// parameters lists are delimited by commas
				// so if we have one then we can continue looking for more
				// if not then there's either no more to find or an error has occurred
				// so we stop now and let the potential error be handled by the calling function
				if self.expect_peek(TokenType::Comma).is_ok() {
					continue;
				} else {
					break;
				}
			} else {
				break;
			}
		}

		Ok(parameters)
	}

	//
	//
	//
	fn parse_parameter(&mut self) -> Result<Parameter, String> {
		let token = self.lexer.next();

		if token.is_none() {
			Err("Unexpected end of file. Expected: identifier.".to_string())
		} else {
			let token = token.unwrap();

			if token.token_type != TokenType::Identifier {
				Err(format!(
					"Unexpected token type: {:?}. Expected: identifier.",
					token.token_type,
				))
			} else {
				let ident = Identifier::new(token.value.unwrap());

				self.expect_peek(TokenType::Colon)?;

				let type_hint = self.parse_type()?;

				Ok(Parameter::new(ident, type_hint))
			}
		}
	}

	//
	//
	//
	fn parse_type(&mut self) -> Result<Type, String> {
		if self.lexer.peek().is_none() {
			Err("Unexpected end of file. Expected: type.".to_string())
		} else {
			match self.lexer.peek().unwrap().token_type {
				TokenType::Identifier => {
					let ident = self.parse_identifier()?;
					Ok(Type::from_identifier(ident))
				},
				_ => {
					let token = self.lexer.next().unwrap();
					let type_hint = Type::from_token(token)?;

					Ok(type_hint)
				},
			}
		}
	}

	//
	//
	//
	fn parse_block(&mut self) -> Result<Block, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_statement(&mut self) -> Result<Statement, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_let_statement(&mut self) -> Result<Statement, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_return_statement(&mut self) -> Result<Statement, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_if_statement(&mut self) -> Result<Statement, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_loop_statement(&mut self) -> Result<Statement, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_expr_statement(&mut self) -> Result<Statement, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_expression(&mut self) -> Result<Expression, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_literal_expression(&mut self) -> Result<Expression, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_identifier_expression(&mut self) -> Result<Expression, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_infix_expression(&mut self) -> Result<Expression, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_assignment_expression(&mut self) -> Result<Expression, String> {
		unimplemented!()
	}

	//
	//
	//
	fn parse_call_expression(&mut self) -> Result<Expression, String> {
		unimplemented!()
	}
}


#[cfg(test)]
mod test {
	use super::*;

	macro_rules! check_parser_errors {
		($parser:expr) => (
			if $parser.errors.len() > 0 {
				for error in $parser.errors.iter() {
					println!("{}", error);
				}

				panic!("Errors found during parsing");
			}
		)
	}

	macro_rules! ident {
		($value:tt) => (
			Identifier::new($value.to_string())
		)
	}

	macro_rules! param {
		($value:tt, $type_hint:expr) => (
			Parameter::new(ident!($value), $type_hint)
		)
	}

	#[test]
	fn test_parse_enum() {
		let mut parser = Parser::new("enum example { x, y, z }");
		let expected = Module {
			definitions: vec![
				Definition::Enum(
					ident!("example"),
					vec![
						ident!("x"),
						ident!("y"),
						ident!("z"),
					]
				),
			]
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_enum_with_trailing_comma() {
		let mut parser = Parser::new("enum example { x, y, z, }");
		let expected = Module {
			definitions: vec![
				Definition::Enum(
					ident!("example"),
					vec![
						ident!("x"),
						ident!("y"),
						ident!("z"),
					]
				),
			]
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_struct() {
		let mut parser = Parser::new("struct example { a: i32, b: u32, c: f32, d: bool }");
		let expected = Module {
			definitions: vec![
				Definition::Struct(
					ident!("example"),
					vec![
						param!("a", Type::Int32),
						param!("b", Type::Uint32),
						param!("c", Type::Float),
						param!("d", Type::Bool),
					]
				),
			]
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_struct_with_trailing_comma() {
		let mut parser = Parser::new("struct example { a: i32, b: u32, c: f32, d: bool }");
		let expected = Module {
			definitions: vec![
				Definition::Struct(
					ident!("example"),
					vec![
						param!("a", Type::Int32),
						param!("b", Type::Uint32),
						param!("c", Type::Float),
						param!("d", Type::Bool),
					]
				),
			]
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}
}
