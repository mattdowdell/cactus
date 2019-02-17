//!
//!
//!

use std::iter::Peekable;

use lexer::lexer::Lexer;
use lexer::token::TokenType;
use parser::ast::{
	Module,
	Definition,
	Identifier,
	Parameter,
	Type,
	Block,
	Statement,
	Expression,
	Operator,
	Literal,
	StructField,
};
use parser::precedence::Precedence;

///
///
///
pub struct Parser<'a> {
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

	//
	//
	//
	fn peek_precedence(&mut self) -> Precedence {
		if self.lexer.peek().is_some() {
			let peek = self.lexer.peek().unwrap();
			let precedence = Precedence::from_token(peek.clone());

			match precedence {
				Ok(value) => value,
				Err(_) => Precedence::Lowest,
			}
		} else {
			Precedence::Lowest
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
			TokenType::Import   => {
				let def = self.parse_import_definition()?;
				self.expect_peek(TokenType::Semicolon)?;
				Ok(def)
			},
			TokenType::Struct   => self.parse_struct_definition(),
			TokenType::Enum     => self.parse_enum_definition(),
			TokenType::Function => self.parse_function_definition(),

			_ => Err(format!(
				"Unexpected token type: {:?}. Expected import, struct, enum or function",
				token.token_type
			)),
		}
	}

	// Parse an import definition.
	fn parse_import_definition(&mut self) -> Result<Definition, String> {
		let identifier = self.parse_import_identifier()?;
		Ok(Definition::Import(identifier))
	}

	// Parse a structure definition.
	fn parse_struct_definition(&mut self) -> Result<Definition, String> {
		let identifier = self.parse_identifier()?;

		self.expect_peek(TokenType::LeftBrace)?;

		let parameters = self.parse_parameter_list()?;

		self.expect_peek(TokenType::RightBrace)?;

		Ok(Definition::Struct(identifier, parameters))
	}

	// Parse an enumeration definition.
	fn parse_enum_definition(&mut self) -> Result<Definition, String> {
		let identifier = self.parse_identifier()?;

		self.expect_peek(TokenType::LeftBrace)?;

		let variants = self.parse_identifier_list()?;

		self.expect_peek(TokenType::RightBrace)?;

		Ok(Definition::Enum(identifier, variants))
	}

	// Parse a function definition.
	fn parse_function_definition(&mut self) -> Result<Definition, String> {
		let identifier = self.parse_identifier()?;

		self.expect_peek(TokenType::LeftParen)?;

		let parameters = self.parse_parameter_list()?;

		self.expect_peek(TokenType::RightParen)?;

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
	fn parse_import_identifier(&mut self) -> Result<Identifier, String> {
		let mut path: Vec<Identifier> = Vec::new();

		loop {
			let ident = self.parse_identifier()?;
			path.push(ident);

			if self.expect_peek(TokenType::ImportJoin).is_err() {
				break;
			}

			if self.lexer.peek().is_some() {
				if self.lexer.peek().unwrap().token_type == TokenType::Identifier {
					continue;
				}
			}

			break;
		}

		let mut ident = path.pop().unwrap();

		if path.len() > 0 {
			ident.set_path(path);
		}

		Ok(ident)
	}

	// Parse an identifier.
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
					let ident = self.parse_import_identifier()?;
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
		let mut block = Block::new();

		self.expect_peek(TokenType::LeftBrace)?;

		loop {
			if self.expect_peek(TokenType::RightBrace).is_ok() {
				break;
			}

			let stmt = self.parse_statement()?;
			block.push(stmt);
		}

		Ok(block)
	}

	//
	//
	//
	fn parse_statement(&mut self) -> Result<Statement, String> {
		if self.lexer.peek().is_none() {
			Err("Unexpected end of file. Expected: statement".to_string())
		} else {
			let stmt = match self.lexer.peek().unwrap().token_type {
				TokenType::Let      => self.parse_let_statement()?,
				TokenType::Return   => self.parse_return_statement()?,
				TokenType::If       => self.parse_if_statement()?,
				TokenType::Loop     => self.parse_loop_statement()?,
				TokenType::Break    => self.parse_break_statement()?,
				TokenType::Continue => self.parse_continue_statement()?,
				_                   => self.parse_expr_statement()?,
			};

			Ok(stmt)
		}
	}

	//
	//
	//
	fn parse_let_statement(&mut self) -> Result<Statement, String> {
		self.expect_peek(TokenType::Let)?;

		let ident = self.parse_identifier()?;

		self.expect_peek(TokenType::Colon)?;

		let type_hint = self.parse_type()?;

		self.expect_peek(TokenType::Assign)?;

		let expr = self.parse_expression(Precedence::Lowest)?;

		Ok(Statement::Let(ident, type_hint, expr))
	}

	//
	//
	//
	fn parse_return_statement(&mut self) -> Result<Statement, String> {
		self.expect_peek(TokenType::Return)?;

		let expr = self.parse_expression(Precedence::Lowest)?;

		Ok(Statement::Return(expr))
	}

	//
	//
	//
	fn parse_if_statement(&mut self) -> Result<Statement, String> {
		self.expect_peek(TokenType::If)?;

		let condition = self.parse_expression(Precedence::Lowest)?;
		let consequence = self.parse_block()?;
		let mut more: Vec<(Expression, Block)> = Vec::new();
		let alternative: Option<Block>;

		loop {
			if self.expect_peek(TokenType::Elif).is_err() {
				break;
			}

			let expr = self.parse_expression(Precedence::Lowest)?;
			let block = self.parse_block()?;

			more.push((expr, block));
		}

		if self.expect_peek(TokenType::Else).is_ok() {
			alternative = Some(self.parse_block()?);
		} else {
			alternative = None;
		}

		Ok(Statement::If(condition, consequence, more, alternative))
	}

	//
	//
	//
	fn parse_loop_statement(&mut self) -> Result<Statement, String> {
		self.expect_peek(TokenType::Loop)?;

		let block = self.parse_block()?;

		Ok(Statement::Loop(block))
	}

	//
	//
	//
	fn parse_break_statement(&mut self) -> Result<Statement, String> {
		self.expect_peek(TokenType::Break)?;
		Ok(Statement::Break)
	}

	//
	//
	//
	fn parse_continue_statement(&mut self) -> Result<Statement, String> {
		self.expect_peek(TokenType::Continue)?;
		Ok(Statement::Continue)
	}

	//
	//
	//
	fn parse_expr_statement(&mut self) -> Result<Statement, String> {
		let expr = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::Semicolon)?;

		Ok(Statement::Expression(expr))
	}

	//
	//
	//
	fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
		if self.lexer.peek().is_none() {
			Err("Unexpected end of file. Expected: expression".to_string())
		} else {
			let mut left = match self.lexer.peek().unwrap().token_type {
				// identifier
				TokenType::Identifier => self.parse_identifier_expression()?,

				// literals
				TokenType::Integer
				| TokenType::Float
				| TokenType::True
				| TokenType::False => self.parse_literal_expression()?,

				// prefix expressions
				TokenType::Not
				| TokenType::Minus
				| TokenType::BitCompl => self.parse_prefix_expression()?,

				// grouped expressions
				TokenType::LeftParen => self.parse_grouped_expression()?,

				// struct initialisation
				TokenType::LeftBrace => self.parse_struct_expression()?,

				_ => {
					let token = self.lexer.next().unwrap();

					return Err(format!(
						"Unexpected token type: {:?}. Expected one of: Identifier, Integer, Float, 'true', 'false', 'not', '-', '~', '(' or '{{'.",
						token.token_type
					));
				}
			};

			loop {
				if self.lexer.peek().is_none() {
					break;
				}

				if
					self.lexer.peek().unwrap().token_type != TokenType::Semicolon &&
					precedence < self.peek_precedence()
				{
					left = match self.lexer.peek().unwrap().token_type {
						TokenType::LeftParen => self.parse_call_expression(left)?,
						_ => self.parse_infix_expression(left)?,
					};

					continue;
				}

				break;
			}

			Ok(left)
		}
	}

	//
	//
	//
	fn parse_literal_expression(&mut self) -> Result<Expression, String> {
		let token = self.lexer.next().unwrap();
		let literal = Literal::from_token(token)?;
		Ok(Expression::Literal(literal))
	}

	//
	//
	//
	fn parse_identifier_expression(&mut self) -> Result<Expression, String> {
		let ident = self.parse_import_identifier()?;
		Ok(Expression::Identifier(ident))
	}

	//
	//
	//
	fn parse_struct_expression(&mut self) -> Result<Expression, String> {
		self.expect_peek(TokenType::LeftBrace)?;
		let mut struct_fields: Vec<StructField> = Vec::new();

		loop {
			if self.expect_peek(TokenType::RightBrace).is_ok() {
				break;
			}

			let ident = self.parse_identifier()?;
			self.expect_peek(TokenType::Colon)?;
			let expr = self.parse_expression(Precedence::Lowest)?;

			let field = StructField::new(ident, expr);
			struct_fields.push(field);

			if self.expect_peek(TokenType::Comma).is_ok() {
				continue;
			}
		}

		Ok(Expression::Struct(struct_fields))
	}

	//
	//
	//
	fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
		let token = self.lexer.next().unwrap();
		let operator = Operator::new_prefix(token)?;
		let expr = self.parse_expression(Precedence::Prefix)?;

		Ok(Expression::Prefix(operator, Box::new(expr)))
	}

	//
	//
	//
	fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
		let token = self.lexer.next().unwrap();
		let operator = Operator::new_infix(token)?;


		if self.lexer.peek().is_none() {
			return Err("Unexpected end of file. Expected: expression.".to_string());
		} else {
			let precedence = self.peek_precedence();

			let right = self.parse_expression(precedence)?;
			Ok(Expression::Infix(Box::new(left), operator, Box::new(right)))
		}
	}

	//
	//
	//
	fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
		let expr = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::RightParen)?;

		Ok(expr)
	}

	//
	//
	//
	fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, String> {
		let mut args: Vec<Expression> = Vec::new();

		self.expect_peek(TokenType::LeftParen)?;

		loop {
			if self.expect_peek(TokenType::RightParen).is_ok() {
				break;
			}

			let argument = self.parse_expression(Precedence::Lowest)?;
			args.push(argument);

			if self.expect_peek(TokenType::Comma).is_ok() {
				continue;
			}

			if self.lexer.peek().is_none() {
				return Err("Unexpected end of file. Expected one of: ')', ','.".to_string());
			} else {
				if self.lexer.peek().unwrap().token_type == TokenType::RightParen {
					continue;
				}

				let token = self.lexer.next().unwrap();

				return Err(format!(
					"Unexpected token type: {:?}. Expected one of: ')', ','.",
					token.token_type,
				));
			}
		}

		match left {
			Expression::Identifier(ident) => Ok(Expression::Call(ident, args)),
			_ => Err(format!(
				"Unexpected expression: {:?}. Expected: identifier.",
				left
			)),
		}
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

	macro_rules! field {
		($name:tt, $value:expr) => {
			StructField::new(ident!($name), $value)
		}
	}

	macro_rules! expr_literal_int {
		($value:tt) => (
			Expression::Literal(
				Literal::Integer($value.to_string())
			)
		)
	}

	macro_rules! expr_literal_float {
		($value:tt) => (
			Expression::Literal(
				Literal::Float($value.to_string())
			)
		)
	}

	macro_rules! expr_literal_true {
		() => (
			Expression::Literal(
				Literal::Boolean(true)
			)
		)
	}

	macro_rules! expr_literal_false {
		() => (
			Expression::Literal(
				Literal::Boolean(false)
			)
		)
	}

	fn ident_with_path(value: &str, path: Vec<Identifier>) -> Identifier {
		let mut ident = ident!(value);
		ident.set_path(path);
		ident
	}


	#[test]
	fn test_parse_import_module() {
		let mut parser = Parser::new("import std;");
		let expected = Module {
			definitions: vec![
				Definition::Import(
					ident!("std")
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_import_from_module() {
		let mut parser = Parser::new("import std::example;");
		let expected = Module {
			definitions: vec![
				Definition::Import(
					ident_with_path("example", vec![ident!("std")])
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
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

	#[test]
	fn test_parse_struct_custom_type() {
		let mut parser = Parser::new("struct example { a: Test }");
		let expected = Module {
			definitions: vec![
				Definition::Struct(
					ident!("example"),
					vec![
						param!(
							"a",
							Type::Custom(
								ident!("Test")
							)
						),
					]
				),
			]
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_struct_custom_imported_type() {
		let mut parser = Parser::new("struct example { a: std::Test }");
		let expected = Module {
			definitions: vec![
				Definition::Struct(
					ident!("example"),
					vec![
						param!(
							"a",
							Type::Custom(
								ident_with_path(
									"Test",
									vec![
										ident!("std")
									]
								)
							)
						),
					]
				),
			]
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty() {
		let mut parser = Parser::new("fn x() {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![],
					None,
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	#[should_panic]
	fn test_parse_function_empty_invalid_identifier() {
		let mut parser = Parser::new("fn a::b() {}");

		parser.parse();
		check_parser_errors!(parser);
	}

	#[test]
	fn test_parse_function_empty_with_type_hint() {
		let mut parser = Parser::new("fn x() -> i32 {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![],
					Some(Type::Int32),
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty_with_custom_type_hint() {
		let mut parser = Parser::new("fn x() -> Test {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![],
					Some(
						Type::Custom(
							ident!("Test")
						)
					),
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty_with_custom_imported_type_hint() {
		let mut parser = Parser::new("fn x() -> std::Test {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![],
					Some(
						Type::Custom(
							ident_with_path(
								"Test",
								vec![
									ident!("std")
								]
							)
						)
					),
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty_with_single_param() {
		let mut parser = Parser::new("fn x(a: i32) {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![
						param!("a", Type::Int32),
					],
					None,
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	#[should_panic]
	fn test_parse_function_empty_with_single_param_invalid_identifier() {
		let mut parser = Parser::new("fn x(a::b: i32) {}");

		parser.parse();
		check_parser_errors!(parser);
	}

	#[test]
	fn test_parse_function_empty_with_single_param_and_type_hint() {
		let mut parser = Parser::new("fn x(a: i32) -> i32 {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![
						param!("a", Type::Int32),
					],
					Some(Type::Int32),
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty_with_single_param_trailing_comma() {
		let mut parser = Parser::new("fn x(a: i32,) {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![
						param!("a", Type::Int32),
					],
					None,
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty_with_multiple_params() {
		let mut parser = Parser::new("fn x(a: i32, b: u32, c: f32, d: bool) {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![
						param!("a", Type::Int32),
						param!("b", Type::Uint32),
						param!("c", Type::Float),
						param!("d", Type::Bool),
					],
					None,
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty_with_multiple_params_and_type_hint() {
		let mut parser = Parser::new("fn x(a: i32, b: u32, c: f32, d: bool) -> i32 {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![
						param!("a", Type::Int32),
						param!("b", Type::Uint32),
						param!("c", Type::Float),
						param!("d", Type::Bool),
					],
					Some(Type::Int32),
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_function_empty_with_multiple_params_trailing_comma() {
		let mut parser = Parser::new("fn x(a: i32, b: u32, c: f32, d: bool,) {}");
		let expected = Module {
			definitions: vec![
				Definition::Function(
					ident!("x"),
					vec![
						param!("a", Type::Int32),
						param!("b", Type::Uint32),
						param!("c", Type::Float),
						param!("d", Type::Bool),
					],
					None,
					Block {
						statements: vec![],
					},
				)
			],
		};

		let module = parser.parse();

		check_parser_errors!(parser);
		assert_eq!(module, expected);
	}

	#[test]
	fn test_parse_expression_literal() {
		let mut parser = Parser::new("true; false; 5; 5.0;");
		let expected = vec![
			Statement::Expression(
				expr_literal_true!(),
			),
			Statement::Expression(
				expr_literal_false!(),
			),
			Statement::Expression(
				expr_literal_int!("5"),
			),
			Statement::Expression(
				expr_literal_float!("5.0"),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_identifier() {
		let mut parser = Parser::new("foo; bar; baz;");
		let expected = vec![
			Statement::Expression(
				Expression::Identifier(
					ident!("foo"),
				),
			),
			Statement::Expression(
				Expression::Identifier(
					ident!("bar"),
				),
			),
			Statement::Expression(
				Expression::Identifier(
					ident!("baz"),
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_struct() {
		let mut parser = Parser::new("{ a: 5, b: 10 };");
		let expected = vec![
			Statement::Expression(
				Expression::Struct(
					vec![
						field!("a", expr_literal_int!("5")),
						field!("b", expr_literal_int!("10")),
					]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_struct_trailing_comma() {
		let mut parser = Parser::new("{ a: 5, b: 10, };");
		let expected = vec![
			Statement::Expression(
				Expression::Struct(
					vec![
						field!("a", expr_literal_int!("5")),
						field!("b", expr_literal_int!("10")),
					]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_call() {
		let mut parser = Parser::new("example();");
		let expected = vec![
			Statement::Expression(
				Expression::Call(
					ident!("example"),
					vec![]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_call_imported() {
		let mut parser = Parser::new("std::example();");
		let expected = vec![
			Statement::Expression(
				Expression::Call(
					ident_with_path("example", vec![ident!("std")]),
					vec![]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_call_with_single_argument() {
		let mut parser = Parser::new("example(5);");
		let expected = vec![
			Statement::Expression(
				Expression::Call(
					ident!("example"),
					vec![
						expr_literal_int!("5"),
					]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_call_with_single_argument_trailing_comma() {
		let mut parser = Parser::new("example(5,);");
		let expected = vec![
			Statement::Expression(
				Expression::Call(
					ident!("example"),
					vec![
						expr_literal_int!("5"),
					]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_call_with_multiple_arguments() {
		let mut parser = Parser::new("example(5, x);");
		let expected = vec![
			Statement::Expression(
				Expression::Call(
					ident!("example"),
					vec![
						expr_literal_int!("5"),
						Expression::Identifier(ident!("x")),
					]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_call_with_multiple_arguments_trailing_comma() {
		let mut parser = Parser::new("example(5, x,);");
		let expected = vec![
			Statement::Expression(
				Expression::Call(
					ident!("example"),
					vec![
						expr_literal_int!("5"),
						Expression::Identifier(ident!("x")),
					]
				),
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_prefix() {
		let mut parser = Parser::new("-5; not x; ~10;");
		let expected = vec![
			Statement::Expression(
				Expression::Prefix(
					Operator::UnaryMinus,
					Box::new(expr_literal_int!("5")),
				)
			),
			Statement::Expression(
				Expression::Prefix(
					Operator::Not,
					Box::new(Expression::Identifier(ident!("x"))),
				)
			),
			Statement::Expression(
				Expression::Prefix(
					Operator::BitCompl,
					Box::new(expr_literal_int!("10")),
				)
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_infix_basic() {
		let mut parser = Parser::new("1 + 1; 1 - 1; 1 * 1; 1 / 1; 1 % 1;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::Plus,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::Minus,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::Multiply,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::Divide,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::Modulo,
					Box::new(expr_literal_int!("1")),
				)
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_infix_bitwise() {
		let mut parser = Parser::new("1 & 1; 1 | 1; 1 ^ 1; 1 << 1; 1 >> 1;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::BitAnd,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::BitOr,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::BitXor,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::BitLeftShift,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::BitRightShift,
					Box::new(expr_literal_int!("1")),
				)
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_infix_boolean() {
		let mut parser = Parser::new("1 == 1; 1 != 1; 1 < 1; 1 <= 1; 1 > 1; 1 >= 1;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::Equal,
					Box::new(expr_literal_int!("1")),
				)),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::NotEqual,
					Box::new(expr_literal_int!("1")),
				)),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::LessThan,
					Box::new(expr_literal_int!("1")),
				)),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::LessThanOrEqual,
					Box::new(expr_literal_int!("1")),
				)),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::GreaterThan,
					Box::new(expr_literal_int!("1")),
				)),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::GreaterThanOrEqual,
					Box::new(expr_literal_int!("1")),
				)
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_infix_boolean_words() {
		let mut parser = Parser::new("true and true; true or true;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_true!()),
					Operator::And,
					Box::new(expr_literal_true!()),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_true!()),
					Operator::Or,
					Box::new(expr_literal_true!()),
				)
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_infix_basic_assignment() {
		let mut parser = Parser::new("x = 1; x += 1; x -= 1; x *= 1; x /= 1; x %= 1;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::Assign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::PlusAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::MinusAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::MultiplyAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::DivideAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::ModuloAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_expression_infix_bitwise_assignment() {
		let mut parser = Parser::new("x &= 1; x |= 1; x ^= 1; x <<= 1; x >>= 1;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::BitAndAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::BitOrAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::BitXorAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::BitLeftShiftAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Identifier(ident!("x"))),
					Operator::BitRightShiftAssign,
					Box::new(expr_literal_int!("1")),
				)
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_expr_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}
}
