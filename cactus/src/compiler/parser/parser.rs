//! The parser for Cactus.
//!
//! The parser is designed to mirror the grammar for Cactus. Starting from the definitions, it
//! then parses statements and then expressions where appropriate. Each definition, statement and
//! expressions has it's own function within the parser and is responsible for consuming all of the
//! tokens required for the grammar rule.

use std::iter::Peekable;

use crate::error::{CompilationError, ErrorCode, syntax_error};
use crate::location::Location;
use crate::compiler::lexer::{Lexer, Token, TokenType};

use super::ast::*;
use super::precedence::Precedence;


/// The parser implementation.
pub struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
	errors: Vec<CompilationError>,
	block_id: usize,
}

impl<'a> Parser<'a> {
	/// Create a new instance of a `Parser`.
	pub fn new(input: &'a str) -> Parser<'a> {
		Parser {
			lexer: Lexer::new(input).peekable(),
			errors: Vec::new(),
			block_id: 0,
		}
	}

	/// Parse the input into an Abstract Syntax Tree (AST).
	pub fn parse(&mut self) -> Result<Ast, Vec<CompilationError>> {
		let module = self.parse_module();

		if self.errors.len() > 0 {
			Err(self.errors.clone())
		} else {
			let mut ast = Ast::new();
			ast.push_module(module);

			Ok(ast)
		}


	}

	//
	//
	//
	fn next_block_id(&mut self) -> usize {
		let ret = self.block_id;
		self.block_id += 1;
		ret
	}

	// A helper method that checks if the next token is the given type and consumes it if so.
	// If the next token is not the given type an error will be returned instead.
	fn expect_peek(&mut self, token_type: TokenType) -> Result<Token, CompilationError> {
		if self.lexer.peek().is_none() {
			Err(syntax_error(ErrorCode::E0001,
				Location::end(),
				format!("Unexpected end of file. Expected {} ({:?})",
					token_type,
					token_type)))
		} else {
			if self.lexer.peek().unwrap().token_type == token_type {
				Ok(self.lexer.next().unwrap())
			} else {
				let token = self.lexer.peek().unwrap().clone();
				Err(syntax_error(ErrorCode::E0002,
					token.location,
					format!("Unexpected token: {} ({:?}). Expected: {} ({:?})",
						token, token,
						token_type, token_type)))
			}
		}
	}

	// Get the precedence of the next token.
	//
	// If there is no more tokens available or if there is no precedence defined for the token, then
	// `Precedence::Lowest` will be returned.
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
	fn parse_module(&mut self) -> Module {
		let mut module = Module::new();

		loop {
			if self.lexer.peek().is_none() {
				break;
			}

			match self.parse_definition() {
				Ok(definition) => {
					module.push_definition(definition);
				}
				Err(error) => {
					self.errors.push(error);
				}
			}
		}

		module
	}

	// Parse a definition from within a Cactus module.
	fn parse_definition(&mut self) -> Result<Definition, CompilationError> {
		let token = self.lexer.peek().unwrap().clone();

		match token.token_type {
			TokenType::Function => self.parse_function_definition(),

			_ => {
				let token = self.lexer.next().unwrap();

				Err(syntax_error(ErrorCode::E0002,
					token.location,
					format!("Unexpected token: {} ({:?}). Expected one of: {}.",
						token, token,
						TokenType::Function)))
			}
		}
	}

	// Parse a function definition.
	fn parse_function_definition(&mut self) -> Result<Definition, CompilationError> {
		let start = self.expect_peek(TokenType::Function)?;
		let identifier = self.parse_identifier()?;

		self.expect_peek(TokenType::LeftParen)?;

		let arguments = self.parse_argument_list()?;

		self.expect_peek(TokenType::RightParen)?;

		let return_type: Option<TypeHint>;

		if self.expect_peek(TokenType::Arrow).is_ok() {
			return_type = Some(self.parse_type()?);
		} else {
			return_type = None;
		}

		let body = self.parse_block()?;

		let function = Function::new(identifier, arguments, return_type, body, start.location);
		Ok(Definition::Function(function))
	}

	// Parse an identifier, e.g. `example`.
	fn parse_identifier(&mut self) -> Result<Identifier, CompilationError> {
		let token = self.expect_peek(TokenType::Identifier)?;
		Ok(Identifier::new(token.value.unwrap(), token.location))
	}

	// A helper for parsing multiple arguments in one go.
	fn parse_argument_list(&mut self) -> Result<Vec<Argument>, CompilationError> {
		let mut arguments: Vec<Argument> = Vec::new();

		loop {
			if self.lexer.peek().is_none() {
				break;
			}

			if self.lexer.peek().unwrap().token_type == TokenType::Identifier {
				let arg = self.parse_argument()?;
				arguments.push(arg);

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

		Ok(arguments)
	}

	// Parse an argument.
	//
	// Arguments consist of an identifier and a type separated by a colon.
	fn parse_argument(&mut self) -> Result<Argument, CompilationError> {
		let ident = self.parse_identifier()?;

		self.expect_peek(TokenType::Colon)?;

		let type_hint = self.parse_type()?;

		Ok(Argument::new(ident, type_hint))
	}

	// Parse a type.
	//
	// Types may be a primitive or an imported identifier in which case the type must be for a
	// structure or enumeration.
	fn parse_type(&mut self) -> Result<TypeHint, CompilationError> {
		if self.lexer.peek().is_none() {
			Err(syntax_error(ErrorCode::E0001,
				Location::end(),
				format!("Unexpected end of file. Expected one of: {}, {} or {}",
					TokenType::TypeInt32,
					TokenType::Float,
					TokenType::TypeBool)))
		} else {
			let token = self.lexer.next().unwrap();
			let type_hint = TypeHint::new_from_token(token)?;

			Ok(type_hint)
		}
	}

	// Parse a block.
	//
	// A block is a series of statements wrapped in braces, e.g. `{ ... }`.
	fn parse_block(&mut self) -> Result<Block, CompilationError> {
		let start = self.expect_peek(TokenType::LeftBrace)?;
		let block_id = self.next_block_id();
		let mut block = Block::new(block_id, start.location);

		loop {
			if self.expect_peek(TokenType::RightBrace).is_ok() {
				break;
			}

			let stmt = self.parse_statement()?;
			block.push_statement(stmt);
		}

		Ok(block)
	}

	// Parse a statement.
	fn parse_statement(&mut self) -> Result<Statement, CompilationError> {
		if self.lexer.peek().is_none() {
			Err(syntax_error(ErrorCode::E0001,
				Location::end(),
				"Unexpected end of file. Expected <statement>".to_string()))
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

	// Parse a let statement.
	fn parse_let_statement(&mut self) -> Result<Statement, CompilationError> {
		let start = self.expect_peek(TokenType::Let)?;

		let ident = self.parse_identifier()?;

		self.expect_peek(TokenType::Colon)?;

		let type_hint = self.parse_type()?;

		self.expect_peek(TokenType::Assign)?;

		let expr = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::Semicolon)?;

		let let_stmt = Let::new(ident, type_hint, expr, start.location);
		Ok(Statement::Let(let_stmt))
	}

	// Parse a return statement.
	fn parse_return_statement(&mut self) -> Result<Statement, CompilationError> {
		self.expect_peek(TokenType::Return)?;

		let expr = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::Semicolon)?;

		Ok(Statement::Return(expr))
	}

	// Parse an if statement.
	//
	// If statements consist of an initial condition expression and consequence block. They may
	// also contain multiple other conditions and consequences, e.g. `elif` and an optional
	// alternative block, e.g. `else`.
	fn parse_if_statement(&mut self) -> Result<Statement, CompilationError> {
		let start = self.expect_peek(TokenType::If)?;
		let mut branches: Vec<Branch> = Vec::new();

		loop {
			let condition = self.parse_expression(Precedence::Lowest)?;
			let consequence = self.parse_block()?;

			let branch = Branch::new(condition, consequence);
			branches.push(branch);

			if self.expect_peek(TokenType::Elif).is_ok() {
				continue;
			}

			if self.expect_peek(TokenType::Else).is_ok() {
				let literal_type = LiteralType::True;
				let literal = Literal::new(TypeHint::Bool, literal_type, Location::end());

				let condition = Expression::Literal(literal);
				let consequence = self.parse_block()?;

				let branch = Branch::new(condition, consequence);
				branches.push(branch);
			}

			break;
		}

		let if_stmt = If::new(branches, start.location);
		Ok(Statement::If(if_stmt))
	}

	// Parse a loop statement.
	fn parse_loop_statement(&mut self) -> Result<Statement, CompilationError> {
		let start = self.expect_peek(TokenType::Loop)?;
		let body = self.parse_block()?;

		let loop_stmt = Loop::new(body, start.location);
		Ok(Statement::Loop(loop_stmt))
	}

	// Parse a break statement.
	fn parse_break_statement(&mut self) -> Result<Statement, CompilationError> {
		let start = self.expect_peek(TokenType::Break)?;
		self.expect_peek(TokenType::Semicolon)?;

		let ctrl = LoopControl::new(start.location);
		Ok(Statement::Break(ctrl))
	}

	// Parse a continue statement.
	fn parse_continue_statement(&mut self) -> Result<Statement, CompilationError> {
		let start = self.expect_peek(TokenType::Continue)?;
		self.expect_peek(TokenType::Semicolon)?;

		let ctrl = LoopControl::new(start.location);
		Ok(Statement::Continue(ctrl))
	}

	// Parse an expression statement.
	//
	//cAn expressions statement is simply an expression followed by a semicolon.
	fn parse_expr_statement(&mut self) -> Result<Statement, CompilationError> {
		let expr = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::Semicolon)?;

		Ok(Statement::Expression(expr))
	}

	// Parse an expression.
	fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, CompilationError> {
		if self.lexer.peek().is_none() {
			Err(syntax_error(ErrorCode::E0001,
				Location::end(),
				"Unexpected end of file. Expected: <expression>".to_string()))
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

				_ => {
					let token = self.lexer.next().unwrap();

					return Err(syntax_error(ErrorCode::E0001,
						token.location,
						format!("Unexpected token: {} ({:?}). Expected one of <identifier>, <literal>, {}, {}, {} or {}",
							token, token,
							TokenType::Not, TokenType::Minus, TokenType::BitCompl, TokenType::LeftParen)))
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

	// Parse a literal expression.
	//
	// Literals may be integers, floating-point number or the booleans `true` and `false`.
	fn parse_literal_expression(&mut self) -> Result<Expression, CompilationError> {
		let token = self.lexer.next().unwrap();
		let literal = Literal::new_from_token(token)?;
		Ok(Expression::Literal(literal))
	}

	// Parse an identifier expression.
	fn parse_identifier_expression(&mut self) -> Result<Expression, CompilationError> {
		let ident = self.parse_identifier()?;
		Ok(Expression::Identifier(ident))
	}

	// Parse a prefix expression.
	//
	// A prefix expression is an expression preceded by a prefix operator, e.g. `~`, `-` or `not`.
	fn parse_prefix_expression(&mut self) -> Result<Expression, CompilationError> {
		let token = self.lexer.next().unwrap();
		let location = token.location;

		let operator = Operator::new_prefix_from_token(token)?;
		let expr = self.parse_expression(Precedence::Prefix)?;

		let prefix = Prefix::new(operator, expr, location);
		Ok(Expression::Prefix(prefix))
	}

	// Parse an infix expression.
	//
	// An infix expression is two expressions separated by an infix operator, e.g. `+`, `-`, etc.
	fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, CompilationError> {
		let token = self.lexer.next().unwrap();
		let operator = Operator::new_infix_from_token(token)?;


		if self.lexer.peek().is_none() {
			return Err(syntax_error(ErrorCode::E0001,
				Location::end(),
				"Unexpected end of file. Expected: <expression>".to_string()));
		} else {
			let precedence = self.peek_precedence();
			let right = self.parse_expression(precedence)?;

			let infix = Infix::new(left, operator, right);
			Ok(Expression::Infix(infix))
		}
	}

	// Parse a grouped expression.
	//
	// A grouped expression is an expression wrapped in parentheses, e.g. `( ... )`.
	fn parse_grouped_expression(&mut self) -> Result<Expression, CompilationError> {
		self.expect_peek(TokenType::LeftParen)?;

		let expr = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::RightParen)?;

		Ok(expr)
	}

	// Parse a function call expression.
	fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, CompilationError> {
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
				return Err(syntax_error(ErrorCode::E0001,
					Location::end(),
					format!("Unexpected end of file. Expected: {} ({:?})",
						TokenType::RightParen, TokenType::RightParen)));
			} else {
				if self.lexer.peek().unwrap().token_type == TokenType::RightParen {
					continue;
				}

				let token = self.lexer.next().unwrap();

				return Err(syntax_error(ErrorCode::E0002,
					token.location,
					format!("Unexpected token: {} ({:?}). Expected: {} ({:?})",
						token, token,
						TokenType::RightParen, TokenType::RightParen)));
			}
		}

		match left {
			Expression::Identifier(ident) => {
				let call = Call::new(ident, args);
				Ok(Expression::Call(call))
			},
			_ => Err(syntax_error(ErrorCode::E0003,
					left.get_location(),
					format!("Expected <identifier> to start a call expression, found {:?} expression.",
						left))),
		}
	}
}

/*
#[cfg(test)]
mod test {
	use super::*;

	macro_rules! output_parser_errors {
		($errors:expr) => (
			for error in $errors.iter() {
				println!("{}", error);
			}

			panic!("Errors found during parsing");
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
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Import(
							Import {
								path: ident!("std"),
							}
						)
					],
				},
			],
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_import_from_module() {
		let mut parser = Parser::new("import std::example;");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Import(
							Import {
								path: ident_with_path("example", vec![ident!("std")])
							}
						)
					],
				}
			],
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_enum() {
		let mut parser = Parser::new("enum example { x, y, z }");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Enum(
							Enum {
								identifier: ident!("example"),
								variants: vec![
									ident!("x"),
									ident!("y"),
									ident!("z"),
								]
							}
						),
					]
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_enum_with_trailing_comma() {
		let mut parser = Parser::new("enum example { x, y, z, }");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Enum(
							Enum {
								identifier: ident!("example"),
								variants: vec![
									ident!("x"),
									ident!("y"),
									ident!("z"),
								]
							}
						),
					]
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_struct() {
		let mut parser = Parser::new("struct example { a: i32, b: u32, c: f32, d: bool }");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Struct(
							Struct {
								identifier: ident!("example"),
								fields: vec![
									param!("a", Type::Int32),
									param!("b", Type::Uint32),
									param!("c", Type::Float),
									param!("d", Type::Bool),
								]
							}
						),
					]
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_struct_with_trailing_comma() {
		let mut parser = Parser::new("struct example { a: i32, b: u32, c: f32, d: bool }");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Struct(
							Struct {
								identifier: ident!("example"),
								fields: vec![
									param!("a", Type::Int32),
									param!("b", Type::Uint32),
									param!("c", Type::Float),
									param!("d", Type::Bool),
								]
							}
						),
					]
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_struct_custom_type() {
		let mut parser = Parser::new("struct example { a: Test }");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Struct(
							Struct {
								identifier: ident!("example"),
								fields: vec![
									param!(
										"a",
										Type::Custom(
											Box::new(
												ident!("Test")
											)
										)
									),
								]
							}
						),
					]
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_struct_custom_imported_type() {
		let mut parser = Parser::new("struct example { a: std::Test }");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Struct(
							Struct {
								identifier: ident!("example"),
								fields: vec![
									param!(
										"a",
										Type::Custom(
											Box::new(
												ident_with_path(
													"Test",
													vec![
														ident!("std")
													]
												)
											)
										)
									),
								]
							}
						),
					]
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty() {
		let mut parser = Parser::new("fn x() {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![],
								return_type: None,
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	#[should_panic]
	fn test_parse_function_empty_invalid_identifier() {
		let mut parser = Parser::new("fn a::b() {}");

		match parser.parse() {
			Ok(_) => {
				println!("Unexpected test success");
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_type_hint() {
		let mut parser = Parser::new("fn x() -> i32 {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![],
								return_type: Some(Type::Int32),
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_custom_type_hint() {
		let mut parser = Parser::new("fn x() -> Test {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![],
								return_type: Some(
									Type::Custom(
										Box::new(
											ident!("Test")
										)
									)
								),
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			],
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_custom_imported_type_hint() {
		let mut parser = Parser::new("fn x() -> std::Test {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![],
								return_type: Some(
									Type::Custom(
										Box::new(
											ident_with_path(
												"Test",
												vec![
													ident!("std")
												]
											)
										)
									)
								),
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_single_param() {
		let mut parser = Parser::new("fn x(a: i32) {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![
									param!("a", Type::Int32),
								],
								return_type: None,
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	#[should_panic]
	fn test_parse_function_empty_with_single_param_invalid_identifier() {
		let mut parser = Parser::new("fn x(a::b: i32) {}");

		match parser.parse() {
			Ok(_) => {
				println!("Unexpected test success");
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_single_param_and_type_hint() {
		let mut parser = Parser::new("fn x(a: i32) -> i32 {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![
									param!("a", Type::Int32),
								],
								return_type: Some(Type::Int32),
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_single_param_trailing_comma() {
		let mut parser = Parser::new("fn x(a: i32,) {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![
									param!("a", Type::Int32),
								],
								return_type: None,
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_multiple_params() {
		let mut parser = Parser::new("fn x(a: i32, b: u32, c: f32, d: bool) {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![
									param!("a", Type::Int32),
									param!("b", Type::Uint32),
									param!("c", Type::Float),
									param!("d", Type::Bool),
								],
								return_type: None,
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_multiple_params_and_type_hint() {
		let mut parser = Parser::new("fn x(a: i32, b: u32, c: f32, d: bool) -> i32 {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![
									param!("a", Type::Int32),
									param!("b", Type::Uint32),
									param!("c", Type::Float),
									param!("d", Type::Bool),
								],
								return_type: Some(Type::Int32),
								body: Block {
									statements: vec![],
									id: 0,
								},
							}
						)
					],
				}
			]
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
	}

	#[test]
	fn test_parse_function_empty_with_multiple_params_trailing_comma() {
		let mut parser = Parser::new("fn x(a: i32, b: u32, c: f32, d: bool,) {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function {
								identifier: ident!("x"),
								arguments: vec![
									param!("a", Type::Int32),
									param!("b", Type::Uint32),
									param!("c", Type::Float),
									param!("d", Type::Bool),
								],
								return_type: None,
								body: Block {
									statements: vec![],
									id: 0,
								}
							}
						),
					],
				},
			],
		};

		match parser.parse() {
			Ok(ast) => {
				assert_eq!(ast, expected);
			},
			Err(errors) => {
				output_parser_errors!(errors);
			}
		}
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

	#[test]
	fn test_parse_expression_precedence() {
		let mut parser = Parser::new("1 + 1 * 1;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_int!("1")),
					Operator::Plus,
					Box::new(Expression::Infix(
						Box::new(expr_literal_int!("1")),
						Operator::Multiply,
						Box::new(expr_literal_int!("1")),
					))
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
	fn test_parse_expression_precedence_grouped() {
		let mut parser = Parser::new("(1 + 1) * 1;");
		let expected = vec![
			Statement::Expression(
				Expression::Infix(
					Box::new(Expression::Infix(
						Box::new(expr_literal_int!("1")),
						Operator::Plus,
						Box::new(expr_literal_int!("1")),
					)),
					Operator::Multiply,
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
	fn test_let_statement() {
		let mut parser = Parser::new("let x: i32 = 1;");
		let expected = vec![
			Statement::Let(
				Let {
					identifier: ident!("x"),
					type_hint: Type::Int32,
					value: expr_literal_int!("1"),
				}
			)
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_return_statement() {
		let mut parser = Parser::new("return 1;");
		let expected = vec![
			Statement::Return(
				expr_literal_int!("1"),
			)
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_loop_statement() {
		let mut parser = Parser::new("loop { 1; }");
		let expected = vec![
			Statement::Loop(
				Block {
					statements: vec![
						Statement::Expression(
							expr_literal_int!("1")
						),
					],
					id: 0,
				}
			)
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_loop_statement_empty() {
		let mut parser = Parser::new("loop {}");
		let expected = vec![
			Statement::Loop(
				Block {
					statements: vec![],
					id: 0,
				}
			)
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_continue_statement() {
		let mut parser = Parser::new("continue;");
		let expected = vec![
			Statement::Continue(LoopRef::new()),
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_break_statement() {
		let mut parser = Parser::new("break;");
		let expected = vec![
			Statement::Break(LoopRef::new()),
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_if_statement() {
		let mut parser = Parser::new("if 1 {}");
		let expected = vec![
			Statement::If(
				If {
					condition: expr_literal_int!("1"),
					consequence: Block {
						statements: vec![],
						id: 0,
					},
					other: vec![],
					alternative: None,
				}
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_if_else_statement() {
		let mut parser = Parser::new("if 1 {} else {}");
		let expected = vec![
			Statement::If(
				If {
					condition: expr_literal_int!("1"),
					consequence: Block {
						statements: vec![],
						id: 0,
					},
					other: vec![],
					alternative: Some(Block {
						statements: vec![],
						id: 1,
					})
				}
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_if_elif_statement() {
		let mut parser = Parser::new("if 1 {} elif 1 {}");
		let expected = vec![
			Statement::If(
				If {
					condition: expr_literal_int!("1"),
					consequence: Block {
						statements: vec![],
						id: 0,
					},
					other: vec![
						(
							expr_literal_int!("1"),
							Block {
								statements: vec![],
								id: 1,
							},
						),
					],
					alternative: None
				}
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_if_elif_else_statement() {
		let mut parser = Parser::new("if 1 {} elif 1 {} else {}");
		let expected = vec![
			Statement::If(
				If {
					condition: expr_literal_int!("1"),
					consequence: Block {
						statements: vec![],
						id: 0,
					},
					other: vec![
						(
							expr_literal_int!("1"),
							Block {
								statements: vec![],
								id: 1,
							},
						),
					],
					alternative: Some(Block {
						statements: vec![],
						id: 2,
					})
				}
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_if_elif_multiple_else_statement() {
		let mut parser = Parser::new("if 1 {} elif 1 {} elif 1 {} elif 1 {} else {}");
		let expected = vec![
			Statement::If(
				If {
					condition: expr_literal_int!("1"),
					consequence: Block {
						statements: vec![],
						id: 0,
					},
					other: vec![
						(
							expr_literal_int!("1"),
							Block {
								statements: vec![],
								id: 1,
							},
						),
						(
							expr_literal_int!("1"),
							Block {
								statements: vec![],
								id: 2,
							},
						),
						(
							expr_literal_int!("1"),
							Block {
								statements: vec![],
								id: 3,
							},
						),
					],
					alternative: Some(Block {
						statements: vec![],
						id: 4,
					})
				}
			),
		];

		for expected in expected.iter() {
			let output = parser.parse_statement();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}

	#[test]
	fn test_parse_block() {
		let mut parser = Parser::new("{ let a: i32 = 1; loop { break; } return 5; }");
		let expected = vec![
			Block {
				statements: vec![
					Statement::Let(
						Let {
							identifier: ident!("a"),
							type_hint: Type::Int32,
							value: expr_literal_int!("1")
						}
					),
					Statement::Loop(
						Block {
							statements: vec![
								Statement::Break(LoopRef::new()),
							],
							id: 1,
						}
					),
					Statement::Return(
						expr_literal_int!("5")
					),
				],
				id: 0,
			}
		];

		for expected in expected.iter() {
			let output = parser.parse_block();

			if output.is_ok() {
				assert_eq!(&output.unwrap(), expected);
			} else {
				println!("{}", output.err().unwrap());
				panic!("Unexpected test failure")
			}
		}
	}
}
*/

