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
						token, token.token_type,
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
						token, token.token_type,
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

		let return_type: TypeHint;

		if self.expect_peek(TokenType::Arrow).is_ok() {
			return_type = self.parse_type()?;
		} else {
			return_type = TypeHint::None;
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
				TokenType::Print    => self.parse_print_statement()?,
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
				let literal_value = LiteralValue::True;
				let literal = Literal::new(TypeHint::Bool, literal_value, Location::end());

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

	// Parse a print statement.
	fn parse_print_statement(&mut self) -> Result<Statement, CompilationError> {
		self.expect_peek(TokenType::Print)?;

		let expr = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::Semicolon)?;

		Ok(Statement::Print(expr))
	}

	// Parse an expression statement.
	//
	// An expression statement is simply an expression followed by a semicolon.
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
						token, token.token_type,
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

	macro_rules! loc {
		($line:tt, $column:tt) => (
			Location::new($line, $column)
		)
	}

	macro_rules! ident {
		($value:tt, $loc:expr) => (
			Identifier::new($value.to_string(), $loc)
		)
	}

	macro_rules! arg {
		($value:tt, $loc:expr, $type_hint:expr) => (
			Argument::new(ident!($value, $loc), $type_hint)
		)
	}

	macro_rules! expr_literal_int {
		($value:tt, $loc:expr) => (
			Expression::Literal(
				Literal {
					type_hint: TypeHint::Int32,
					value: LiteralValue::Int32($value.to_string()),
					location: $loc,
				}
			)
		)
	}

	macro_rules! expr_literal_float {
		($value:tt, $loc:expr) => (
			Expression::Literal(
				Literal {
					type_hint: TypeHint::Float,
					value: LiteralValue::Float($value.to_string()),
					location: $loc,
				}
			)
		)
	}

	macro_rules! expr_literal_true {
		($loc:expr) => (
			Expression::Literal(
				Literal {
					type_hint: TypeHint::Bool,
					value: LiteralValue::True,
					location: $loc,
				}
			)
		)
	}

	macro_rules! expr_literal_false {
		($loc:expr) => (
			Expression::Literal(
				Literal {
					type_hint: TypeHint::Bool,
					value: LiteralValue::False,
					location: $loc,
				}
			)
		)
	}

	#[test]
	fn test_parse_function_empty() {
		let mut parser = Parser::new("fn x() {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![],
								TypeHint::None,
								Block::new(0, loc!(1, 8)),
								loc!(1, 1),
							)
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
	fn test_parse_function_empty_with_type_hint() {
		let mut parser = Parser::new("fn x() -> i32 {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![],
								TypeHint::Int32,
								Block::new(0, loc!(1, 15)),
								loc!(1, 1),
							)
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
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![
									arg!("a", loc!(1, 6), TypeHint::Int32),
								],
								TypeHint::None,
								Block::new(0, loc!(1, 14)),
								loc!(1, 1),
							)
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
	fn test_parse_function_empty_with_single_param_and_type_hint() {
		let mut parser = Parser::new("fn x(a: i32) -> i32 {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![
									arg!("a", loc!(1, 6), TypeHint::Int32),
								],
								TypeHint::Int32,
								Block::new(0, loc!(1, 21)),
								loc!(1, 1),
							)
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
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![
									arg!("a", loc!(1, 6), TypeHint::Int32),
								],
								TypeHint::None,
								Block::new(0, loc!(1, 15)),
								loc!(1, 1),
							)
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
		let mut parser = Parser::new("fn x(a: i32, b: f32, c: bool) {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![
									arg!("a", loc!(1, 6), TypeHint::Int32),
									arg!("b", loc!(1, 14), TypeHint::Float),
									arg!("c", loc!(1, 22), TypeHint::Bool),
								],
								TypeHint::None,
								Block::new(0, loc!(1, 31)),
								loc!(1, 1),
							)
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
		let mut parser = Parser::new("fn x(a: i32, b: f32, c: bool) -> i32 {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![
									arg!("a", loc!(1, 6), TypeHint::Int32),
									arg!("b", loc!(1, 14), TypeHint::Float),
									arg!("c", loc!(1, 22), TypeHint::Bool),
								],
								TypeHint::Int32,
								Block::new(0, loc!(1, 38)),
								loc!(1, 1),
							)
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
		let mut parser = Parser::new("fn x(a: i32, b: f32, c: bool,) {}");
		let expected = Ast {
			modules: vec![
				Module {
					definitions: vec![
						Definition::Function(
							Function::new(
								ident!("x", loc!(1, 4)),
								vec![
									arg!("a", loc!(1, 6), TypeHint::Int32),
									arg!("b", loc!(1, 14), TypeHint::Float),
									arg!("c", loc!(1, 22), TypeHint::Bool),
								],
								TypeHint::None,
								Block::new(
									0,
									loc!(1, 32),
								),
								loc!(1, 1),
							)
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
				expr_literal_true!(loc!(1, 1)),
			),
			Statement::Expression(
				expr_literal_false!(loc!(1, 7)),
			),
			Statement::Expression(
				expr_literal_int!("5", loc!(1, 14)),
			),
			Statement::Expression(
				expr_literal_float!("5.0", loc!(1, 17)),
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
					ident!("foo", loc!(1, 1)),
				),
			),
			Statement::Expression(
				Expression::Identifier(
					ident!("bar", loc!(1, 6)),
				),
			),
			Statement::Expression(
				Expression::Identifier(
					ident!("baz", loc!(1, 11)),
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
					Call::new(
						ident!("example", loc!(1, 1)),
						vec![],
					)
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
					Call::new(
						ident!("example", loc!(1, 1)),
						vec![
							expr_literal_int!("5", loc!(1, 9)),
						],
					)
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
					Call::new(
						ident!("example", loc!(1, 1)),
						vec![
							expr_literal_int!("5", loc!(1, 9)),
						],
					)
				),
			)
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
					Call::new(
						ident!("example", loc!(1, 1)),
						vec![
							expr_literal_int!("5", loc!(1, 9)),
							Expression::Identifier(ident!("x", loc!(1, 12))),
						],
					)
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
					Call::new(
						ident!("example", loc!(1, 1)),
						vec![
							expr_literal_int!("5", loc!(1, 9)),
							Expression::Identifier(ident!("x", loc!(1, 12))),
						],
					)
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
					Prefix::new(
						Operator::UnaryMinus,
						expr_literal_int!("5", loc!(1, 2)),
						loc!(1, 1),
					)
				)
			),
			Statement::Expression(
				Expression::Prefix(
					Prefix::new(
						Operator::Not,
						Expression::Identifier(ident!("x", loc!(1, 9))),
						loc!(1, 5),
					)
				)
			),
			Statement::Expression(
				Expression::Prefix(
					Prefix::new(
						Operator::BitCompl,
						expr_literal_int!("10", loc!(1, 13)),
						loc!(1, 12),
					)
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
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 1))),
						operator: Operator::Plus,
						right: Box::new(expr_literal_int!("1", loc!(1, 5))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 8))),
						operator: Operator::Minus,
						right: Box::new(expr_literal_int!("1", loc!(1, 12))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 15))),
						operator: Operator::Multiply,
						right: Box::new(expr_literal_int!("1", loc!(1, 19))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 22))),
						operator: Operator::Divide,
						right: Box::new(expr_literal_int!("1", loc!(1, 26))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 29))),
						operator: Operator::Modulo,
						right: Box::new(expr_literal_int!("1", loc!(1, 33))),
					}
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
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 1))),
						operator: Operator::BitAnd,
						right: Box::new(expr_literal_int!("1", loc!(1, 5))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 8))),
						operator: Operator::BitOr,
						right: Box::new(expr_literal_int!("1", loc!(1, 12))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 15))),
						operator: Operator::BitXor,
						right: Box::new(expr_literal_int!("1", loc!(1, 19))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 22))),
						operator: Operator::BitLeftShift,
						right: Box::new(expr_literal_int!("1", loc!(1, 27))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 30))),
						operator: Operator::BitRightShift,
						right: Box::new(expr_literal_int!("1", loc!(1, 35))),
					}
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
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 1))),
						operator: Operator::Equal,
						right: Box::new(expr_literal_int!("1", loc!(1, 6))),
					}
				)),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 9))),
						operator: Operator::NotEqual,
						right: Box::new(expr_literal_int!("1", loc!(1, 14))),
					}
				)),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 17))),
						operator: Operator::LessThan,
						right: Box::new(expr_literal_int!("1", loc!(1, 21))),
					}
				)),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 24))),
						operator: Operator::LessThanOrEqual,
						right: Box::new(expr_literal_int!("1", loc!(1, 29))),
					}
				)),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 32))),
						operator: Operator::GreaterThan,
						right: Box::new(expr_literal_int!("1", loc!(1, 36))),
					}
				)),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 39))),
						operator: Operator::GreaterThanOrEqual,
						right: Box::new(expr_literal_int!("1", loc!(1, 44))),
					}
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
					Infix {
						left: Box::new(expr_literal_true!(loc!(1, 1))),
						operator: Operator::And,
						right: Box::new(expr_literal_true!(loc!(1, 10))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(expr_literal_true!(loc!(1, 16))),
						operator: Operator::Or,
						right: Box::new(expr_literal_true!(loc!(1, 24))),
					}
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
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 1)))),
						operator: Operator::Assign,
						right: Box::new(expr_literal_int!("1", loc!(1, 5))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 8)))),
						operator: Operator::PlusAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 13))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 16)))),
						operator: Operator::MinusAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 21))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 24)))),
						operator: Operator::MultiplyAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 29))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 32)))),
						operator: Operator::DivideAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 37))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 40)))),
						operator: Operator::ModuloAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 45))),
					}
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
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 1)))),
						operator: Operator::BitAndAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 6))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 9)))),
						operator: Operator::BitOrAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 14))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 17)))),
						operator: Operator::BitXorAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 22))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 25)))),
						operator: Operator::BitLeftShiftAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 31))),
					}
				)
			),
			Statement::Expression(
				Expression::Infix(
					Infix {
						left: Box::new(Expression::Identifier(ident!("x", loc!(1, 34)))),
						operator: Operator::BitRightShiftAssign,
						right: Box::new(expr_literal_int!("1", loc!(1, 40))),
					}
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
					Infix {
						left: Box::new(expr_literal_int!("1", loc!(1, 1))),
						operator: Operator::Plus,
						right: Box::new(Expression::Infix(Infix {
							left: Box::new(expr_literal_int!("1", loc!(1, 5))),
							operator: Operator::Multiply,
							right: Box::new(expr_literal_int!("1", loc!(1, 9))),
						})),
					}
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
					Infix {
						left: Box::new(Expression::Infix(Infix {
							left: Box::new(expr_literal_int!("1", loc!(1, 2))),
							operator: Operator::Plus,
							right: Box::new(expr_literal_int!("1", loc!(1, 6))),
						})),
						operator: Operator::Multiply,
						right: Box::new(expr_literal_int!("1", loc!(1, 11))),
					}
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
				Let::new(
					ident!("x", loc!(1, 5)),
					TypeHint::Int32,
					expr_literal_int!("1", loc!(1, 14)),
					loc!(1, 1),
				)
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
				expr_literal_int!("1", loc!(1, 8)),
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
				Loop {
					body: Block::new_with_statements(
						vec![
							Statement::Expression(
								expr_literal_int!("1", loc!(1, 8))
							),
						],
						0,
						loc!(1, 6),
					),
					location: loc!(1, 1),
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
				Loop {
					body: Block::new(
						0,
						loc!(1, 6),
					),
					location: loc!(1, 1),
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
			Statement::Continue(LoopControl {
				loop_id: 0,
				location: loc!(1, 1),
			}),
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
			Statement::Break(LoopControl {
				loop_id: 0,
				location: loc!(1, 1),
			}),
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
					branches: vec![
						Branch {
							condition: expr_literal_int!("1", loc!(1, 4)),
							consequence: Block::new(0, loc!(1, 6)),
						},
					],
					location: loc!(1, 1),
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
					branches: vec![
						Branch {
							condition: expr_literal_int!("1", loc!(1, 4)),
							consequence: Block::new(0, loc!(1, 6)),
						},
						Branch {
							condition: expr_literal_true!(loc!(0, 0)),
							consequence: Block::new(1, loc!(1, 14)),
						},
					],
					location: loc!(1, 1),
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
					branches: vec![
						Branch {
							condition: expr_literal_int!("1", loc!(1, 4)),
							consequence: Block::new(0, loc!(1, 6)),
						},
						Branch {
							condition: expr_literal_int!("1", loc!(1, 14)),
							consequence: Block::new(1, loc!(1, 16)),
						},
					],
					location: loc!(1, 1),
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
					branches: vec![
						Branch {
							condition: expr_literal_int!("1", loc!(1, 4)),
							consequence: Block::new(0, loc!(1, 6)),
						},
						Branch {
							condition: expr_literal_int!("1", loc!(1, 14)),
							consequence: Block::new(1, loc!(1, 16)),
						},
						Branch {
							condition: expr_literal_true!(loc!(0, 0)),
							consequence: Block::new(2, loc!(1, 24)),
						},
					],
					location: loc!(1, 1),
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
					branches: vec![
						Branch {
							condition: expr_literal_int!("1", loc!(1, 4)),
							consequence: Block::new(
								0,
								loc!(1, 6),
							),
						},
						Branch {
							condition: expr_literal_int!("1", loc!(1, 14)),
							consequence: Block::new(
								1,
								loc!(1, 16),
							),
						},
						Branch {
							condition: expr_literal_int!("1", loc!(1, 24)),
							consequence: Block::new(
								2,
								loc!(1, 26),
							),
						},
						Branch {
							condition: expr_literal_int!("1", loc!(1, 34)),
							consequence: Block::new(
								3,
								loc!(1, 36),
							),
						},
						Branch {
							condition: expr_literal_true!(loc!(0, 0)),
							consequence: Block::new(
								4,
								loc!(1, 44),
							),
						},
					],
					location: loc!(1, 1),
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
			Block::new_with_statements(
				vec![
					Statement::Let(
						Let::new(
							ident!("a", loc!(1, 7)),
							TypeHint::Int32,
							expr_literal_int!("1", loc!(1, 16)),
							loc!(1, 3),
						)
					),
					Statement::Loop(
						Loop::new(
							Block::new_with_statements(
								vec![
									Statement::Break(LoopControl {
										loop_id: 0,
										location: loc!(1, 26),
									}),
								],
								1,
								loc!(1, 24),
							),
							loc!(1, 19),
						),
					),
					Statement::Return(
						expr_literal_int!("5", loc!(1, 42))
					),
				],
				0,
				loc!(1, 1),
			)
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
