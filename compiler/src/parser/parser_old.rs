//!
//!
//!

use std::iter::Peekable;
use lexer::{
	lexer::Lexer,
	token::{
		Location,
		Token,
		TokenType,
	},
};
use parser::ast;
use parser::error::{Error, ErrorCode};
use parser::symbol::{SymbolTable};


macro_rules! pointer_byte_size {
	() => (
		if cfg!(target_pointer_width="64") {
			8
		} else if cfg!(target_pointer_width="32") {
			4
		} else {
			panic!("Unsupported value for target_pointer_width")
		}
	)
}


/// Operator precedences for Cactus.
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
	Lowest,
	Equals,      // ==
	LessGreater, // > or <
	Sum,         // +
	Product,     // *
	Prefix,      // -X or !X
	Call,        // myFunction(X)
}


impl Precedence {
	/// Try to convert a `TokenType` to a `Precedence`.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::{Location, Token, TokenType};
	/// use cactus::parser::parser::Precedence;
	///
	/// let token = Token::from_type(TokenType::Equal, Location::new(1, 0));
	/// let precedence = Precedence::from_token(token);
	/// assert!(precedence.is_ok());
	/// assert_eq!(precedence.unwrap(), Precedence::Equals);
	///
	/// let token = Token::new(TokenType::Illegal, "@".to_string(), Location::new(1, 0));
	/// let precedence = Precedence::from_token(token);
	/// assert!(precedence.is_err());
	/// ```
	pub fn from_token(token: Token) -> Result<Precedence, Error> {
		match token.token_type {
			TokenType::Equal
			| TokenType::NotEqual => Ok(Precedence::Equals),

			TokenType::LessThan
			| TokenType::GreaterThan => Ok(Precedence::LessGreater),

			TokenType::Plus
			| TokenType::Minus => Ok(Precedence::Sum),

			TokenType::Multiply
			| TokenType::Divide => Ok(Precedence::Product),

			TokenType::LeftParen => Ok(Precedence::Call),

			_ => Err(Error::from_token(ErrorCode::E0005, Some(token))),
		}
	}
}


/// A parser for the high-level language.
pub struct Parser<'a> {
	pub module: ast::Module,
	pub errors: Vec<Error>,
	lexer: Peekable<Lexer<'a>>,
	symbol_table: SymbolTable,
	scope: Option<SymbolTable>,
}


impl<'a> Parser<'a> {
	/// Create a new instance of `Parser`.
	///
	/// # Example
	/// ```
	/// use cactus::parser::parser::Parser;
	///
	/// Parser::new("let x: i32 = 5;");
	/// ```
	pub fn new(input: &'a str) -> Parser<'a> {
		Parser {
			module: ast::Module::new(),
			errors: Vec::new(),
			lexer: Lexer::new(input).peekable(),
			symbol_table: SymbolTable::new(pointer_byte_size!(), 0),
			scope: None,
		}
	}

	/// Check if any errors occurred during parsing.
	pub fn has_errors(&self) -> bool {
		!self.errors.is_empty()
	}

	/// Parse the input into an AST.
	///
	/// # Example
	/// ```
	/// use cactus::parser::parser::Parser;
	///
	/// let mut parser = Parser::new("let x: i32 = 5;");
	///
	/// parser.parse();
	/// ```
	pub fn parse(&mut self) {
		loop {
			let cur = self.lexer.next();

			if cur.is_none() {
				break;
			}

			let statement = self.parse_statement(cur.unwrap());

			match statement {
				Ok(statement) => self.module.push(statement),
				Err(error) => {
					self.errors.push(error);

					// consume tokens until we find a synchronisation point
					// at which point we can safely start the parser again
					/*
					loop {
						if self.cur.is_none() {
							break;
						}

						if self.peek_type_is(TokenType::Semicolon) {
							break;
						}

						self.lexer.next();
					}
					*/
				},
			};
		}
	}

	//
	//
	//
	fn peek_type_is(&mut self, token_type: TokenType) -> bool {
		self.lexer.peek().is_some() && self.lexer.peek().unwrap().token_type == token_type
	}

	//
	//
	//
	fn parse_statement(&mut self, token: Token) -> Result<ast::Statement, Error> {
		match token.token_type {
			TokenType::Let => self.parse_let_statement(),
			TokenType::Return => self.parse_return_statement(),
			TokenType::Function => self.parse_function_statement(),
			TokenType::If => self.parse_if_statement(),
			_ => self.parse_expression_statement(token),
		}
	}

	// Parse a let (assignment) statement.
	//
	// should be in the form `let <identifier>: <type> = <expression>;`
	fn parse_let_statement(&mut self) -> Result<ast::Statement, Error> {
		let ident = self.parse_identifier()?;

		if !self.peek_type_is(TokenType::Colon) {
			let token = self.lexer.next();
			return Err(Error::from_token(ErrorCode::E0001, token));
		} else {
			self.lexer.next();
		}

		let type_hint = self.parse_type()?;

		match self.scope {
			Some(ref mut table) => {
				table.insert_local(ident.value.clone(), type_hint.size_of())
			},
			None => {
				return Err(Error::new(ErrorCode::E0001, ident.location));
			},
		};

		if !self.peek_type_is(TokenType::Assign) {
			let token = self.lexer.next();
			return Err(Error::from_token(ErrorCode::E0001, token));
		} else {
			self.lexer.next();
		}

		let token = self.lexer.next().unwrap_or(Token::eof());
		let expression = self.parse_expression(token, Precedence::Lowest)?;

		if !self.peek_type_is(TokenType::Semicolon) {
			let token = self.lexer.next();
			return Err(Error::from_token(ErrorCode::E0001, token));
		} else {
			self.lexer.next();
		}

		Ok(ast::Statement::Let(ident, type_hint, expression))
	}

	// Parse a return statement.
	//
	// Should be in the form `return <expression>;`
	fn parse_return_statement(&mut self) -> Result<ast::Statement, Error> {
		let token = self.lexer.next().unwrap_or(Token::eof());
		let expression = self.parse_expression(token, Precedence::Lowest)?;

		if !self.peek_type_is(TokenType::Semicolon) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E1007, token.location));
		} else {
			self.lexer.next();
		}

		Ok(ast::Statement::Return(expression))
	}

	//
	//
	//
	fn parse_block_statement(&mut self) -> Result<ast::Statement, Error> {
		let block = self.parse_block()?;
		Ok(ast::Statement::Block(block))
	}

	// Parse an expression as a statement,
	fn parse_expression_statement(&mut self, token: Token) -> Result<ast::Statement, Error> {
		let expression = self.parse_expression(token, Precedence::Lowest)?;

		if self.peek_type_is(TokenType::Semicolon) {
			self.lexer.next();
		}

		Ok(ast::Statement::Expression(expression))
	}

	//
	//
	//
	fn parse_function_statement(&mut self) -> Result<ast::Statement, Error> {
		let ident = self.parse_identifier()?;
		let mut func_symbol = self.symbol_table.new_function(ident.value.clone());

		if !self.peek_type_is(TokenType::LeftParen) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		let mut params: Vec<ast::Parameter> = Vec::new();

		loop {
			if self.peek_type_is(TokenType::RightParen) {
				break;
			}

			let ident = self.parse_identifier()?;

			if !self.peek_type_is(TokenType::Colon) {
				let token = self.lexer.next().unwrap_or(Token::eof());
				return Err(Error::new(ErrorCode::E1007, token.location));
			} else {
				self.lexer.next();
			}

			let type_hint = self.parse_type()?;

			func_symbol.insert_argument(ident.value.clone(), type_hint.size_of());
			params.push(ast::Parameter::new(ident, type_hint));

			if self.peek_type_is(TokenType::Comma) {
				self.lexer.next();
			} else {
				break;
			}
		}

		if !self.peek_type_is(TokenType::RightParen) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E1006, token.location));
		} else {
			self.lexer.next();
		}

		let ret_type = if self.peek_type_is(TokenType::Arrow) {
			self.lexer.next();
			Some(self.parse_type()?)
		} else {
			None
		};

		// save the current scope (if it exists)
		// and use the function for the current scope
		let prev_scope = self.scope.clone();
		func_symbol.align_index();
		self.scope = Some(SymbolTable::new(pointer_byte_size!(), func_symbol.table.index));

		let block = self.parse_block()?;

		func_symbol.extend(self.scope.clone().unwrap());
		self.scope = prev_scope;

		self.symbol_table.insert_function(func_symbol);

		Ok(ast::Statement::Function(ident, params, ret_type, block))
	}

	//
	//
	//
	fn parse_if_statement(&mut self) -> Result<ast::Statement, Error> {
		let token = self.lexer.next().unwrap_or(Token::eof());
		let condition = self.parse_expression(token, Precedence::Lowest)?;
		let consequence = self.parse_block()?;

		Ok(ast::Statement::If(condition, consequence))
	}

	//
	//
	//
	fn parse_type(&mut self) -> Result<ast::Type, Error> {
		let type_token = self.lexer.next().unwrap_or(Token::eof());

		match type_token.token_type {
			TokenType::TypeInt32 => Ok(ast::Type::Int32),
			TokenType::TypeFloat => Ok(ast::Type::Float),
			TokenType::TypeBool  => Ok(ast::Type::Bool),

			_ => Err(Error::from_token(ErrorCode::E1005, type_token))
		}
	}

	//
	//
	//
	fn parse_identifier(&mut self) -> Result<ast::Identifier, Error> {
		let token = self.lexer.next().unwrap_or(Token::eof());

		if token.token_type != TokenType::Identifier {
			Err(Error::from_token(ErrorCode::E1004, token))
		} else {
			Ok(ast::Identifier::new(token.value.unwrap(), token.location))
		}
	}

	//
	//
	//
	fn parse_block(&mut self) -> Result<ast::Block, Error> {
		if !self.peek_type_is(TokenType::LeftBrace) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E1002, token.location));
		} else {
			self.lexer.next();
		}

		let mut block = ast::Block::new();

		loop {
			if self.lexer.peek().is_none() {
				break;
			}

			if self.peek_type_is(TokenType::RightBrace) {
				break;
			}

			let token = self.lexer.next().unwrap();
			let statement = self.parse_statement(token)?;

			block.push(statement);
		}

		if !self.peek_type_is(TokenType::RightBrace) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::from_token(ErrorCode::E1003, token));
		} else {
			self.lexer.next();
		}

		Ok(block)
	}

	//
	//
	//
	fn parse_expression(&mut self, token: Token, precedence: Precedence) -> Result<ast::Expression, Error> {
		let mut left = match token.token_type {
			// TODO: move specific token types to be handled by parse_prefix_expression?
			TokenType::Identifier => Ok(ast::Expression::Identifier(ast::Identifier::from_token(token)?)),

			TokenType::Integer
			| TokenType::Float
			| TokenType::True
			| TokenType::False => Ok(ast::Expression::Literal(ast::Literal::from_token(token)?)),

			TokenType::Bang
			| TokenType::Minus => self.parse_prefix_expression(token),

			TokenType::LeftParen => self.parse_grouped_expression(),

			_ => {
				println!("Unexpected token: {:?}", token);
				Err(Error::from_token(ErrorCode::E1001, token))
			}
		}?;

		loop {
			if !self.peek_type_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
				let next_token = self.lexer.next().unwrap_or(Token::eof());

				left = match next_token.token_type {
					TokenType::LeftParen => self.parse_call_expression(left)?,
					_ => self.parse_infix_expression(left, next_token)?,
				};

				continue;
			}

			break;
		}

		Ok(left)
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

	//
	//
	//
	fn parse_prefix_expression(&mut self, token: Token) -> Result<ast::Expression, Error> {
		let operator = ast::Operator::from_prefix_token(token)?;
		let token = self.lexer.next().unwrap_or(Token::eof());
		let right = self.parse_expression(token, Precedence::Prefix)?;

		Ok(ast::Expression::Prefix(operator, Box::new(right)))
	}

	//
	//
	//
	fn parse_infix_expression(&mut self, left: ast::Expression, token: Token) -> Result<ast::Expression, Error> {
		let operator = ast::Operator::from_infix_token(token)?;

		let token = self.lexer.next().unwrap_or(Token::eof());
		let precedence = Precedence::from_token(token.clone()).unwrap_or(Precedence::Lowest);

		let right = self.parse_expression(token, precedence)?;

		Ok(ast::Expression::Infix(Box::new(left), operator, Box::new(right)))
	}

	//
	//
	//
	fn parse_grouped_expression(&mut self) -> Result<ast::Expression, Error> {
		let token = self.lexer.next().unwrap_or(Token::eof());
		let expr = self.parse_expression(token, Precedence::Lowest)?;

		if !self.peek_type_is(TokenType::RightParen) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::from_token(ErrorCode::E1000, token));
		} else {
			self.lexer.next();
		}

		Ok(expr)

	}

	//
	//
	//
	fn parse_call_expression(&mut self, identifier: ast::Expression) -> Result<ast::Expression, Error> {
		let mut args: Vec<ast::Expression> = Vec::new();

		loop {
			let token = self.lexer.next().unwrap_or(Token::eof());
			let argument = self.parse_expression(token, Precedence::Lowest)?;

			args.push(argument);

			if self.peek_type_is(TokenType::RightParen) {
				self.lexer.next();
				break;
			}

			if self.peek_type_is(TokenType::Comma) {
				self.lexer.next();
				continue;
			}

			return Err(Error::from_token(ErrorCode::E0001, self.lexer.next()));
		}

		match identifier {
			ast::Expression::Identifier(ident) => Ok(ast::Expression::Call(ident, args)),
			_ => panic!("Expected identifier, found: {:?}", identifier),
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;

	//
	//
	//
	macro_rules! expr_ident {
		($ident:expr, $line:expr, $column:expr) => (
			ast::Expression::Identifier(
				ast::Identifier::new(
					$ident.to_string(),
					Location::new($line, $column),
				)
			)
		)
	}

	//
	//
	//
	macro_rules! expr_literal_i32 {
		($value:expr) => (
			ast::Expression::Literal(
				ast::Literal::Int32(
					$value.to_string()
				)
			)
		)
	}

	//
	//
	//
	macro_rules! param {
		($ident:expr, $line:expr, $column:expr, $type_hint:expr) => (
			ast::Parameter::new(
				ast::Identifier::new(
					$ident.to_string(),
					Location::new($line, $column)
				),
				$type_hint
			)
		)
	}

	// Test precedence ordering
	#[test]
	fn test_precedence_ordering() {
		assert!(Precedence::Lowest < Precedence::Equals);
		assert!(Precedence::Equals < Precedence::LessGreater);
		assert!(Precedence::LessGreater < Precedence::Sum);
		assert!(Precedence::Sum < Precedence::Product);
		assert!(Precedence::Product < Precedence::Prefix);
		assert!(Precedence::Prefix < Precedence::Call);
	}

	// Test a let statement is correctly matched.
	#[test]
	fn test_let_statement() {
		let mut parser = Parser::new("let x: i32 = 5;");
		let expected = ast::Statement::Let(
			ast::Identifier::new(
				"x".to_string(),
				Location::new(1, 5)
			),
			ast::Type::Int32,
			ast::Expression::Literal(
				ast::Literal::Int32("5".to_string())
			)
		);

		parser.scope = Some(SymbolTable::new(pointer_byte_size!(), 0));
		parser.parse();

		if parser.has_errors() {
			println!("{}", parser.errors.len());
		}

		assert_eq!(parser.module.statements[0], expected);
	}

	// Test a return statement is correctly matched.
	#[test]
	fn test_return_statement() {
		let mut parser = Parser::new("return 5; return x;");
		let expected = vec![
			ast::Statement::Return(
				ast::Expression::Literal(
					ast::Literal::Int32("5".to_string())
				)
			),
			ast::Statement::Return(
				ast::Expression::Identifier(
					ast::Identifier::new("x".to_string(), Location::new(1, 18))
				)
			),
		];

		parser.parse();

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	#[test]
	fn test_if_statement() {
		let mut parser = Parser::new("if 1 > 5 { true; }");
		//let expected: Vec<ast::Statement> = vec![];

		parser.parse();

		dbg!(parser.module.statements);

		assert!(false);
	}

	// Test prefix operators are correctly matched
	#[test]
	fn test_prefix_operators() {
		let mut parser = Parser::new("!x; -5;");
		let expected = vec![
			ast::Statement::Expression(
				ast::Expression::Prefix(
					ast::Operator::Not,
					Box::new(
						ast::Expression::Identifier(
							ast::Identifier::new(
								"x".to_string(),
								Location::new(1, 2)
							)
						)
					)
				)
			),
			ast::Statement::Expression(
				ast::Expression::Prefix(
					ast::Operator::UnaryMinus,
					Box::new(
						ast::Expression::Literal(
							ast::Literal::Int32("5".to_string())
						)
					)
				)
			)
		];

		parser.parse();

		println!("errors: {}", parser.errors.len());

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	// Test infix operators are correctly matched
	#[test]
	fn test_infix_operators() {
		let mut parser = Parser::new("x + 5; 10 * 5; x / y;");
		let expected = vec![
			ast::Statement::Expression(
				ast::Expression::Infix(
					Box::new(expr_ident!("x", 1, 1)),
					ast::Operator::Plus,
					Box::new(expr_literal_i32!(5)),
				)
			),
			ast::Statement::Expression(
				ast::Expression::Infix(
					Box::new(expr_literal_i32!(10)),
					ast::Operator::Multiply,
					Box::new(expr_literal_i32!(5)),
				)
			),
			ast::Statement::Expression(
				ast::Expression::Infix(
					Box::new(expr_ident!("x", 1, 16)),
					ast::Operator::Divide,
					Box::new(expr_ident!("y", 1, 20)),
				)
			),
		];

		parser.parse();

		println!("errors: {}", parser.errors.len());

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	// Test infix operator precedence
	#[test]
	fn test_precedence() {
		let mut parser = Parser::new("1 + 2 * 3;");
		let expected = vec![
			ast::Statement::Expression(
				ast::Expression::Infix(
					Box::new(expr_literal_i32!(1)),
					ast::Operator::Plus,
					Box::new(
						ast::Expression::Infix(
							Box::new(expr_literal_i32!(2)),
							ast::Operator::Multiply,
							Box::new(expr_literal_i32!(3)),
						)
					),
				)
			),
		];

		parser.parse();

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	// Test infix operator precedence with brackets
	#[test]
	fn test_precedence_grouped() {
		let mut parser = Parser::new("(1 + 2) * 3;");
		let expected = vec![
			ast::Statement::Expression(
				ast::Expression::Infix(
					Box::new(
						ast::Expression::Infix(
							Box::new(expr_literal_i32!(1)),
							ast::Operator::Plus,
							Box::new(expr_literal_i32!(2)),
						)
					),
					ast::Operator::Multiply,
					Box::new(expr_literal_i32!(3)),
				),
			),
		];

		parser.parse();

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	// Test functions are correctly matched
	#[test]
	fn test_function() {
		let mut parser = Parser::new("fn square(n: i32) -> i32 { return n * n; }");
		let expected = vec![
			ast::Statement::Function(
				ast::Identifier::new(
					"square".to_string(),
					Location::new(1, 4)
				),
				vec![param!("n", 1, 11, ast::Type::Int32)],
				Some(ast::Type::Int32),
				ast::Block {
					statements: vec![
						ast::Statement::Return(
							ast::Expression::Infix(
								Box::new(expr_ident!("n", 1, 35)),
								ast::Operator::Multiply,
								Box::new(expr_ident!("n", 1, 39)),
							)
						)
					]
				}
			)
		];

		parser.parse();

		println!("errors: {}", parser.errors.len());

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	// Test function calls
	#[test]
	fn test_function_call() {
		let mut parser = Parser::new("square(5);");
		let expected = vec![
			ast::Statement::Expression(
				ast::Expression::Call(
					ast::Identifier::new(
						"square".to_string(),
						Location::new(1, 1)
					),
					vec![
						expr_literal_i32!("5")
					]
				)
			)
		];

		parser.parse();
		println!("errors: {}", parser.errors.len());

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	// Test symbol table
	#[test]
	fn test_symbol_table() {
		let input = "fn square(n: i32) -> i32 { let ret: i32 = n * n; return ret; }";
		let mut parser = Parser::new(input);

		parser.parse();

		//dbg!(parser.symbol_table);
		println!("{}",parser.symbol_table);

		panic!("test failed");
	}
}
