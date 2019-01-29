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
use parser::{
	ast::{
		Module,
		Statement,
		Expression,
		Literal,
		Type,
		Identifier,
		Operator,
		Parameter,
		Block,
	},
	error::{
		Error,
		ErrorCode
	}
};


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
	/// Convert a `TokenType` to a `Precedence`.
	///
	/// If the `TokenType` cannot be converted to a `Precedence` a panic will occur.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::TokenType;
	/// use compiler::parser::parser::Precedence;
	///
	/// let precedence = Precedence::from_token_type(TokenType::Equal);
	///
	/// assert_eq!(precedence, Precedence::Equals);
	/// ```
	pub fn from_token_type(token_type: TokenType) -> Precedence {
		match token_type {
			TokenType::Equal
			| TokenType::NotEqual => Precedence::Equals,

			TokenType::LessThan
			| TokenType::GreaterThan => Precedence::LessGreater,

			TokenType::Plus
			| TokenType::Minus => Precedence::Sum,

			TokenType::Multiply
			| TokenType::Divide  => Precedence::Product,

			_ => panic!("Unable to convert TokenType to Precedence: {:?}", token_type),
		}
	}

	/// Try to convert a `TokenType` to a `Precedence`.
	///
	/// # Example
	/// ```
	/// use compiler::lexer::token::TokenType;
	/// use compiler::parser::parser::Precedence;
	///
	/// let precedence = Precedence::from_token_type_safe(TokenType::Equal);
	/// assert!(precedence.is_ok());
	/// assert_eq!(precedence.unwrap(), Precedence::Equals);
	///
	/// let precedence = Precedence::from_token_type_safe(TokenType::Illegal);
	/// assert!(precedence.is_err());
	/// ```
	pub fn from_token_type_safe(token_type: TokenType) -> Result<Precedence, ()> {
		match token_type {
			TokenType::Equal
			| TokenType::NotEqual => Ok(Precedence::Equals),

			TokenType::LessThan
			| TokenType::GreaterThan => Ok(Precedence::LessGreater),

			TokenType::Plus
			| TokenType::Minus => Ok(Precedence::Sum),

			TokenType::Multiply
			| TokenType::Divide  => Ok(Precedence::Product),

			// TODO: put a proper error in here
			_ => Err(())
		}
	}
}


/// A parser for the high-level language.
pub struct Parser<'a> {
	pub module: Module,
	pub errors: Vec<Error>,
	lexer: Peekable<Lexer<'a>>,
}


impl<'a> Parser<'a> {
	/// Create a new instance of `Parser`.
	///
	/// # Example
	/// ```
	/// use compiler::parser::parser::Parser;
	///
	/// Parser::new("let x: i32 = 5;");
	/// ```
	pub fn new(input: &'a str) -> Parser<'a> {
		Parser {
			module: Module::new(),
			errors: Vec::new(),
			lexer: Lexer::new(input).peekable(),
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
	/// use compiler::parser::parser::Parser;
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
	fn parse_statement(&mut self, token: Token) -> Result<Statement, Error> {
		match token.token_type {
			TokenType::Let => self.parse_let_statement(),
			TokenType::Return => self.parse_return_statement(),
			TokenType::Print => self.parse_print_statement(),
			_ => self.parse_expression_statement(token),
		}
	}

	//
	//
	//
	fn peek_type_is(&mut self, token_type: TokenType) -> bool {
		self.lexer.peek().is_some() && self.lexer.peek().unwrap().token_type == token_type
	}

	// Parse a let (assignment) statement.
	//
	// should be in the form `let <identifier>: <type> = <expression>;`
	fn parse_let_statement(&mut self) -> Result<Statement, Error> {
		let ident = self.parse_identifier()?;

		if !self.peek_type_is(TokenType::Colon) {
			let token = self.lexer.next();
			return Err(Error::from_token(ErrorCode::E0001, token));
		} else {
			self.lexer.next();
		}

		let type_hint = self.parse_type()?;

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

		Ok(Statement::Let(ident, type_hint, expression))
	}

	// Parse a return statement.
	//
	// Should be in the form `return <expression>;`
	fn parse_return_statement(&mut self) -> Result<Statement, Error> {
		let token = self.lexer.next().unwrap_or(Token::eof());
		let expression = self.parse_expression(token, Precedence::Lowest)?;

		if !self.peek_type_is(TokenType::Semicolon) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		Ok(Statement::Return(expression))
	}

	// Parse a print statement.
	//
	// Should be in the form `print <expression>;`
	fn parse_print_statement(&mut self) -> Result<Statement, Error> {
		let token = self.lexer.next().unwrap_or(Token::eof());
		let expression = self.parse_expression(token, Precedence::Lowest)?;

		if !self.peek_type_is(TokenType::Semicolon) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		Ok(Statement::Print(expression))
	}

	// Parse an expression as a statement,
	fn parse_expression_statement(&mut self, token: Token) -> Result<Statement, Error> {
		let expression = self.parse_expression(token, Precedence::Lowest)?;

		if self.peek_type_is(TokenType::Semicolon) {
			self.lexer.next();
		}

		Ok(Statement::Expression(expression))
	}

	//
	//
	//
	fn parse_type(&mut self) -> Result<Type, Error> {
		let type_token = self.lexer.next().unwrap_or(Token::eof());

		match type_token.token_type {
			TokenType::TypeInt32 => Ok(Type::Int32),
			TokenType::TypeFloat => Ok(Type::Float),
			TokenType::TypeBool  => Ok(Type::Bool),

			_ => Err(Error::new(ErrorCode::E0001, type_token.location))
		}
	}

	//
	//
	//
	fn parse_identifier(&mut self) -> Result<Identifier, Error> {
		if self.lexer.peek().is_none() {
			Err(Error::new(ErrorCode::E0001, Location::end()))
		} else {
			let token = self.lexer.next().unwrap();

			if token.token_type != TokenType::Identifier {
				Err(Error::new(ErrorCode::E0001, token.location))
			} else {
				Ok(Identifier::new(token.value.unwrap(), token.location))
			}
		}
	}

	//
	//
	//
	fn parse_expression(&mut self, token: Token, precedence: Precedence) -> Result<Expression, Error> {
		let mut left = match token.token_type {
			// TODO: move specific token types to be handled by parse_prefix_expression?
			TokenType::Identifier => Ok(Expression::Identifier(Identifier::from_token(token))),
			TokenType::Integer
			| TokenType::Float
			| TokenType::True
			| TokenType::False => Ok(Expression::Literal(Literal::from_token(token))),

			TokenType::Bang
			| TokenType::Minus => self.parse_prefix_expression(token),

			TokenType::Function => self.parse_function_expression(),

			_ => {
				println!("Unexpected token: {:?}", token);
				Err(Error::new(ErrorCode::E0001, token.location))
			}
		}?;

		loop {
			if self.peek_type_is(TokenType::Semicolon) {
				break;
			}

			dbg!(precedence);
			dbg!(self.peek_precedence());

			if precedence < self.peek_precedence() {
				let next_token = self.lexer.next().unwrap_or(Token::eof());

				left = match next_token.token_type {
					TokenType::LeftParen => self.parse_call_expression(left)?,
					_ => self.parse_infix_expression(left, next_token)?,
				};
			} else {
				break;
			}
		}

		Ok(left)
	}

	//
	//
	//
	fn parse_prefix_expression(&mut self, token: Token) -> Result<Expression, Error> {
		let operator = Operator::from_prefix_token(token);
		let token = self.lexer.next().unwrap_or(Token::eof());
		let right = self.parse_expression(token, Precedence::Prefix)?;

		Ok(Expression::Prefix(operator, Box::new(right)))
	}

	//
	//
	//
	fn parse_infix_expression(&mut self, left: Expression, token: Token) -> Result<Expression, Error> {
		let operator = Operator::from_infix_token(token);

		let token = self.lexer.next().unwrap_or(Token::eof());
		let precedence = Precedence::from_token_type_safe(token.token_type).unwrap_or(Precedence::Lowest);

		let right = self.parse_expression(token, precedence)?;

		Ok(Expression::Infix(Box::new(left), operator, Box::new(right)))
	}

	//
	//
	//
	fn peek_precedence(&mut self) -> Precedence {
		if self.lexer.peek().is_some() {
			let peek_type = self.lexer.peek().unwrap().token_type;
			let precedence = Precedence::from_token_type_safe(peek_type);

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
	fn parse_function_expression(&mut self) -> Result<Expression, Error> {
		let ident = self.parse_identifier()?;

		if !self.peek_type_is(TokenType::LeftParen) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		let mut params: Vec<Parameter> = Vec::new();

		loop {
			if self.peek_type_is(TokenType::RightParen) {
				break;
			}

			let ident = self.parse_identifier()?;

			if !self.peek_type_is(TokenType::Colon) {
				let token = self.lexer.next().unwrap_or(Token::eof());
				return Err(Error::new(ErrorCode::E0001, token.location));
			} else {
				self.lexer.next();
			}

			let type_hint = self.parse_type()?;
			params.push(Parameter::new(ident, type_hint));

			if self.peek_type_is(TokenType::Comma) {
				self.lexer.next();
			} else {
				break;
			}
		}

		if !self.peek_type_is(TokenType::RightParen) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		if !self.peek_type_is(TokenType::Arrow) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		let ret_type = self.parse_type()?;
		let block = self.parse_block_statement()?;

		Ok(Expression::Function(ident, params, ret_type, Box::new(block)))
	}

	//
	//
	//
	fn parse_block_statement(&mut self) -> Result<Statement, Error> {
		if !self.peek_type_is(TokenType::LeftBrace) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		let mut block = Block::new();

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
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		Ok(Statement::Block(block))
	}

	//
	//
	//
	fn parse_call_expression(&mut self, identifier: Expression) -> Result<Expression, Error> {
		let mut args: Vec<Expression> = Vec::new();

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

		Ok(Expression::Call(Box::new(identifier), args))
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
			Expression::Identifier(
				Identifier::new(
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
			Expression::Literal(Literal::Int32($value))
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
		let expected = Statement::Let(Identifier::new("x".to_string(), Location::new(1, 5)),
										Type::Int32,
										Expression::Literal(Literal::Int32(5)));

		parser.parse();

		assert_eq!(parser.module.statements[0], expected);
	}

	// Test a return statement is correctly matched.
	#[test]
	fn test_return_statement() {
		let mut parser = Parser::new("return 5; return x;");
		let expected = vec![
			Statement::Return(Expression::Literal(Literal::Int32(5))),
			Statement::Return(Expression::Identifier(Identifier::new("x".to_string(), Location::new(1, 18)))),
		];

		parser.parse();

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(statement, &expected[i]);
		}
	}

	// Test prefix operators are correctly matched
	#[test]
	fn test_prefix_operators() {
		let mut parser = Parser::new("!x; -5;");
		let expected = vec![
			Statement::Expression(Expression::Prefix(
				Operator::Not,
				Box::new(Expression::Identifier(Identifier::new("x".to_string(), Location::new(1, 2))))
			)),
			Statement::Expression(Expression::Prefix(
				Operator::UnaryMinus,
				Box::new(Expression::Literal(Literal::Int32(5)))
			))
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
			Statement::Expression(Expression::Infix(
				Box::new(expr_ident!("x", 1, 1)),
				Operator::Plus,
				Box::new(expr_literal_i32!(5)),
			)),
			Statement::Expression(Expression::Infix(
				Box::new(expr_literal_i32!(10)),
				Operator::Multiply,
				Box::new(expr_literal_i32!(5)),
			)),
			Statement::Expression(Expression::Infix(
				Box::new(expr_ident!("x", 1, 16)),
				Operator::Divide,
				Box::new(expr_ident!("y", 1, 20)),
			)),
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
			Statement::Expression(
				Expression::Infix(
					Box::new(expr_literal_i32!(1)),
					Operator::Plus,
					Box::new(Expression::Infix(
						Box::new(expr_literal_i32!(2)),
						Operator::Multiply,
						Box::new(expr_literal_i32!(3)),
					)),
				)
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
			"fn square(n: i32) -> i32 {\nreturn (n * n);\n}"
		];

		parser.parse();

		println!("errors: {}", parser.errors.len());

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(format!("{}", statement), expected[i]);
		}
	}

	// Test function calls
	#[test]
	fn test_function_call() {
		let mut parser = Parser::new("square(5);");
		let expected = vec![
			"square(5);\n",
		];

		parser.parse();
		println!("errors: {}", parser.errors.len());

		for (i, statement) in parser.module.statements.iter().enumerate() {
			assert_eq!(format!("{}", statement), expected[i]);
		}
	}
}
