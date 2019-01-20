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
	},
	error::{
		Error,
		ErrorCode
	}
};

/// A parser for the high-level language.
pub struct Parser<'a> {
	errors: Vec<Error>,
	lexer: Peekable<Lexer<'a>>,
	pub module: Module,
}


impl<'a> Parser<'a> {
	/// Create a new instance of `Parser`.
	pub fn new(input: &'a str) -> Parser<'a> {
		Parser {
			errors: Vec::new(),
			lexer: Lexer::new(input).peekable(),
			module: Module::new(),
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
			if self.lexer.peek().is_none() {
				break;
			}

			let token = self.lexer.next().unwrap();
			let statement = match token.token_type {
				TokenType::Let => self.parse_let_statement(),
				//TokenType::Return => {},

				// TODO: handle this better
				_ => panic!("Unexpected token: {:?}", token),
			};

			match statement {
				Ok(statement) => self.module.push(statement),
				Err(error) => self.errors.push(error),
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
	fn parse_let_statement(&mut self) -> Result<Statement, Error> {
		let ident = self.parse_identifier()?;

		if !self.peek_type_is(TokenType::Colon) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		let type_hint = self.parse_type()?;

		if !self.peek_type_is(TokenType::Assign) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		let expression = self.parse_expression()?;

		if !self.peek_type_is(TokenType::Semicolon) {
			let token = self.lexer.next().unwrap_or(Token::eof());
			return Err(Error::new(ErrorCode::E0001, token.location));
		} else {
			self.lexer.next();
		}

		Ok(Statement::Let(ident, type_hint, expression))
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
	fn parse_expression(&mut self) -> Result<Expression, Error> {
		let token = self.lexer.next().unwrap_or(Token::eof());

		match token.token_type {
			TokenType::Integer
			| TokenType::Float
			| TokenType::True
			| TokenType::False => Ok(Expression::Literal(Literal::from_token(token))),

			_ => Err(Error::new(ErrorCode::E0001, token.location))
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;

	// Test illegal characters are correctly matched.
	#[test]
	fn test_let_statement() {
		let mut parser = Parser::new("let x: i32 = 5;");
		let expected = Statement::Let(Identifier::new("x".to_string(), Location::new(1, 5)),
			                          Type::Int32,
			                          Expression::Literal(Literal::Int32(5)));

		parser.parse();

		assert_eq!(parser.module.statements[0], expected);
	}
}
