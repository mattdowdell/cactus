//!
//!
//!

extern crate itertools;

use std::fmt;
use itertools::join;
use lexer::token::{Location, Token, TokenType};
use parser::error::{Error, ErrorCode};


/// A representation of a module in Cactus.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
	pub statements: Vec<Statement>,
}


impl Module {
	/// Create a new instance of module.
	///
	/// # Example
	/// ```
	/// use cactus::parser::ast::Module;
	///
	/// Module::new();
	/// ```
	pub fn new() -> Module {
		Module {
			statements: Vec::new(),
		}
	}

	/// Add a statement to a module.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::{Location, Token};
	/// use cactus::parser::ast::{Module, Statement, Expression, Literal};
	///
	/// let mut module = Module::new();
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_ident("true".to_string(), location);
	/// let literal = Literal::from_token(token).unwrap();
	/// let expr = Expression::Literal(literal);
	/// let statement = Statement::Return(expr);
	///
	/// module.push(statement);
	///
	/// assert_eq!(module.statements.len(), 1);
	/// ```
	pub fn push(&mut self, statement: Statement) {
		self.statements.push(statement);
	}
}


impl fmt::Display for Module {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut ret = String::new();

		for statement in &self.statements {
			ret = format!("{}{}\n", ret, statement);
		}

		write!(f, "{}", ret)
	}
}

pub type Block = Module;


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	Let(Identifier, Type, Expression),
	Return(Expression),
	Expression(Expression),
	Block(Block),
}


impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Statement::Let(ident, typehint, expr) => write!(f, "let {}: {} = {};", ident, typehint, expr),
			Statement::Return(expr)               => write!(f, "return {};", expr),
			Statement::Expression(expr)           => write!(f, "{}", expr),
			Statement::Block(block)               => write!(f, "{{\n{}}}", block),
		}
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	Literal(Literal),
	Identifier(Identifier),
	Prefix(Operator, Box<Expression>),
	Infix(Box<Expression>, Operator, Box<Expression>),
	Function(Identifier, Vec<Parameter>, Option<Type>, Box<Statement>),
	Call(Identifier, Vec<Expression>),
}


impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expression::Literal(value)         => write!(f, "{}", value),
			Expression::Identifier(value)      => write!(f, "{}", value),
			Expression::Prefix(op, right)      => write!(f, "{}{}", op, right),
			Expression::Infix(left, op, right) => write!(f, "({} {} {})", left, op, right),
			Expression::Function(ident, params, ret_type, block) => {
				match ret_type {
					Some(ret) => {
						write!(f, "fn {}({}) -> {} {}",
							ident,
							join(params, ", "),
							ret,
							block)
					},
					None => {
						write!(f, "fn {}({}) {}",
							ident,
							join(params, ", "),
							block)
					}
				}

			},
			Expression::Call(ident, exprs) => {
				write!(f, "{}({})",
					ident,
					join(exprs, ", "))
			}
		}
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
	Int32(String),
	Float(String),
	Boolean(bool),
}


impl Literal {
	/// Convert a token to a literal.
	///
	/// Only supports tokens that map directly to literals. If anything else is passed in, then a
	/// panic will occur.
	///
	/// # Example
	/// ```
	/// use cactus::parser::ast::Literal;
	/// use cactus::lexer::token::{Location, Token, TokenType};
	///
	/// let location = Location::new(5, 20);
	/// let token = Token::new(TokenType::Integer, "10".to_string(), location);
	/// let literal = Literal::from_token(token);
	///
	/// assert!(literal.is_ok());
	/// assert_eq!(literal.unwrap(), Literal::Int32("10".to_string()));
	/// ```
	pub fn from_token(token: Token) -> Result<Literal, Error> {
		match token.token_type {
			TokenType::Integer => Ok(Literal::Int32(token.value.unwrap())),
			TokenType::Float   => Ok(Literal::Float(token.value.unwrap())),
			TokenType::True    => Ok(Literal::Boolean(true)),
			TokenType::False   => Ok(Literal::Boolean(false)),

			// we shouldn't hit this, but just to be safe
			_ => Err(Error::from_token(ErrorCode::E0001, Some(token)))
		}
	}
}


impl fmt::Display for Literal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Literal::Int32(value) => write!(f, "{}", value),
			Literal::Float(value) => write!(f, "{}", value),
			Literal::Boolean(value)  => write!(f, "{}", value),
		}
	}
}


///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type {
	Int32,
	Float,
	Bool,
}


impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Type::Int32 => write!(f, "i32"),
			Type::Float => write!(f, "f32"),
			Type::Bool  => write!(f, "bool"),
		}
	}
}


/// A representation of an Identifier.
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
	pub value: String,
	pub location: Location,
}


impl Identifier {
	/// Create a new instance of an identifier.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::Location;
	/// use cactus::parser::ast::Identifier;
	///
	/// let location = Location::new(1, 0);
	/// let ident = Identifier::new("foo".to_string(), location);
	///
	/// assert_eq!(ident.value, "foo");
	/// assert_eq!(ident.location.line, 1);
	/// assert_eq!(ident.location.column, 0);
	/// ```
	pub fn new(value: String, location: Location) -> Identifier {
		Identifier {
			value: value,
			location: location,
		}
	}

	/// Create a new identifier from an existing identifier token.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::{Location, Token};
	/// use cactus::parser::ast::Identifier;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_ident("foo".to_string(), location);
	///
	/// let ident = Identifier::from_token(token);
	/// assert!(ident.is_ok());
	///
	/// let ident = ident.unwrap();
	/// assert_eq!(ident.value, "foo");
	/// assert_eq!(ident.location.line, 1);
	/// assert_eq!(ident.location.column, 0);
	/// ```
	pub fn from_token(token: Token) -> Result<Identifier, Error> {
		if token.token_type != TokenType::Identifier {
			Err(Error::from_token(ErrorCode::E0002, Some(token)))
		} else {
			Ok(Identifier::new(token.value.unwrap(), token.location))
		}
	}
}


impl fmt::Display for Identifier {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}


///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
	// prefix operators
	UnaryMinus,
	Not,
	BitCompl,

	// infix operators
	Plus,
	Minus,
	Multiply,
	Divide,
}


impl Operator {
	/// Convert a token to an operator for a prefix expression.
	///
	/// If the token type is not a valid prefix operator then a panic will occur.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::{Location, Token, TokenType};
	/// use cactus::parser::ast::Operator;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::Minus, location);
	/// let operator = Operator::from_prefix_token(token);
	///
	/// assert!(operator.is_ok());
	/// assert_eq!(operator.unwrap(), Operator::UnaryMinus);
	/// ```
	pub fn from_prefix_token(token: Token) -> Result<Operator, Error> {
		match token.token_type {
			TokenType::Minus    => Ok(Operator::UnaryMinus),
			TokenType::Bang     => Ok(Operator::Not),
			TokenType::BitCompl => Ok(Operator::BitCompl),

			_ => Err(Error::from_token(ErrorCode::E0003, Some(token)))
		}
	}

	/// Convert a token to an operator for a infix expression.
	///
	/// If the token type is not a valid infix operator then a panic will occur.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::token::{Location, Token, TokenType};
	/// use cactus::parser::ast::Operator;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::Minus, location);
	/// let operator = Operator::from_infix_token(token);
	///
	/// assert!(operator.is_ok());
	/// assert_eq!(operator.unwrap(), Operator::Minus);
	/// ```
	pub fn from_infix_token(token: Token) -> Result<Operator, Error> {
		match token.token_type {
			TokenType::Plus     => Ok(Operator::Plus),
			TokenType::Minus    => Ok(Operator::Minus),
			TokenType::Multiply => Ok(Operator::Multiply),
			TokenType::Divide   => Ok(Operator::Divide),

			_ => Err(Error::from_token(ErrorCode::E0004, Some(token))),
		}
	}
}


impl fmt::Display for Operator {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			// prefix operators
			Operator::UnaryMinus => write!(f, "-"),
			Operator::Not        => write!(f, "!"),
			Operator::BitCompl   => write!(f, "~"),

			// infix operators
			Operator::Plus     => write!(f, "+"),
			Operator::Minus    => write!(f, "-"),
			Operator::Multiply => write!(f, "*"),
			Operator::Divide   => write!(f, "/"),

		}
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
	pub identifier: Identifier,
	pub type_hint: Type,
}


impl Parameter {
	///
	///
	///
	pub fn new(identifier: Identifier, type_hint: Type) -> Parameter {
		Parameter {
			identifier: identifier,
			type_hint: type_hint,
		}
	}
}


impl fmt::Display for Parameter {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}: {}", self.identifier, self.type_hint)
	}
}



#[cfg(test)]
mod test {
	use super::*;

	const LOCATION: Location = Location {
		line: 1,
		column: 1,
	};

	// Helper macro for creating a new token.
	macro_rules! token {
		($tt:expr, $value:expr) => (
			Token::new($tt, $value.to_string(), LOCATION);
		);
		($tt:expr) => (
			Token::from_type($tt, LOCATION);
		)
	}

	#[test]
	fn test_literal_from_token() {
		let data = vec![
			(token!(TokenType::Integer, "10"), Literal::Int32("10".to_string())),
			(token!(TokenType::Float, "10.0"), Literal::Float("10.0".to_string())),
			(token!(TokenType::True), Literal::Boolean(true)),
			(token!(TokenType::False), Literal::Boolean(false)),
		];

		for (input, expected) in data.iter() {
			let literal = Literal::from_token(input.clone());

			assert!(literal.is_ok());
			assert_eq!(&literal.unwrap(), expected);
		}
	}

	#[test]
	fn test_literal_from_token_error() {
		let data = vec![
			token!(TokenType::Illegal),
			token!(TokenType::Let),
			token!(TokenType::Function),
			token!(TokenType::Plus),
		];

		for input in data.iter() {
			let literal = Literal::from_token(input.clone());
			assert!(literal.is_err());

			let error = literal.err().unwrap();
			assert_eq!(error.code, ErrorCode::E0001);
		}
	}

	#[test]
	fn test_identifier_from_token() {
		let token = token!(TokenType::Identifier, "foo");
		let ident = Identifier::from_token(token);
		let expected = Identifier::new("foo".to_string(), LOCATION);

		assert!(ident.is_ok());
		assert_eq!(ident.unwrap(), expected);
	}

	#[test]
	fn test_identifier_from_token_error() {
		let token = token!(TokenType::Function);
		let ident = Identifier::from_token(token);

		assert!(ident.is_err());

		let error = ident.err().unwrap();
		assert_eq!(error.code, ErrorCode::E0002);
	}

	#[test]
	fn test_operator_from_prefix_token() {
		let data = vec![
			(token!(TokenType::Minus), Operator::UnaryMinus),
			(token!(TokenType::Bang), Operator::Not),
			(token!(TokenType::BitCompl), Operator::BitCompl),
		];

		for (input, expected) in data.iter() {
			let operator = Operator::from_prefix_token(input.clone());

			assert!(operator.is_ok());
			assert_eq!(&operator.unwrap(), expected);
		}
	}

	#[test]
	fn test_operator_from_prefix_token_error() {
		let token = token!(TokenType::Function);
		let operator = Operator::from_prefix_token(token);

		assert!(operator.is_err());

		let error = operator.err().unwrap();
		assert_eq!(error.code, ErrorCode::E0003);
	}

	#[test]
	fn test_operator_from_infix_token() {
		let data = vec![
			(token!(TokenType::Plus), Operator::Plus),
			(token!(TokenType::Minus), Operator::Minus),
			(token!(TokenType::Multiply), Operator::Multiply),
			(token!(TokenType::Divide), Operator::Divide),
		];

		for (input, expected) in data.iter() {
			let operator = Operator::from_infix_token(input.clone());

			assert!(operator.is_ok());
			assert_eq!(&operator.unwrap(), expected);
		}
	}

	#[test]
	fn test_operator_from_infix_token_error() {
		let token = token!(TokenType::Function);
		let operator = Operator::from_infix_token(token);

		assert!(operator.is_err());

		let error = operator.err().unwrap();
		assert_eq!(error.code, ErrorCode::E0004);
	}
}
