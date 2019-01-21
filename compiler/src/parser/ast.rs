//!
//!
//!


use lexer::token::{Location, Token, TokenType};


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
	pub statements: Vec<Statement>,
}


impl Module {
	///
	///
	///
	pub fn new() -> Module {
		Module {
			statements: Vec::new(),
		}
	}

	///
	///
	///
	pub fn push(&mut self, statement: Statement) {
		self.statements.push(statement);
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	Let(Identifier, Type, Expression),
	Return(Expression),
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	Literal(Literal),
	Identifier(Identifier),
}


///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal {
	Int32(i32),
	Float(f32),
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
	/// use compiler::parser::ast::Literal;
	/// use compiler::lexer::token::{Location, Token, TokenType};
	///
	/// let location = Location::new(5, 20);
	/// let token = Token::new(TokenType::Integer, "10".to_string(), location);
	/// let literal = Literal::from_token(token);
	///
	/// assert_eq!(literal, Literal::Int32(10));
	/// ```
	pub fn from_token(token: Token) -> Literal {
		match token.token_type {
			TokenType::Integer => Literal::Int32(token.value.unwrap().parse::<i32>().unwrap()),
			TokenType::Float   => Literal::Float(token.value.unwrap().parse::<f32>().unwrap()),
			TokenType::True    => Literal::Boolean(true),
			TokenType::False   => Literal::Boolean(false),

			// we shouldn't hit this, but just to be safe
			_ => panic!("Unexpected token type: {:?}", token.token_type)
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


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
	value: String,
	location: Location,
}


impl Identifier {
	///
	///
	///
	pub fn new(value: String, location: Location) -> Identifier {
		Identifier {
			value: value,
			location: location,
		}
	}
}
