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
	/// use compiler::lexer::token::Location;
	/// use compiler::parser::ast::Identifier;
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
	/// use compiler::lexer::token::{Location, Token};
	/// use compiler::parser::ast::Identifier;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_ident("foo".to_string(), location);
	/// let ident = Identifier::from_token(token);
	///
	/// assert_eq!(ident.value, "foo");
	/// assert_eq!(ident.location.line, 1);
	/// assert_eq!(ident.location.column, 0);
	/// ```
	pub fn from_token(token: Token) -> Identifier {
		if token.token_type != TokenType::Identifier {
			panic!("Unexpected token type: {:?}, expected: Identifier (L{}:{})",
				token.token_type, token.location.line, token.location.column);
		}

		Identifier::new(token.value.unwrap(), token.location)
	}
}

