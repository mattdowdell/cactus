//!
//!
//!

use lexer::token::{Token, TokenType};
use parser::error::Error;

macro_rules! error {
	($msg:expr) => (
		Error::new($msg, Location::end())
	);
	($msg:expr, $location:expr) => (
		Error::new($msg, $location)
	);
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
	pub modules: Vec<Module>,
}

impl Ast {
	///
	///
	///
	pub fn new() -> Ast {
		Ast {
			modules: Vec::new(),
		}
	}

	///
	///
	///
	pub fn push(&mut self, module: Module) {
		self.modules.push(module);
	}
}


/// A representation of a module in Cactus.
///
/// Modules are made up of 4 types of definition:
/// - Imports
/// - Structures
/// - Enumerations
/// - Functions
///
/// Imports are simply structures, enumerations and functions in another module.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
	pub definitions: Vec<Definition>,
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
			definitions: Vec::new(),
		}
	}

	/// Add a definition to the module.
	///
	/// # Example
	/// ```
	/// use cactus::parser::ast::{Module, Definition, Enum, Identifier};
	///
	/// let ident = Identifier::new("example".to_string());
	/// let field = Identifier::new("x".to_string());
	/// let enumeration = Enum::new(ident, vec![field]);
	/// let def = Definition::Enum(enumeration);
	/// let mut module = Module::new();
	///
	/// module.push(def);
	/// ```
	pub fn push(&mut self, definition: Definition) {
		self.definitions.push(definition);
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
	Import(Import),
	Struct(Struct),
	Enum(Enum),
	Function(Function),
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Import {
	pub path: Identifier,
}

impl Import {
	///
	///
	///
	pub fn new(path: Identifier) -> Import {
		Import {
			path: path,
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
	pub identifier: Identifier,
	pub fields: Vec<Parameter>,
}

impl Struct {
	///
	///
	///
	pub fn new(identifier: Identifier, fields: Vec<Parameter>) -> Struct {
		Struct {
			identifier: identifier,
			fields: fields,
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Enum {
	pub identifier: Identifier,
	pub variants: Vec<Identifier>,
}

impl Enum {
	///
	///
	///
	pub fn new(identifier: Identifier, variants: Vec<Identifier>) -> Enum {
		Enum {
			identifier: identifier,
			variants: variants,
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
	pub identifier: Identifier,
	pub arguments: Vec<Parameter>,
	pub return_type: Option<Type>,
	pub body: Block,
}

impl Function {
	///
	///
	///
	pub fn new(identifier: Identifier, arguments: Vec<Parameter>, return_type: Option<Type>, body: Block) -> Function {
		Function {
			identifier: identifier,
			arguments: arguments,
			return_type: return_type,
			body: body,
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
	pub name: String,
	address: usize,
	path: Vec<Identifier>,
}

impl Identifier {
	/// Create a new identifier.
	///
	/// # Example
	/// ```
	/// use cactus::parser::ast::Identifier;
	///
	/// Identifier::new("example".to_string());
	/// ```
	pub fn new(name: String) -> Identifier {
		Identifier {
			name: name,
			address: 0,
			path: Vec::new(),
		}
	}

	/// Set the import path for an identifier.
	///
	/// An identifier with an import path indicates that it has been imported from another
	/// module and is thus defined somewhere else.
	///
	/// # Example
	/// ```
	/// use cactus::parser::ast::Identifier;
	///
	/// let module_name = Identifier::new("std".to_string());
	/// let import_path = vec![module_name];
	/// let mut ident = Identifier::new("example".to_string());
	///
	/// ident.set_path(import_path);
	/// ```
	pub fn set_path(&mut self, path: Vec<Identifier>) {
		self.path = path;
	}

	/// Test if the identifier is local by checking if it has an import path set.
	///
	/// ```
	/// use cactus::parser::ast::Identifier;
	///
	/// let module_name = Identifier::new("std".to_string());
	/// let import_path = vec![module_name];
	///
	/// let mut ident = Identifier::new("example".to_string());
	/// assert!(ident.is_local());
	///
	/// ident.set_path(import_path);
	/// assert!(!ident.is_local());
	/// ```
	pub fn is_local(&self) -> bool {
		self.path.len() == 0
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

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
	// primitive types
	Int32,
	Uint32,
	Float,
	Bool,

	// other types
	Custom(Identifier),
}

impl Type {
	///
	///
	///
	pub fn from_token(token: Token) -> Result<Type, Error> {
		match token.token_type {
			TokenType::TypeInt32  => Ok(Type::Int32),
			TokenType::TypeUint32 => Ok(Type::Uint32),
			TokenType::TypeFloat  => Ok(Type::Float),
			TokenType::TypeBool   => Ok(Type::Bool),

			_ => Err(error!(
				format!("Unexpected token type: {:?}. Expected one of: i32, u32, f32 or bool.",
					token.token_type),
				token.location
			))
		}
	}

	///
	///
	///
	pub fn from_identifier(identifier: Identifier) -> Type {
		Type::Custom(identifier)
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
	pub statements: Vec<Statement>,
}

impl Block {
	///
	///
	///
	pub fn new() -> Block {
		Block {
			statements: Vec::new(),
		}
	}

	/// Add a statement to the block.
	///
	/// # Example
	/// ```
	/// use cactus::parser::ast::{Block, Statement, Expression, Identifier};
	///
	/// let ident = Identifier::new("example".to_string());
	/// let expr = Expression::Identifier(ident);
	/// let stmt = Statement::Return(expr);
	/// let mut block = Block::new();
	///
	/// block.push(stmt);
	/// ```
	pub fn push(&mut self, statement: Statement) {
		self.statements.push(statement);
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	Let(Let),
	Return(Expression),
	Expression(Expression),
	Loop(Block),
	If(If),
	Break,
	Continue,
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Let {
	pub identifier: Identifier,
	pub type_hint: Type,
	pub value: Expression,
}

impl Let {
	///
	///
	///
	pub fn new(identifier: Identifier, type_hint: Type, value: Expression) -> Let {
		Let {
			identifier: identifier,
			type_hint: type_hint,
			value: value,
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct If {
	pub condition: Expression,
	pub consequence: Block,
	pub other: Vec<(Expression, Block)>,
	pub alternative: Option<Block>,
}

impl If {
	///
	///
	///
	pub fn new(condition: Expression, consequence: Block, other: Vec<(Expression, Block)>, alternative: Option<Block>) -> If {
		If {
			condition: condition,
			consequence: consequence,
			other: other,
			alternative: alternative,
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
	Struct(Vec<StructField>),
	Prefix(Operator, Box<Expression>),
	Infix(Box<Expression>, Operator, Box<Expression>),
	Call(Identifier, Vec<Expression>)
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
	Integer(String),
	Float(String),
	Boolean(bool),
}

impl Literal {
	/// Convert a token to a literal.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
	/// use cactus::parser::ast::Literal;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::True, location);
	/// let literal = Literal::from_token(token).unwrap();
	///
	/// assert_eq!(literal, Literal::Boolean(true));
	/// ```
	pub fn from_token(token: Token) -> Result<Literal, Error> {
		match token.token_type {
			TokenType::Integer => Ok(Literal::Integer(token.value.unwrap().clone())),
			TokenType::Float   => Ok(Literal::Float(token.value.unwrap().clone())),
			TokenType::True    => Ok(Literal::Boolean(true)),
			TokenType::False   => Ok(Literal::Boolean(false)),

			_ => Err(error!(
				format!("Unexpected token type: {:?}. Expected one of: Integer, Float, True or False.",
					token.token_type),
				token.location
			))
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
	name: Identifier,
	value: Expression,
}

impl StructField {
	///
	///
	///
	pub fn new(name: Identifier, value: Expression) -> StructField {
		StructField {
			name: name,
			value: value,
		}
	}
}

///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
	// prefix operators
	Not,
	UnaryMinus,
	BitCompl,

	// infix operators
	Plus,
	Minus,
	Multiply,
	Divide,
	Modulo,
	BitAnd,
	BitOr,
	BitXor,
	BitLeftShift,
	BitRightShift,
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
	And,
	Or,
	Assign,
	PlusAssign,
	MinusAssign,
	MultiplyAssign,
	DivideAssign,
	ModuloAssign,
	BitAndAssign,
	BitOrAssign,
	BitXorAssign,
	BitLeftShiftAssign,
	BitRightShiftAssign,
}

impl Operator {
	/// Convert a token to a prefix operator.
	///
	/// Only accepts tokens that are prefix operators. If an invalid token s converted an error
	/// will be returned.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
	/// use cactus::parser::ast::Operator;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::Not, location);
	/// let operator = Operator::new_prefix(token).unwrap();
	///
	/// assert_eq!(operator, Operator::Not);
	/// ```
	pub fn new_prefix(token: Token) -> Result<Operator, Error> {
		match token.token_type {
			TokenType::Not => Ok(Operator::Not),
			TokenType::Minus => Ok(Operator::UnaryMinus),
			TokenType::BitCompl => Ok(Operator::BitCompl),

			_ => Err(error!(
				format!("Unable to convert token type to prefix operator: {:?}.",
					token.token_type),
				token.location
			))
		}
	}

	/// Convert a token to an infix operator.
	///
	/// Only accepts tokens that are infix operators. If an invalid token is given an error will be
	/// returned.
	///
	/// # Example
	/// ```
	/// use cactus::lexer::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
	/// use cactus::parser::ast::Operator;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::Plus, location);
	/// let operator = Operator::new_infix(token).unwrap();
	///
	/// assert_eq!(operator, Operator::Plus);
	/// ```
	pub fn new_infix(token: Token) -> Result<Operator, Error> {
		match token.token_type {
			TokenType::Plus               => Ok(Operator::Plus),
			TokenType::Minus              => Ok(Operator::Minus),
			TokenType::Multiply           => Ok(Operator::Multiply),
			TokenType::Divide             => Ok(Operator::Divide),
			TokenType::Modulo             => Ok(Operator::Modulo),
			TokenType::BitAnd             => Ok(Operator::BitAnd),
			TokenType::BitOr              => Ok(Operator::BitOr),
			TokenType::BitXor             => Ok(Operator::BitXor),
			TokenType::BitLeftShift       => Ok(Operator::BitLeftShift),
			TokenType::BitRightShift      => Ok(Operator::BitRightShift),
			TokenType::Equal              => Ok(Operator::Equal),
			TokenType::NotEqual           => Ok(Operator::NotEqual),
			TokenType::LessThan           => Ok(Operator::LessThan),
			TokenType::LessThanOrEqual    => Ok(Operator::LessThanOrEqual),
			TokenType::GreaterThan        => Ok(Operator::GreaterThan),
			TokenType::GreaterThanOrEqual => Ok(Operator::GreaterThanOrEqual),
			TokenType::And                => Ok(Operator::And),
			TokenType::Or                 => Ok(Operator::Or),
			TokenType::Assign              => Ok(Operator::Assign),
			TokenType::PlusAssign          => Ok(Operator::PlusAssign),
			TokenType::MinusAssign         => Ok(Operator::MinusAssign),
			TokenType::MultiplyAssign      => Ok(Operator::MultiplyAssign),
			TokenType::DivideAssign        => Ok(Operator::DivideAssign),
			TokenType::ModuloAssign        => Ok(Operator::ModuloAssign),
			TokenType::BitAndAssign        => Ok(Operator::BitAndAssign),
			TokenType::BitOrAssign         => Ok(Operator::BitOrAssign),
			TokenType::BitXorAssign        => Ok(Operator::BitXorAssign),
			TokenType::BitLeftShiftAssign  => Ok(Operator::BitLeftShiftAssign),
			TokenType::BitRightShiftAssign => Ok(Operator::BitRightShiftAssign),

			_ => Err(error!(
				format!("Unable to convert token type to prefix operator: {:?}.",
					token.token_type),
				token.location
			))
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use lexer::location::Location;

	// An instance of `Location` used as a placeholder as tokens aren't responsible for
	// discovering where they are relative to an input string.
	//
	// Tests for checking the location of tokens are correct can be found in the lexer tests.
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
	fn test_operator_new_prefix() {
		let data = vec![
			(token!(TokenType::Not), Operator::Not),
			(token!(TokenType::Minus), Operator::UnaryMinus),
			(token!(TokenType::BitCompl), Operator::BitCompl),
		];

		for (input, expected) in data.iter() {
			let operator = Operator::new_prefix(input.clone());

			assert!(operator.is_ok());
			assert_eq!(&operator.unwrap(), expected);
		}
	}

	#[test]
	#[ignore]
	fn test_operator_new_prefix_error() {
		unimplemented!()
	}

	#[test]
	fn test_operator_new_infix() {
		let data = vec![
			(token!(TokenType::Plus), Operator::Plus),
			(token!(TokenType::Minus), Operator::Minus),
			(token!(TokenType::Multiply), Operator::Multiply),
			(token!(TokenType::Divide), Operator::Divide),
			(token!(TokenType::Modulo), Operator::Modulo),
			(token!(TokenType::BitAnd), Operator::BitAnd),
			(token!(TokenType::BitOr), Operator::BitOr),
			(token!(TokenType::BitXor), Operator::BitXor),
			(token!(TokenType::BitLeftShift), Operator::BitLeftShift),
			(token!(TokenType::BitRightShift), Operator::BitRightShift),
			(token!(TokenType::Equal), Operator::Equal),
			(token!(TokenType::NotEqual), Operator::NotEqual),
			(token!(TokenType::LessThan), Operator::LessThan),
			(token!(TokenType::LessThanOrEqual), Operator::LessThanOrEqual),
			(token!(TokenType::GreaterThan), Operator::GreaterThan),
			(token!(TokenType::GreaterThanOrEqual), Operator::GreaterThanOrEqual),
			(token!(TokenType::And), Operator::And),
			(token!(TokenType::Or), Operator::Or),
			(token!(TokenType::Assign), Operator::Assign),
			(token!(TokenType::PlusAssign), Operator::PlusAssign),
			(token!(TokenType::MinusAssign), Operator::MinusAssign),
			(token!(TokenType::MultiplyAssign), Operator::MultiplyAssign),
			(token!(TokenType::DivideAssign), Operator::DivideAssign),
			(token!(TokenType::ModuloAssign), Operator::ModuloAssign),
			(token!(TokenType::BitAndAssign), Operator::BitAndAssign),
			(token!(TokenType::BitOrAssign), Operator::BitOrAssign),
			(token!(TokenType::BitXorAssign), Operator::BitXorAssign),
			(token!(TokenType::BitLeftShiftAssign), Operator::BitLeftShiftAssign),
			(token!(TokenType::BitRightShiftAssign), Operator::BitRightShiftAssign),
		];

		for (input, expected) in data.iter() {
			let operator = Operator::new_infix(input.clone());

			assert!(operator.is_ok());
			assert_eq!(&operator.unwrap(), expected);
		}
	}

	#[test]
	#[ignore]
	fn test_operator_new_infix_error() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_literal_from_token() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_expression() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_statement() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_block() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_type_from_token() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_type_from_token_error() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_type_from_ident() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_parameter() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_identifier() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_definition() {
		unimplemented!()
	}

	#[test]
	#[ignore]
	fn test_module() {
		unimplemented!()
	}
}





/*
impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Statement::Let(ident, typehint, expr) => write!(f, "let {}: {} = {};", ident, typehint, expr),
			Statement::Return(expr)               => write!(f, "return {};", expr),
			Statement::Expression(expr)           => write!(f, "{}", expr),
			Statement::Block(block)               => write!(f, "{{\n{}}}", block),
			Statement::Function(ident, params, ret_type, block) => {
				let mut ret = format!("fn {}({})", ident, join(params, ", "));

				match ret_type {
					Some(ret_type) => {
						ret = format!("{} -> {}", ret, ret_type)
					},
					None => {}
				};

				write!(f, "{} {}", ret, block)
			},
			Statement::If(cond, consq) => {
				write!(f, "if {} {}", cond, consq)
			},
			Statement::IfElse(cond, consq, alt) => {
				write!(f, "if {} {} else {}", cond, consq, alt)
			},
			Statement::IfElif(cond, consq, other) => {
				write!(f, "if {} {} el{}", cond, consq, other)
			},
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
	Call(Identifier, Vec<Expression>),
}


impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expression::Literal(value)         => write!(f, "{}", value),
			Expression::Identifier(value)      => write!(f, "{}", value),
			Expression::Prefix(op, right)      => write!(f, "{}{}", op, right),
			Expression::Infix(left, op, right) => write!(f, "({} {} {})", left, op, right),
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


impl Type {
	///
	///
	///
	pub fn size_of(&self) -> usize {
		match self {
			Type::Int32 => 4,
			Type::Float => 4,
			Type::Bool  => 1,
		}
	}
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
	fn test_type_size_of() {
		assert_eq!(Type::Int32.size_of(), 4);
		assert_eq!(Type::Float.size_of(), 4);
		assert_eq!(Type::Bool.size_of(), 1);
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
*/
