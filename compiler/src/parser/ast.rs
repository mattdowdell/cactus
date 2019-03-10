//!
//!
//!

use std::fmt;

use crate::{
	location::Location,
	error::{
		CompilationError,
		ErrorCode,
		ErrorType,
	},
	lexer::token::{
		Token,
		TokenType
	},
	analyser::{
		symbol::SymbolType,
		type_checker::TypeHint,
	},
};


/// A representation of the abstract syntax tree for Cactus.
///
/// This contains modules produced by successful runs of the parser.
#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
	pub modules: Vec<Module>,
}

impl Ast {
	/// Create a new instance of `Ast`.
	pub fn new() -> Ast {
		Ast {
			modules: Vec::new(),
		}
	}

	/// Add a module to the `Ast` instance.
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

/// The possible definitions in Cactus.
#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
	Import(Import),
	Struct(Struct),
	Enum(Enum),
	Function(Function),
}

/// A representation of an import definition in Cactus.
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

/// A representation of a structure definition in Cactus.
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

/// A representation of an enumeration definition in Cactus.
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

/// A representation of a function definition in cactus.
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

/// A representation of an identifier in Cactus.
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
	pub name: String,
	offset: usize,
	symbol_type: SymbolType,
	type_hint: Type,
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
			offset: 0,
			symbol_type: SymbolType::Unknown,
			type_hint: Type::Unknown,
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

	/// Set the symbol type for the identifier.
	///
	///
	pub fn set_symbol_type(&mut self, symbol_type: SymbolType) {
		self.symbol_type = symbol_type;
	}

	/// Set the offset for the identifier.
	///
	/// The offset is the relative to the symbols for the scope. Function arguments have one set
	/// of offsets, while local variables have another.
	pub fn set_offset(&mut self, offset: usize) {
		self.offset = offset;
	}

}

/// A representation of a parameter in Cactus.
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

/// A representation of a type in Cactus.
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
	Custom(Box<Identifier>),

	// default for identifiers
	Unknown,
}

impl Type {
	///
	///
	///
	pub fn from_token(token: Token) -> Result<Type, CompilationError> {
		match token.token_type {
			TokenType::TypeInt32  => Ok(Type::Int32),
			TokenType::TypeUint32 => Ok(Type::Uint32),
			TokenType::TypeFloat  => Ok(Type::Float),
			TokenType::TypeBool   => Ok(Type::Bool),

			_ => Err(internal_error!(
				ErrorCode::E1004,
				token.location,
				"Unexpected token type: {:?}. Expected one of: i32, u32, f32 or bool",
				token.token_type
			))
		}
	}

	///
	///
	///
	pub fn from_identifier(identifier: Identifier) -> Type {
		Type::Custom(Box::new(identifier))
	}
}

/// A representation of a block in Cactus.
///
/// Block are simply multiple statements wrapped in braces, e.g. `{ <statement> }`.
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

/// Possible statements in Cactus.
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

/// A representation of a let statement in Cactus.
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
	/// use cactus::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
	/// use cactus::parser::ast::Literal;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::True, location);
	/// let literal = Literal::from_token(token).unwrap();
	///
	/// assert_eq!(literal, Literal::Boolean(true));
	/// ```
	pub fn from_token(token: Token) -> Result<Literal, CompilationError> {
		match token.token_type {
			TokenType::Integer => Ok(Literal::Integer(token.value.unwrap().clone())),
			TokenType::Float   => Ok(Literal::Float(token.value.unwrap().clone())),
			TokenType::True    => Ok(Literal::Boolean(true)),
			TokenType::False   => Ok(Literal::Boolean(false)),

			_ => Err(internal_error!(
				ErrorCode::E1000,
				token.location,
				"Unexpected token type: {:?}. Expected one of: Integer, Float, True or False.",
				token.token_type
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

	// assignment operators
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
	/// use cactus::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
	/// use cactus::parser::ast::Operator;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::Not, location);
	/// let operator = Operator::new_prefix(token).unwrap();
	///
	/// assert_eq!(operator, Operator::Not);
	/// ```
	pub fn new_prefix(token: Token) -> Result<Operator, CompilationError> {
		match token.token_type {
			TokenType::Not => Ok(Operator::Not),
			TokenType::Minus => Ok(Operator::UnaryMinus),
			TokenType::BitCompl => Ok(Operator::BitCompl),

			_ => Err(syntax_error!(
				ErrorCode::E0003,
				token.location,
				"Unable to convert token type to prefix operator: {:?}",
				token.token_type
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
	/// use cactus::location::Location;
	/// use cactus::lexer::token::{Token, TokenType};
	/// use cactus::parser::ast::Operator;
	///
	/// let location = Location::new(1, 0);
	/// let token = Token::from_type(TokenType::Plus, location);
	/// let operator = Operator::new_infix(token).unwrap();
	///
	/// assert_eq!(operator, Operator::Plus);
	/// ```
	pub fn new_infix(token: Token) -> Result<Operator, CompilationError> {
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

			_ => Err(syntax_error!(
				ErrorCode::E0004,
				token.location,
				"Unable to convert token type to infix operator: {:?}",
				token.token_type
			))
		}
	}

	///
	///
	///
	pub fn is_assignment(&self) -> bool {
		match self {
			Operator::Assign
			| Operator::PlusAssign
			| Operator::MinusAssign
			| Operator::MultiplyAssign
			| Operator::DivideAssign
			| Operator::ModuloAssign
			| Operator::BitAndAssign
			| Operator::BitOrAssign
			| Operator::BitXorAssign
			| Operator::BitLeftShiftAssign
			| Operator::BitRightShiftAssign => true,
			_ => false
		}
	}

	///
	///
	///
	pub fn assignment_to_infix(&self) -> Result<Operator, CompilationError> {
		match self {
			Operator::PlusAssign          => Ok(Operator::Plus),
			Operator::MinusAssign         => Ok(Operator::Minus),
			Operator::MultiplyAssign      => Ok(Operator::Multiply),
			Operator::DivideAssign        => Ok(Operator::Divide),
			Operator::ModuloAssign        => Ok(Operator::Modulo),
			Operator::BitAndAssign        => Ok(Operator::BitAnd),
			Operator::BitOrAssign         => Ok(Operator::BitOr),
			Operator::BitXorAssign        => Ok(Operator::BitXor),
			Operator::BitLeftShiftAssign  => Ok(Operator::BitLeftShift),
			Operator::BitRightShiftAssign => Ok(Operator::BitRightShift),
			_ => Err(internal_error!(
				ErrorCode::E1002,
				Location::end(),
				"Unable to convert non-assignment operator to infix operator: {}, {:?}",
				self,
				self
			)),
		}
	}

	///
	///
	///
	pub fn allow_prefix(&self, right_type: TypeHint) -> Result<TypeHint, CompilationError> {
		match right_type {
			TypeHint::Int32
			| TypeHint::Uint32 => {
				match self {
					Operator::UnaryMinus
					| Operator::BitCompl => Ok(right_type),
					_ => Err(type_error!(
						ErrorCode::E0202,
						Location::end(),
						"Type {:?} cannot be used with prefix operator: {} ({:?})",
						right_type,
						self,
						self
					)),
				}
			},
			TypeHint::Bool => {
				match self {
					Operator::Not => Ok(right_type),
					_ => Err(type_error!(
						ErrorCode::E0202,
						Location::end(),
						"Type {:?} cannot be used with prefix operator: {} ({:?})",
						right_type,
						self,
						self
					)),
				}
			},
			TypeHint::Float => {
				match self {
					Operator::UnaryMinus => Ok(right_type),
					_ => Err(type_error!(
						ErrorCode::E0202,
						Location::end(),
						"Type {:?} cannot be used with prefix operator: {} ({:?})",
						right_type,
						self,
						self
					)),
				}
			},
			_ => {
				Err(internal_error!(
					ErrorCode::E1001,
					Location::end(),
					"Unexpected type for prefix operator check: {:?}",
					right_type
				))
			}
		}
	}

	///
	///
	///
	pub fn allow_infix(&self, left_type: TypeHint, right_type: TypeHint) -> Result<TypeHint, CompilationError> {
		if left_type != right_type {
			Err(type_error!(
				ErrorCode::E0203,
				Location::end(),
				"Type of left hand side ({:?}) does not match type of right hand side ({:?})",
				left_type,
				right_type
			))
		} else {
			match left_type {
				TypeHint::Int32
				| TypeHint::Uint32 => {
					match self {
						Operator::Plus
						| Operator::Minus
						| Operator::Multiply
						| Operator::Divide
						| Operator::Modulo
						| Operator::BitAnd
						| Operator::BitOr
						| Operator::BitXor
						| Operator::BitLeftShift
						| Operator::BitRightShift
						| Operator::Assign => Ok(left_type),

						Operator::Equal
						| Operator::NotEqual
						| Operator::LessThan
						| Operator::LessThanOrEqual
						| Operator::GreaterThan
						| Operator::GreaterThanOrEqual => Ok(TypeHint::Bool),

						_ => Err(type_error!(
							ErrorCode::E0204,
							Location::end(),
							"Type {:?} cannot be used with infix operator: {} ({:?})",
							right_type,
							self,
							self
						)),
					}
				},
				TypeHint::Bool => {
					match self {
						Operator::Equal
						| Operator::NotEqual
						| Operator::And
						| Operator::Or
						| Operator::Assign => Ok(left_type),

						_ => Err(type_error!(
							ErrorCode::E0204,
							Location::end(),
							"Type {:?} cannot be used with infix operator: {} ({:?})",
							right_type,
							self,
							self
						)),
					}
				},
				TypeHint::Float => {
					match self {
						Operator::Plus
						| Operator::Minus
						| Operator::Multiply
						| Operator::Divide
						| Operator::Modulo
						| Operator::Assign => Ok(left_type),

						Operator::LessThan
						| Operator::LessThanOrEqual
						| Operator::GreaterThan
						| Operator::GreaterThanOrEqual => Ok(TypeHint::Bool),

						_ => Err(type_error!(
							ErrorCode::E0204,
							Location::end(),
							"Type {:?} cannot be used with infix operator: {} ({:?})",
							right_type,
							self,
							self
						)),
					}
				},
				_ => {
					Err(internal_error!(
						ErrorCode::E0204,
						Location::end(),
						"Unexpected type for infix operator check: {:?}",
						right_type
					))
				}
			}
		}
	}
}

impl fmt::Display for Operator {
	//
	//
	//
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			// prefix operators
			Operator::Not        => write!(f, "not"),
			Operator::UnaryMinus => write!(f, "-"),
			Operator::BitCompl   => write!(f, "~"),

			// infix operators
			Operator::Plus               => write!(f, "+"),
			Operator::Minus              => write!(f, "-"),
			Operator::Multiply           => write!(f, "*"),
			Operator::Divide             => write!(f, "/"),
			Operator::Modulo             => write!(f, "%"),
			Operator::BitAnd             => write!(f, "&"),
			Operator::BitOr              => write!(f, "|"),
			Operator::BitXor             => write!(f, "^"),
			Operator::BitLeftShift       => write!(f, "<<"),
			Operator::BitRightShift      => write!(f, ">>"),
			Operator::Equal              => write!(f, "=="),
			Operator::NotEqual           => write!(f, "!="),
			Operator::LessThan           => write!(f, "<"),
			Operator::LessThanOrEqual    => write!(f, "<="),
			Operator::GreaterThan        => write!(f, ">"),
			Operator::GreaterThanOrEqual => write!(f, ">="),
			Operator::And                => write!(f, "and"),
			Operator::Or                 => write!(f, "or"),

			// assignment operators
			Operator::Assign              => write!(f, "="),
			Operator::PlusAssign          => write!(f, "+="),
			Operator::MinusAssign         => write!(f, "-="),
			Operator::MultiplyAssign      => write!(f, "*="),
			Operator::DivideAssign        => write!(f, "/="),
			Operator::ModuloAssign        => write!(f, "%="),
			Operator::BitAndAssign        => write!(f, "&="),
			Operator::BitOrAssign         => write!(f, "|="),
			Operator::BitXorAssign        => write!(f, "^="),
			Operator::BitLeftShiftAssign  => write!(f, "<<="),
			Operator::BitRightShiftAssign => write!(f, ">>="),
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::location::Location;

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
