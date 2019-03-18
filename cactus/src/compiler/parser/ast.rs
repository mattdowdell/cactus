//! Data structures used to create the abstract syntax tree (AST) of a Cactus program.
//!
//! The AST is generated by the parser by taking in tokens from the lexer and turning them into a
//! tree like structure that represents the behaviour and structure of the input program.

use std::fmt;

use crate::error::{CompilationError, ErrorCode, internal_error};
use crate::location::Location;
use crate::compiler::lexer::{Token, TokenType};
use crate::compiler::analyser::SymbolType;

/// Common methods for all AST nodes.
pub trait TAstNode {
	/// Get the location for the node.
	fn get_location(&self) -> Location;
}

/// A representation of the abstract syntax tree (AST) generated by the `Parser`.
///
/// This acts as the root node of the tree.
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

	/// Add a module to the tree.
	pub fn push_module(&mut self, module: Module) {
		self.modules.push(module);
	}
}

impl TAstNode for Ast {
	fn get_location(&self) -> Location {
		Location::new(0, 0)
	}
}

/// A representation of a module in the AST.
///
/// A module represents the AST of a single file.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
	pub definitions: Vec<Definition>,
}

impl Module {
	/// Create a new instance of `Module`.
	pub fn new() -> Module {
		Module {
			definitions: Vec::new(),
		}
	}

	/// Add a definition to the module.
	pub fn push_definition(&mut self, definition: Definition) {
		self.definitions.push(definition);
	}
}

impl TAstNode for Module {
	fn get_location(&self) -> Location {
		Location::start()
	}
}

/// A representation of the possible definitions in the AST.
///
/// A definition represents a top-level declaration in a module. Definitions cannot be nested and
/// are the only object allowed to appear at the top-level of a module.
#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
	Function(Function),
}

impl TAstNode for Definition {
	fn get_location(&self) -> Location {
		match self {
			Definition::Function(func) => func.get_location(),
		}
	}
}

/// A representation of a function in the AST.
///
/// A function represents a re-usable section of code within a program and can be invoked by
/// using a call expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
	pub identifier: Identifier,
	pub arguments: Vec<Argument>,
	pub return_type: TypeHint,
	pub body: Block,
	location: Location,
}

impl Function {
	/// Create a new instance of `Function`.
	pub fn new(identifier: Identifier, arguments: Vec<Argument>, return_type: TypeHint, body: Block, location: Location) -> Function {
		let mut ret = Function {
			identifier: identifier,
			arguments: arguments,
			return_type: return_type,
			body: body,
			location: location,
		};

		ret.identifier.set_symbol_type(SymbolType::Function);

		ret
	}

	///
	///
	///
	pub fn get_name(&self) -> String {
		self.identifier.name.clone()
	}
}

impl TAstNode for Function {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of an identifier in the AST.
///
/// An identifier is used to identify where certain data is stored. This might be used as part of a
/// let statement or the use of the variable declared in said statement, to identify a function or
/// identify the name of a single argument within a function.
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
	name: String,
	location: Location,
	symbol_type: SymbolType,
}

impl Identifier {
	/// Create a new instance of `Identifier`.
	pub fn new(name: String, location: Location) -> Identifier {
		Identifier {
			name: name,
			location: location,
			symbol_type: SymbolType::Unknown,
		}
	}

	///
	///
	pub fn get_name(&self) -> String {
		self.name.clone()
	}

	///
	///
	pub fn set_symbol_type(&mut self, symbol_type: SymbolType) {
		self.symbol_type = symbol_type;
	}

	///
	///
	pub fn get_symbol_type(&mut self) -> SymbolType {
		self.symbol_type
	}
}

impl TAstNode for Identifier {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of an argument in the AST.
///
/// An argument is used to declare the name and type of an argument within a function definition.
/// During type checking, the type of the argument is used to validate any call expressions for
/// that function. The name of the argument is also used to match any identifier expressions that
/// refer to the argument,
#[derive(Clone, Debug, PartialEq)]
pub struct Argument {
	identifier: Identifier,
	type_hint: TypeHint,
}

impl Argument {
	/// Create a new instance of `Argument`.
	pub fn new(identifier: Identifier, type_hint: TypeHint) -> Argument {
		let mut ret = Argument {
			identifier: identifier,
			type_hint: type_hint,
		};

		ret.identifier.set_symbol_type(SymbolType::Argument);

		ret
	}

	///
	///
	///
	pub fn get_name(&self) -> String {
		self.identifier.name.clone()
	}

	///
	///
	///
	pub fn get_type_hint(&self) -> TypeHint {
		self.type_hint
	}
}

impl TAstNode for Argument {
	fn get_location(&self) -> Location {
		self.identifier.get_location()
	}
}

/// A representation of a type hint in the AST.
///
/// A type hint is used to describe the type of various constructs, such as in a let statement,
/// function argument or function return type.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TypeHint {
	Int32,
	Float,
	Bool,
	None,
}

impl TypeHint {
	/// Create a new instance of `TypeHint` from the given token.
	pub fn new_from_token(token: Token) -> Result<TypeHint, CompilationError> {
		match token.token_type {
			TokenType::TypeInt32 => Ok(TypeHint::Int32),
			TokenType::TypeFloat => Ok(TypeHint::Float),
			TokenType::TypeBool  => Ok(TypeHint::Bool),

			_ => {
				Err(internal_error(ErrorCode::E1005,
					token.location,
					format!("Unable to convert token to TypeHint: {} ({:?})",
						token,
						token.token_type)))
			}
		}
	}
}

impl fmt::Display for TypeHint {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			// prefix operators
			TypeHint::Int32 => write!(f, "i32"),
			TypeHint::Float => write!(f, "f32"),
			TypeHint::Bool  => write!(f, "bool"),
			TypeHint::None  => write!(f, "<none>"),
		}
	}
}

/// A representation of a block in the AST.
///
/// A block is denoted by the opening and closing of braces, e.g. `{ ... }`, and is a collection of
/// statements. Blocks are used for the body of a function definition, loop statement and if
/// statement. Any variables defined within the block are only available within that block and any
/// child blocks.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
	pub statements: Vec<Statement>,
	pub id: usize,
	location: Location,
}

impl Block {
	/// Create a new instance of `Block`.
	pub fn new(id: usize, location: Location) -> Block {
		Block {
			statements: Vec::new(),
			id: id,
			location: location,
		}
	}

	/// Create a new instance of `Block` with predefined statements
	///
	/// This is to support testing, so the compiler will complain about it for normal usage.
	#[allow(dead_code)]
	pub fn new_with_statements(statements: Vec<Statement>, id: usize, location: Location) -> Block {
		Block {
			statements: statements,
			id: id,
			location: location,
		}
	}

	/// Add a statement to the block.
	pub fn push_statement(&mut self, statement: Statement) {
		self.statements.push(statement);
	}
}

impl TAstNode for Block {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of the possible statements within the AST.
///
/// Statements can only appear within the context of a block and usually have side-effects. As
/// such, statements form the logic of a program and describe what it does at any given point.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	Let(Let),
	Return(Expression),
	Loop(Loop),
	If(If),
	Break(LoopControl),
	Continue(LoopControl),
	Expression(Expression),
}

impl TAstNode for Statement {
	fn get_location(&self) -> Location {
		match self {
			Statement::Let(let_stmt)    => let_stmt.get_location(),
			Statement::Return(expr)     => expr.get_location(),
			Statement::Loop(loop_stmt)  => loop_stmt.get_location(),
			Statement::If(if_stmt)      => if_stmt.get_location(),
			Statement::Break(ctrl)      => ctrl.get_location(),
			Statement::Continue(ctrl)   => ctrl.get_location(),
			Statement::Expression(expr) => expr.get_location(),
		}
	}
}

/// A representation of a let statement in the AST.
///
/// A let statement declares a new variable and assigns a value to said variable. Is also has a type
/// hint which the value must match and is used for type checking during the semantic analysis stage.
#[derive(Clone, Debug, PartialEq)]
pub struct Let {
	identifier: Identifier,
	pub type_hint: TypeHint,
	pub value: Expression,
	location: Location,
}

impl Let {
	/// Create a new instance of `Let`.
	pub fn new(identifier: Identifier, type_hint: TypeHint, value: Expression, location: Location) -> Let {
		let mut ret = Let {
			identifier: identifier,
			type_hint: type_hint,
			value: value,
			location: location,
		};

		ret.identifier.set_symbol_type(SymbolType::Local);

		ret
	}

	///
	///
	///
	pub fn get_name(&self) -> String {
		self.identifier.name.clone()
	}

	///
	///
	///
	pub fn get_type_hint(&self) -> TypeHint {
		self.type_hint
	}
}

impl TAstNode for Let {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of a loop statement within the AST.
///
/// Loop statements are used to perform the action or set of actions repeatedly until a
/// pre-specified condition is reached.
#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
	pub body: Block,
	pub location: Location,
}

impl Loop {
	/// Create a new instance of `Loop`.
	pub fn new(body: Block, location: Location) -> Loop {
		Loop {
			body: body,
			location: location,
		}
	}

	///
	///
	///
	pub fn get_id(&self) -> usize {
		self.body.id
	}
}

impl TAstNode for Loop {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of an if statement in the AST.
///
/// An if statement consists of multiple conditions, each followed by a consequence where if the
/// condition evaluates to true, then the associated consequence will be executed and no more
/// conditions will be evaluated.
#[derive(Clone, Debug, PartialEq)]
pub struct If {
	pub branches: Vec<Branch>,
	pub location: Location,
}

impl If {
	/// Create a new instance of `If`.
	pub fn new(branches: Vec<Branch>, location: Location) -> If {
		If {
			branches: branches,
			location: location,
		}
	}
}

impl TAstNode for If {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of a branch in the AST.
///
/// A branch is part of an if statement, where each branch represents a branch within the if
/// statement. branches usually have a condition and consequence, although in cases where an
/// `if-else` statement is found, the final branch's condition will simply be `true`.
#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
	pub condition: Expression,
	pub consequence: Block,
}

impl Branch {
	/// Create a new instance of `Branch`.
	pub fn new(condition: Expression, consequence: Block) -> Branch {
		Branch {
			condition: condition,
			consequence: consequence,
		}
	}
}

impl TAstNode for Branch {
	fn get_location(&self) -> Location {
		self.condition.get_location()
	}
}

/// TODO: document
///
///
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LoopControl {
	pub loop_id: usize,
	pub location: Location,
}

impl LoopControl {
	/// Create a new instance of `LoopControl`.
	pub fn new(location: Location) -> LoopControl {
		LoopControl {
			loop_id: 0,
			location: location,
		}
	}

	/// Setter for `loop_id`.
	pub fn set_loop_id(&mut self, loop_id: usize) {
		self.loop_id = loop_id;
	}

	/// Getter for `loop_id`.
	pub fn get_loop_id(&mut self) -> usize {
		self.loop_id
	}
}

impl TAstNode for LoopControl {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of the possible expressions in the AST.
///
/// Expressions are used within statements, such as let, return and if statements, and are also
/// commonly recursive within expressions themselves, e.g. an infix expression contains two
/// sub-expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	Identifier(Identifier),
	Literal(Literal),
	Infix(Infix),
	Prefix(Prefix),
	Call(Call),
}

impl Expression {
	pub fn is_assignment(&self) -> bool {
		match self {
			Expression::Infix(infix) => infix.is_assignment(),
			_ => false,
		}
	}
}

impl TAstNode for Expression {
	fn get_location(&self) -> Location {
		match self {
			Expression::Identifier(ident) => ident.get_location(),
			Expression::Literal(literal)  => literal.get_location(),
			Expression::Infix(infix)      => infix.get_location(),
			Expression::Prefix(prefix)    => prefix.get_location(),
			Expression::Call(call)        => call.get_location(),
		}
	}
}

/// A representation of a literal expression in the AST.
///
/// Literals are static values within a program and can be one of the following 3 types:
/// - Integer
/// - Float
/// - Boolean
#[derive(Clone, Debug, PartialEq)]
pub struct Literal {
	pub type_hint: TypeHint,
	pub value: LiteralValue,
	pub location: Location,
}

impl Literal {
	/// Create a new instance of `Literal`.
	pub fn new(type_hint: TypeHint, value: LiteralValue, location: Location) -> Literal {
		Literal {
			type_hint: type_hint,
			value: value,
			location: location,
		}
	}

	pub fn new_from_token(token: Token) -> Result<Literal, CompilationError> {
		match token.token_type {
			TokenType::Integer => {
				Ok(Literal::new(TypeHint::Int32,
					LiteralValue::Int32(token.value.unwrap()),
					token.location))
			},
			TokenType::Float => {
				Ok(Literal::new(TypeHint::Float,
					LiteralValue::Float(token.value.unwrap()),
					token.location))
			},
			TokenType::True => {
				Ok(Literal::new(TypeHint::Bool,
					LiteralValue::True,
					token.location))
			},
			TokenType::False => {
				Ok(Literal::new(TypeHint::Bool,
					LiteralValue::False,
					token.location))
			},
			_ => {
				Err(internal_error(ErrorCode::E1000,
					token.location,
					format!("Unable to convert token to literal: {} ({:?})",
						token,
						token.token_type)))
			}
		}
	}
}

impl TAstNode for Literal {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// TODO: Document
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
	Int32(String),
	Float(String),
	True,
	False,
}

/// A representation of an infix expression in the AST.
///
/// Infix expressions consist of two operands delimited by an operator, e.g. `1 + 1`.
#[derive(Clone, Debug, PartialEq)]
pub struct Infix {
	pub left: Box<Expression>,
	pub operator: Operator,
	pub right: Box<Expression>,
}

impl Infix {
	/// Create a new instance of `Infix`.
	pub fn new(left: Expression, operator: Operator, right: Expression) -> Infix {
		Infix {
			left: Box::new(left),
			operator: operator,
			right: Box::new(right),
		}
	}

	/// Test if the operator within the infix expression is an assignment operator or not.
	pub fn is_assignment(&self) -> bool {
		self.operator == Operator::Assign || self.is_complex_assignment()
	}

	/// Test if the operator within the infix expression is an assignment operator with
	/// side-effects or not.
	///
	/// Assignment operators with aide-effects are operators such as `+=` compared to the basic
	/// assignment operator `=`
	pub fn is_complex_assignment(&self) -> bool {
		match self.operator {
			Operator::PlusAssign
			| Operator::MinusAssign
			| Operator::MultiplyAssign
			| Operator::DivideAssign
			| Operator::ModuloAssign
			| Operator::BitAndAssign
			| Operator::BitOrAssign
			| Operator::BitXorAssign
			| Operator::BitLeftShiftAssign
			| Operator::BitRightShiftAssign => true,

			_ => false,
		}
	}

	/// Expand an assignment expression if possible.
	pub fn expand_assignment(&mut self) -> Result<(), CompilationError> {
		if self.is_complex_assignment() {
			let left = self.left.clone();
			let right = self.right.clone();

			let op = self.operator.expand_assignment()?;
			let infix = Infix::new(*left, op, *right);

			self.operator = Operator::Assign;
			self.right = Box::new(Expression::Infix(infix));
		}

		Ok(())
	}
}

impl TAstNode for Infix {
	fn get_location(&self) -> Location {
		self.left.get_location()
	}
}

/// A representation of a prefix expression in the AST.
///
/// Prefix expressions contain a single operator followed by an operand, e.g. `not true`.
#[derive(Clone, Debug, PartialEq)]
pub struct Prefix {
	pub operator: Operator,
	pub right: Box<Expression>,
	location: Location,
}

impl Prefix {
	/// Create a new instance of `Prefix`.
	pub fn new(operator: Operator, right: Expression, location: Location) -> Prefix {
		Prefix {
			operator: operator,
			right: Box::new(right),
			location: location,
		}
	}
}

impl TAstNode for Prefix {
	fn get_location(&self) -> Location {
		self.location
	}
}

/// A representation of a call expression in the AST.
///
/// Call expressions are used to call functions and must supply the expected number and type of
/// arguments for the function. The resulting type of the call expression is dependant on the
/// return type of the function.
#[derive(Clone, Debug, PartialEq)]
pub struct Call {
	identifier: Identifier,
	pub arguments: Vec<Expression>,
}

impl Call {
	/// Create a new instance of `Call`.
	pub fn new(identifier: Identifier, arguments: Vec<Expression>) -> Call {
		Call {
			identifier: identifier,
			arguments: arguments,
		}
	}

	///
	///
	///
	pub fn get_name(&self) -> String {
		self.identifier.name.clone()
	}
}

impl TAstNode for Call {
	fn get_location(&self) -> Location {
		self.identifier.get_location()
	}
}


/// A representation of an operator in the AST.
///
/// Operators are used to modify or join other expressions together and fall into 3 categories:
/// - Prefix, e.g. `not true`.
/// - Infix, e.g. `1 + 1`.
/// - Assignment, e.g. `x = 5`.
#[derive(Clone, Copy, Debug, PartialEq)]
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
	/// Attempt to convert a token to a prefix operator.
	///
	/// Note that this method is separate from the similar `Operator::new_infix_from_token`
	/// method below because `TokenType::Minus` is a valid prefix and infix operator depending on
	/// the context.
	///
	/// If a token with a type not in the above list is provided then an error will be returned.
	pub fn new_prefix_from_token(token: Token) -> Result<Operator, CompilationError> {
		match token.token_type {
			// valid prefix operators
			TokenType::Not      => Ok(Operator::Not),
			TokenType::Minus    => Ok(Operator::UnaryMinus),
			TokenType::BitCompl => Ok(Operator::BitCompl),

			// everything else
			_ => {
				Err(internal_error(ErrorCode::E1001,
					token.location,
					format!("Unable to convert token to prefix operator: {} ({:?})",
						token,
						token.token_type)))
			},
		}
	}

	/// Attempt to convert a token to a infix operator.
	///
	/// This method accepts both infix and assignment operators which have different semantics.
	/// For example, assignment operators require the left operand to be an identifier and
	/// expressions that use them cannot be used in conditionals. It is expected that these
	/// differences are handled during the semantic analysis stage.
	///
	/// Note that this method is separate from the similar `Operator::new_prefix_from_token`
	/// method below because `TokenType::Minus` is a valid prefix and infix operator depending on
	/// the context.
	///
	/// If a token with a type not in the above list is provided then an error will be returned.
	pub fn new_infix_from_token(token: Token) -> Result<Operator, CompilationError> {
		match token.token_type {
			// valid infix operators
			TokenType::Plus                => Ok(Operator::Plus),
			TokenType::Minus               => Ok(Operator::Minus),
			TokenType::Multiply            => Ok(Operator::Multiply),
			TokenType::Divide              => Ok(Operator::Divide),
			TokenType::Modulo              => Ok(Operator::Modulo),
			TokenType::BitAnd              => Ok(Operator::BitAnd),
			TokenType::BitOr               => Ok(Operator::BitOr),
			TokenType::BitXor              => Ok(Operator::BitXor),
			TokenType::BitCompl            => Ok(Operator::BitCompl),
			TokenType::BitLeftShift        => Ok(Operator::BitLeftShift),
			TokenType::BitRightShift       => Ok(Operator::BitRightShift),
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
			TokenType::LessThan            => Ok(Operator::LessThan),
			TokenType::LessThanOrEqual     => Ok(Operator::LessThanOrEqual),
			TokenType::GreaterThan         => Ok(Operator::GreaterThan),
			TokenType::GreaterThanOrEqual  => Ok(Operator::GreaterThanOrEqual),
			TokenType::Equal               => Ok(Operator::Equal),
			TokenType::NotEqual            => Ok(Operator::NotEqual),
			TokenType::And                 => Ok(Operator::And),
			TokenType::Or                  => Ok(Operator::Or),

			// everything else
			_ => {
				Err(internal_error(ErrorCode::E1002,
				token.location,
				format!("Unable to convert token to infix operator: {} ({:?})",
					token,
					token.token_type)))
			},
		}
	}

	///
	///
	///
	pub fn expand_assignment(&self) -> Result<Operator, CompilationError> {
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

			// cannot be expanded
			Operator::Assign => {
				Err(internal_error(ErrorCode::E1003,
					Location::end(),
					format!("Unable to expand simple assignment operator: {} ({:?})",
						self,
						self)))
			},

			// not an assignment operator
			_ => {
				Err(internal_error(ErrorCode::E1004,
					Location::end(),
					format!("Unable to expand non-assignment operator: {} ({:?})",
						self,
						self)))
			},
		}
	}
}

impl fmt::Display for Operator {
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
			Operator::BitLeftShift       => write!(f, ">>"),
			Operator::BitRightShift      => write!(f, "<<"),
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

	// An instance of `Location` used as a placeholder.
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

	// Test all valid inputs for `Operator::new_prefix_from_token` are accepted.
	#[test]
	fn test_operator_new_prefix_from_token_valid() {
		let data = vec![
			(token!(TokenType::Not), Operator::Not),
			(token!(TokenType::Minus), Operator::UnaryMinus),
			(token!(TokenType::BitCompl), Operator::BitCompl),
		];

		for (input, expected) in data.iter() {
			let res = Operator::new_prefix_from_token(input.clone());
			assert!(res.is_ok());

			let output = res.unwrap();
			assert_eq!(output, *expected);
		}
	}

	// Test all valid inputs for `Operator::new_infix_from_token` are accepted,
	#[test]
	fn test_operator_new_infix_from_token_valid() {
		let data = vec![
			(token!(TokenType::Plus), Operator::Plus),
			(token!(TokenType::Minus), Operator::Minus),
			(token!(TokenType::Multiply), Operator::Multiply),
			(token!(TokenType::Divide), Operator::Divide),
			(token!(TokenType::Modulo), Operator::Modulo),
			(token!(TokenType::BitAnd), Operator::BitAnd),
			(token!(TokenType::BitOr), Operator::BitOr),
			(token!(TokenType::BitXor), Operator::BitXor),
			(token!(TokenType::BitCompl), Operator::BitCompl),
			(token!(TokenType::BitLeftShift), Operator::BitLeftShift),
			(token!(TokenType::BitRightShift), Operator::BitRightShift),
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
			(token!(TokenType::LessThan), Operator::LessThan),
			(token!(TokenType::LessThanOrEqual), Operator::LessThanOrEqual),
			(token!(TokenType::GreaterThan), Operator::GreaterThan),
			(token!(TokenType::GreaterThanOrEqual), Operator::GreaterThanOrEqual),
			(token!(TokenType::Equal), Operator::Equal),
			(token!(TokenType::NotEqual), Operator::NotEqual),
			(token!(TokenType::And), Operator::And),
			(token!(TokenType::Or), Operator::Or),
		];

		for (input, expected) in data.iter() {
			let res = Operator::new_infix_from_token(input.clone());
			assert!(res.is_ok());

			let output = res.unwrap();
			assert_eq!(output, *expected);
		}
	}

	#[test]
	fn test_operator_expand_assignment() {}

	#[test]
	fn test_infix_is_assignment() {}

	#[test]
	fn test_infix_is_complex_assignment() {}
}
