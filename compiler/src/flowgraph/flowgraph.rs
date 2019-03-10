//!
//!
//!


use crate::{
	parser::ast::{
		Function,
		Block,
		Statement,
		Let,
		If,
		Expression,
	},
	flowgraph::instruction::Instruction,
};

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct FlowGraph {
	start: BasicBlock,
}

impl FlowGraph {
	///
	///
	///
	pub fn new(function: Function) -> FlowGraph {
		let mut block = BasicBlock::new();
		block.convert_block(function.body);

		FlowGraph {
			start: block,
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock {
	instructions: Vec<Instruction>,
}

impl BasicBlock {
	///
	///
	///
	pub fn new() -> BasicBlock {
		BasicBlock {
			instructions: Vec::new(),
		}
	}
	///
	///
	///
	pub fn convert_block(&mut self, block: Block) {
		for statement in block.statements.iter() {
			match statement {
				Statement::Let(let_stmt) => {
					self.convert_let_statement(let_stmt);
				},
				Statement::Return(expr) => {
					self.convert_expression(expr);

					self.instructions.push(Instruction::Movret);
					self.instructions.push(Instruction::Return);
				},
				Statement::Expression(expr) => {
					self.convert_expression(expr);
				},
				Statement::Loop(loop_stmt) => {
					self.convert_loop_statement(loop_stmt);
				},
				Statement::If(if_stmt) => {
					self.convert_if_statement(if_stmt);
				},
				Statement::Break => {},
				Statement::Continue => {},
			}
		}
	}

	fn convert_let_statement(&mut self, let_stmt: &Let) {
		self.convert_expression(&let_stmt.value);
		// STORE value in LOCALS
	}

	fn convert_loop_statement(&mut self, body: &Block) {
		unimplemented!()
	}

	fn convert_if_statement(&mut self, if_stmt: &If) {
		unimplemented!()
	}

	fn convert_expression(&mut self, expression: &Expression) {
		match expression {
			Expression::Literal(literal) => {
				self.instructions.push(
					Instruction::push_from_literal(literal.clone())
				);
			},
			Expression::Identifier(ident) => {
				// load the value
			},
			Expression::Struct(_) => {
				panic!("Converting a struct expression to instructions is not yet supported");
			},
			Expression::Prefix(op, expr) => {
				self.convert_expression(expr);
				self.instructions.push(Instruction::from_operator(*op));
			},
			Expression::Infix(left, op, right) => {
				self.convert_expression(left);
				self.convert_expression(right);
				self.instructions.push(Instruction::from_operator(*op));
			},
			Expression::Call(ident, params) => {},
		}
	}
}
