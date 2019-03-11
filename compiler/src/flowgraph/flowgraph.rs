//!
//!
//!

use std::collections::HashMap;

use crate::{
	parser::ast::{
		Ast,
		Definition,
		Block,
		Statement,
		Let,
		If,
		Expression,
	},
	analyser::symbol::SymbolType,
	flowgraph::instruction::{
		Instruction,
		Literal,
		Symbol,
		loop_label_start,
		loop_label_end,
	},
};

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct FlowGraph {
	instructions: Vec<Instruction>,
	blocks: HashMap<String, BasicBlock>,
}

impl FlowGraph {
	///
	///
	///
	pub fn new() -> FlowGraph {
		FlowGraph {
			instructions: Vec::new(),
			blocks: HashMap::new(),
		}
	}

	///
	///
	///
	pub fn convert_ast(&mut self, ast: Ast) {
		for module in ast.modules.iter() {
			for definition in module.definitions.iter() {
				match definition {
					Definition::Import(_)
					| Definition::Struct(_)
					| Definition::Enum(_) => {},

					Definition::Function(func) => {
						self.convert_block(&func.body)
					},
				}
			}
		}
	}

	///
	///
	///
	pub fn to_basic_blocks(&mut self) {
		let mut add_to_block = false;
		let mut block = BasicBlock::new("_".to_string());

		for instr in self.instructions.iter() {
			match instr {
				Instruction::Labeldef(label) => {
					if block.len() > 0 {
						self.blocks.insert(block.label.clone(), block.clone());
					}

					block = BasicBlock::new(label.clone());
					add_to_block = true;
				},
				Instruction::Jmp => {
					add_to_block = false;
				},
				_ => {
					if add_to_block {
						block.push(instr.clone());
					}
				},
			}
		}
	}

	///
	///
	///
	pub fn follow_graph(&mut self, name: String) {
		// starting at main, move until a jmp instruction is found
		// when that occurs, move to the last address on the stack
		// until we reach something without a jmp
		// at that point insert a halt and stop

		// for conditions, we need to account for jmpnz as well
		// but that also needs support when creating the block
		// as well and when converting to instructions

		if !self.blocks.contains_key(&name) {
			panic!("Unable to find basic block with label: {:?}", name);
		} else {
			let block = self.blocks.get_mut(&name).unwrap();
			let mut last_addr: Option<String> = None;

			block.used = true;

			for instr in block.instructions.iter() {
				match instr {
					Instruction::Pushaddr(addr) => {
						last_addr = Some(addr.clone());
					},
					Instruction::Jmp => {
						match last_addr {
							Some(addr) => {
								self.follow_graph(addr);
								return;
							},
							None => {
								panic!("JMP instruction found with no Pushaddr");
							}
						}
					},
					// nothing to do for this
					_ => {},
				}
			}

			block.push(Instruction::Halt);
		}
	}

	//
	//
	//
	pub fn flatten_basic_blocks(&self) -> Vec<Instruction> {
		let mut instructions: Vec<Instruction> = Vec::new();

		for (label, block) in self.blocks.iter() {
			if !block.used {
				continue;
			}

			instructions.push(Instruction::Labeldef(label.clone()));

			for instr in block.instructions.iter() {
				instructions.push(instr.clone());
			}
		}

		instructions
	}

	//
	//
	//
	fn convert_block(&mut self, block: &Block) {
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
				Statement::Break(loop_ref) => {
					let label = loop_label_end(loop_ref.loop_id);

					self.instructions.push(Instruction::Pushaddr(label));
					self.instructions.push(Instruction::Jmp);
				},
				Statement::Continue(loop_ref) => {
					let label = loop_label_start(loop_ref.loop_id);

					self.instructions.push(Instruction::Pushaddr(label));
					self.instructions.push(Instruction::Jmp);
				},
			}
		}
	}

	fn convert_let_statement(&mut self, let_stmt: &Let) {
		self.instructions.push(Instruction::Push(Literal::Symbol(Symbol::Locals)));
		self.instructions.push(Instruction::Push(Literal::Integer(let_stmt.identifier.get_offset_str())));
		self.convert_expression(&let_stmt.value);

		self.instructions.push(Instruction::Storeidx);

	}

	fn convert_loop_statement(&mut self, body: &Block) {
		let start_label = Instruction::new_loop_label(body.id, true);
		let end_label = Instruction::new_loop_label(body.id, false);

		self.instructions.push(start_label);
		self.convert_block(body);
		self.instructions.push(end_label);
	}

	fn convert_if_statement(&mut self, _if_stmt: &If) {
		unimplemented!()
	}

	fn convert_expression(&mut self, expression: &Expression) {
		match expression {
			Expression::Literal(literal) => {
				let literal = Literal::from_ast_literal(literal.clone());
				let instr = Instruction::Push(literal);

				self.instructions.push(instr);
			},
			Expression::Identifier(ident) => {
				let literal = match ident.get_symbol_type() {
					SymbolType::Local => Literal::Symbol(Symbol::Locals),
					SymbolType::Argument => Literal::Symbol(Symbol::Args),
					_ => {
						panic!(
							"Unexpected symbol type for identifier expression: {:?}",
							ident.get_symbol_type()
						);
					}
				};

				self.instructions.push(Instruction::Push(literal));
				// TODO need LOADIDX here, plus the addition of the index
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
			Expression::Call(ident, params) => {
				for param in params.iter() {
					self.convert_expression(param);
					self.instructions.push(Instruction::Pusharg);
				}

				self.instructions.push(Instruction::Pushaddr(ident.name.clone()));
				self.instructions.push(Instruction::Subcall);

				// TODO: figure out if this can be made optional
				//       as we don't always need the result of the function call
				self.instructions.push(Instruction::Pushret);
			},
		}
	}
}




///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock {
	pub label: String,
	pub instructions: Vec<Instruction>,
	pub used: bool,
}

impl BasicBlock {
	///
	///
	///
	pub fn new(label: String) -> BasicBlock {
		BasicBlock {
			label: label,
			instructions: Vec::new(),
			used: false,
		}
	}

	///
	///
	///
	pub fn push(&mut self, instruction: Instruction) {
		self.instructions.push(instruction);
	}

	///
	///
	///
	pub fn len(&self) -> usize {
		self.instructions.len()
	}
}
