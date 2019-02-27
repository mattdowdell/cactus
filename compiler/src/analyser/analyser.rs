//!
//!
//!

use std::collections::VecDeque;

use parser::ast::{
	Ast,
	Module,
	Definition,
	Function,
	Block,
	Statement,
	Expression,
};
use analyser::symbol::{
	SymbolTable,
};


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Analyser {
	ast: Ast,
	symbol_table: SymbolTable,
	table_path: VecDeque<usize>,
}

impl Analyser {
	///
	///
	///
	pub fn new(ast: Ast) -> Analyser {
		Analyser {
			ast: ast,
			symbol_table: SymbolTable::new(),
			table_path: VecDeque::new(),
		}
	}

	///
	///
	///
	pub fn populate_symbol_table(&mut self) -> Result<(), Vec<String>> {
		let mut errors: Vec<String> = Vec::new();

		for module in self.ast.modules.clone().iter() {
			let res = self.add_module_symbols(module.clone());

			match res {
				Ok(_) => {},
				Err(mut sub_errors) => {
					errors.append(&mut sub_errors);
				}
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(())
		}
	}

	//
	//
	//
	fn add_module_symbols(&mut self, module: Module) -> Result<(), Vec<String>> {
		let mut errors: Vec<String> = Vec::new();

		for definition in module.definitions.iter() {
			match self.add_definition_symbols(definition.clone()) {
				Ok(_) => {},
				Err(mut sub_errors) => {
					errors.append(&mut sub_errors);
				}
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(())
		}
	}

	//
	//
	//
	fn add_definition_symbols(&mut self, definition: Definition) -> Result<(), Vec<String>> {
		let mut errors: Vec<String> = Vec::new();

		match definition {
			Definition::Function(func) => {
				match self.add_function_symbols(func.clone()) {
					Ok(_) => {},
					Err(mut sub_errors) => {
						errors.append(&mut sub_errors);
					}
				};
			},

			Definition::Import(_) => {
				let error = "Imports are not yet supported".to_string();
				errors.push(error);
			},

			_ => {
				// nothing to do for structs and enums
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(())
		}
	}

	//
	//
	//
	fn add_function_symbols(&mut self, func: Function) -> Result<(), Vec<String>> {
		let mut errors: Vec<String> = Vec::new();

		match self.symbol_table.push_function(func.identifier.name.clone()) {
			Ok(_) => {},
			Err(error) => {
				errors.push(error);
			},
		};

		let sub_table_index = self.symbol_table.sub_table();
		self.table_path.push_back(sub_table_index);

		for argument in func.arguments.iter() {
			let name = argument.identifier.name.clone();
			let path = self.table_path.clone();

			match self.symbol_table.push_argument(name, path) {
				Ok(_) => {},
				Err(error) => {
					errors.push(error);
				},
			};
		}

		match self.add_block_symbols(func.body) {
			Ok(_) => {},
			Err(mut sub_errors) => {
				errors.append(&mut sub_errors);
			},
		};

		self.table_path.pop_back();

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(())
		}
	}

	//
	//
	//
	fn add_block_symbols(&mut self, block: Block) -> Result<(), Vec<String>> {
		let mut errors: Vec<String> = Vec::new();

		for statement in block.statements.iter() {
			match statement {
				Statement::Let(let_stmt) => {
					let name = let_stmt.identifier.name.clone();
					let path = self.table_path.clone();

					match self.symbol_table.push_local(name, path) {
						Ok(_) => {},
						Err(error) => {
							errors.push(error);
						},
					};
				},
				Statement::Return(expr) => {
					match self.lookup_expression_symbols(expr.clone()) {
						Ok(_) => {},
						Err(mut sub_errors) => {
							errors.append(&mut sub_errors);
						},
					};
				},
				Statement::Loop(body) => {
					let sub_table_index = self.symbol_table.sub_table();
					self.table_path.push_back(sub_table_index);

					match self.add_block_symbols(body.clone()) {
						Ok(_) => {},
						Err(mut sub_errors) => {
							errors.append(&mut sub_errors);
						},
					};

					self.table_path.pop_back();
				},
				Statement::If(if_stmt) => {
					match self.lookup_expression_symbols(if_stmt.condition.clone()) {
						Ok(_) => {},
						Err(mut sub_errors) => {
							errors.append(&mut sub_errors);
						},
					}

					let sub_table_index = self.symbol_table.sub_table();
					self.table_path.push_back(sub_table_index);

					match self.add_block_symbols(if_stmt.consequence.clone()) {
						Ok(_) => {},
						Err(mut sub_errors) => {
							errors.append(&mut sub_errors);
						},
					}

					self.table_path.pop_back();

					// TODO: handle other/alternative fields
				},
				Statement::Expression(expr) => {
					match self.lookup_expression_symbols(expr.clone()) {
						Ok(_) => {},
						Err(mut sub_errors) => {
							errors.append(&mut sub_errors);
						},
					};
				},

				// nothing to do
				Statement::Continue | Statement::Break => {},
			};
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(())
		}
	}

	//
	//
	//
	fn lookup_expression_symbols(&self, expression: Expression) -> Result<(), Vec<String>> {
		let mut errors: Vec<String> = Vec::new();

		match expression {
			Expression::Identifier(ident) => {
				if !self.symbol_table.lookup_symbol(ident.name.clone(), self.table_path.clone(), false) {
					let error = format!("Error: Identifier {:?} used before it was defined.",
						ident.name.clone());
					errors.push(error);
				};
			},
			Expression::Literal(_) => {},
			Expression::Struct(_) => {
				// TODO: check struct field name are valid
				let error = "Error: Struct initialisation expressions are not implemented.".to_string();
				errors.push(error);
			},
			Expression::Prefix(_, expr) => {
				match self.lookup_expression_symbols(*expr.clone()) {
					Ok(_) => {},
					Err(mut sub_errors) => {
						errors.append(&mut sub_errors);
					},
				};
			},
			Expression::Infix(left, _, right) => {
				match self.lookup_expression_symbols(*left.clone()) {
					Ok(_) => {},
					Err(mut sub_errors) => {
						errors.append(&mut sub_errors);
					},
				};

				match self.lookup_expression_symbols(*right.clone()) {
					Ok(_) => {},
					Err(mut sub_errors) => {
						errors.append(&mut sub_errors);
					},
				};
			},
			Expression::Call(ident, params) => {
				if !self.symbol_table.lookup_symbol(ident.name.clone(), self.table_path.clone(), true) {
					let error = format!("Error: Function {:?} used before it was defined.",
						ident.name.clone());
					errors.push(error);
				};

				for param_expr in params.iter() {
					match self.lookup_expression_symbols(param_expr.clone()) {
						Ok(_) => {},
						Err(mut sub_errors) => {
							errors.append(&mut sub_errors);
						},
					};
				}
			},
		};

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(())
		}
	}
}


