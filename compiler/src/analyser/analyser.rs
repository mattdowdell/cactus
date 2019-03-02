//!
//!
//!

use std::collections::VecDeque;

use crate::{
	error::{
		CompilationError,
		ErrorCode,
		ErrorType,
	},
	location::Location,
	parser::ast::{
		Ast,
		Module,
		Definition,
		Function,
		Block,
		Statement,
		Expression,
	},
	analyser::{
		symbol::{
			SymbolTable,
			SymbolType,
		},
		type_checker::{
			FunctionSignature,
			TypeHint,
		},
	},
};


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Analyser {
	ast: Ast,
	errors: Vec<CompilationError>,
	signatures: Vec<FunctionSignature>,
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
			errors: Vec::new(),
			signatures: Vec::new(),
			symbol_table: SymbolTable::new(),
			table_path: VecDeque::new(),
		}
	}

	///
	///
	///
	pub fn analyse_ast(&mut self) -> Result<Ast, Vec<CompilationError>> {
		let mut ast = self.ast.clone();

		for module in ast.modules.iter_mut() {
			self.analyse_module(module);
		}

		if self.errors.len() > 0 {
			Err(self.errors.clone())
		} else {
			Ok(ast)
		}
	}

	//
	//
	//
	fn analyse_module(&mut self, module: &mut Module) {
		for definition in module.definitions.iter_mut() {
			self.analyse_definition(definition);
		}
	}

	//
	//
	//
	fn analyse_definition(&mut self, definition: &mut Definition) {
		match definition {
			Definition::Function(function) => {
				self.analyse_function(function);
			},
			Definition::Import(_) => {
				let error = not_implemented_error!(
					ErrorCode::E0801,
					Location::end(),
					"Imports are not yet supported"
				);
				self.errors.push(error);
			},
			Definition::Enum(_) => {},
			Definition::Struct(_) => {},
		}
	}

	//
	//
	//
	fn analyse_function(&mut self, function: &mut Function) {
		// add the function signature for type checking
		match FunctionSignature::new(function.clone()) {
			Ok(signature) => self.signatures.push(signature),
			Err(error) => self.errors.push(error),
		};

		// add the function name for lookups
		let name = function.identifier.name.clone();

		match self.symbol_table.push_function(name) {
			Ok(_) => {
				function.identifier.set_symbol_type(SymbolType::Function);
			},
			Err(error) => self.errors.push(error),
		};

		let sub_table_index = self.symbol_table.sub_table();
		self.table_path.push_back(sub_table_index);

		for argument in function.arguments.iter_mut() {
			let name = argument.identifier.name.clone();
			let path = self.table_path.clone();

			match self.symbol_table.push_argument(name, path) {
				Ok(offset) => {
					argument.identifier.set_symbol_type(SymbolType::Argument);
					argument.identifier.set_offset(offset);
				},
				Err(error) => self.errors.push(error),
			};


		}

		self.analyse_block(&mut function.body, false);
		self.table_path.pop_back();
	}

	//
	//
	//
	fn analyse_block(&mut self, block: &mut Block, in_loop: bool) {
		for statement in block.statements.iter_mut() {
			match statement {
				Statement::Let(let_stmt) => {
					let name = let_stmt.identifier.name.clone();
					let path = self.table_path.clone();

					match self.symbol_table.push_local(name, path) {
						Ok(offset) => {
							let_stmt.identifier.set_symbol_type(SymbolType::Local);
							let_stmt.identifier.set_offset(offset);
						},
						Err(error) => {
							self.errors.push(error);
						},
					};

					match self.analyse_expression(&mut let_stmt.value) {
						Ok(type_hint) => {
							match TypeHint::from_ast_type(let_stmt.type_hint.clone()) {
								Ok(expected) => {
									if type_hint != expected {
										let error = type_error!(
											ErrorCode::E0200,
											Location::end(),
											"Expected type {:?} for expression, found: {:?}",
											let_stmt.type_hint,
											type_hint
										);
										self.errors.push(error);
									}
								},
								Err(error) => {
									self.errors.push(error);
								}
							}
						},
						Err(_) => {},
					}
				},
				Statement::Return(ref mut expr) => {
					match self.analyse_expression(expr) {
						Ok(_type_hint) => {},
						Err(_) => {}
					}
				},
				Statement::Loop(ref mut body) => {
					let sub_table_index = self.symbol_table.sub_table();
					self.table_path.push_back(sub_table_index);
					self.analyse_block(body, true);
					self.table_path.pop_back();
				},
				Statement::If(ref mut if_stmt) => {
					match self.analyse_expression(&mut if_stmt.condition) {
						Ok(type_hint) => {
							if type_hint != TypeHint::Bool {
								let error = type_error!(
									ErrorCode::E0201,
									Location::end(),
									"Expected type {:?} for condition expression, found {:?}",
									TypeHint::Bool,
									type_hint
								);
								self.errors.push(error);
							}
						},
						Err(_) => {}
					}

					let sub_table_index = self.symbol_table.sub_table();
					self.table_path.push_back(sub_table_index);
					self.analyse_block(&mut if_stmt.consequence, in_loop);
					self.table_path.pop_back();

					if if_stmt.other.len() > 0 {
						let error = not_implemented_error!(
							ErrorCode::E0802,
							Location::end(),
							"elif in if statements is not yet supported"
						);
						self.errors.push(error);
					}

					match if_stmt.alternative {
						Some(ref mut alt) => {
							let sub_table_index = self.symbol_table.sub_table();
							self.table_path.push_back(sub_table_index);
							self.analyse_block(alt, in_loop);
							self.table_path.pop_back();
						}
						None => {},
					}

				},
				Statement::Expression(ref mut expr) => {
					match self.analyse_expression(expr) {
						Ok(_) => {},
						Err(_) => {}
					}
				},
				Statement::Break => {
					if !in_loop {
						let error = syntax_error!(
							ErrorCode::E0000,
							Location::end(),
							"A break statement can only appear within the body of a loop statement"
						);

						self.errors.push(error);
					}
				},
				Statement::Continue => {
					if !in_loop {
						let error = syntax_error!(
							ErrorCode::E0001,
							Location::end(),
							"A continue statement can only appear within the body of a loop statement"
						);

						self.errors.push(error);
					}
				},
			}
		}
	}

	//
	//
	//
	fn analyse_expression(&mut self, _expression: &mut Expression) -> Result<TypeHint, ()> {
		unimplemented!()
	}

	/*
	fn lookup_expression_symbols(&self, expression: Expression) -> Result<(), Vec<String>> {
		let mut errors: Vec<String> = Vec::new();

		match expression {
			Expression::Identifier(ident) => {
				let name = ident.name.clone();
				let path = self.table_path.clone();

				if !self.symbol_table.lookup_symbol(name, path, false) {
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
	*/
}


#[cfg(test)]
mod test {
	//use super::*;
}
