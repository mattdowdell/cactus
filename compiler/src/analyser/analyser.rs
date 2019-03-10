//!
//!
//!

use std::{
	collections::{
		HashMap,
		VecDeque
	},
	mem,
};

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
		Literal,
		Operator,
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
	signatures: HashMap<String, FunctionSignature>,
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
			signatures: HashMap::new(),
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
			Ok((name, signature)) => {
				self.signatures.insert(name, signature);
			},
			Err(error) => {
				self.errors.push(error);
			},
		};

		// add the function name for lookups
		let name = function.identifier.name.clone();
		let return_type = match TypeHint::from_optional_ast_type(function.return_type.clone()) {
			Ok(type_hint) => type_hint,
			Err(error) => {
				self.errors.push(error);
				// default to none as the return type
				TypeHint::None
			}
		};

		match self.symbol_table.push_function(name, return_type) {
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
			let type_hint = match TypeHint::from_ast_type(argument.type_hint.clone()) {
				Ok(type_hint) => type_hint,
				Err(error) => {
					self.errors.push(error);
					// default to i32 as the argument type
					TypeHint::Int32
				}
			};

			match self.symbol_table.push_argument(name, path, type_hint) {
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
					let type_hint = match TypeHint::from_ast_type(let_stmt.type_hint.clone()) {
						Ok(type_hint) => type_hint,
						Err(error) => {
							self.errors.push(error);
							// default to i32 as the argument type
							TypeHint::Int32
						}
					};

					match self.symbol_table.push_local(name, path, type_hint) {
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
	fn analyse_expression(&mut self, expression: &mut Expression) -> Result<TypeHint, ()> {
		match expression {
			Expression::Identifier(ident) => {
				let name = ident.name.clone();
				let path = self.table_path.clone();

				match self.symbol_table.lookup_symbol(name, path, false) {
					Ok(type_hint) => Ok(type_hint),
					Err(error) => {
						self.errors.push(error);
						Err(())
					}
				}
			},
			Expression::Literal(literal) => {
				match literal {
					Literal::Integer(_) => Ok(TypeHint::Int32),
					Literal::Boolean(_) => Ok(TypeHint::Bool),
					Literal::Float(_) => Ok(TypeHint::Float),
				}
			},
			Expression::Struct(_) => {
				let error = not_implemented_error!(
					ErrorCode::E0803,
					Location::end(),
					"Struct initialisations are not yet supported for analysis"
				);

				self.errors.push(error);
				Err(())
			},
			Expression::Prefix(operator, sub_expr) => {
				let type_hint = self.analyse_expression(sub_expr)?;

				match operator.allow_prefix(type_hint) {
					Ok(_) => Ok(type_hint),
					Err(error) => {
						self.errors.push(error);
						Err(())
					},
				}
			},
			Expression::Infix(left_expr, operator, right_expr) => {
				if operator.is_assignment() && *operator != Operator::Assign {
					let sub_operator = match operator.assignment_to_infix() {
						Ok(op) => Ok(op),
						Err(error) => {
							self.errors.push(error);
							Err(())
						}
					}?;

					let new_right_expr = Expression::Infix(
						left_expr.clone(),
						sub_operator,
						right_expr.clone(),
					);

					mem::replace(right_expr, Box::new(new_right_expr));
				}

				let left_type = self.analyse_expression(left_expr)?;
				let right_type = self.analyse_expression(right_expr)?;

				match operator.allow_infix(left_type, right_type) {
					Ok(type_hint) => Ok(type_hint),
					Err(error) => {
						self.errors.push(error);
						Err(())
					}
				}
			},
			Expression::Call(ident, params) => {
				let name = ident.name.clone();
				let path = self.table_path.clone();

				match self.symbol_table.lookup_symbol(name, path, true) {
					Ok(_) => {},
					Err(error) => {
						self.errors.push(error);
						return Err(())
					}
				};

				let mut param_types: Vec<TypeHint> = Vec::new();

				for param_expr in params.iter_mut() {
					match self.analyse_expression(param_expr) {
						Ok(type_hint) => param_types.push(type_hint),
						Err(_) => {},
					}
				}

				if self.signatures.contains_key(&ident.name) {
					let signature = &self.signatures[&ident.name];
					let signature_args = signature.arguments.clone();

					if signature_args.len() != param_types.len() {
						let error = type_error!(
							ErrorCode::E0206,
							Location::end(),
							"Expected {} arguments for function call {:?}. Found {}",
							signature_args.len(),
							ident.name,
							param_types.len()
						);

						self.errors.push(error);
					}

					for (index, (given, expected)) in param_types.iter().zip(signature_args.iter()).enumerate() {
						if expected != given {
							let error = type_error!(
								ErrorCode::E0205,
								Location::end(),
								"Incorrect type given in function call for {:?}: Expected {:?} for argument {}, found {:?}",
								ident.name,
								expected,
								index + 1,
								given
							);

							self.errors.push(error);
						}
					}

					Ok(signature.return_type)
				} else {
					// if the signature is not present
					// then there was a problem creating it
					// in which case the error will be output somewhere above
					Err(())
				}
			},
		}
	}
}


#[cfg(test)]
mod test {
	//use super::*;
}
