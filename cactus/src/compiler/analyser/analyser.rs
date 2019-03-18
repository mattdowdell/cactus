//!
//!
//!

use std::collections::{HashMap, VecDeque};
use std::iter::FromIterator;

use crate::error::{CompilationError, ErrorCode, type_error, lookup_error, syntax_error};
use crate::compiler::parser::{Ast, Module, Definition, Function, Block, Statement, Expression, TypeHint, TAstNode};

use super::symbol_table::SymbolTable;
use super::typing::FunctionSignature;


pub struct Analyser {
	symbol_table: SymbolTable,
	signatures: HashMap<String, FunctionSignature>,
	errors: Vec<CompilationError>,
	symbol_path: Vec<usize>,
	cur_ret_type: TypeHint,
	has_returned: bool,
}

impl Analyser {
	///
	///
	///
	pub fn new() -> Analyser {
		Analyser {
			symbol_table: SymbolTable::new(0),
			signatures: HashMap::new(),
			errors: Vec::new(),
			symbol_path: Vec::new(),
			cur_ret_type: TypeHint::None,
			has_returned: false,
		}
	}

	///
	///
	///
	pub fn analyse_ast(&mut self, ast: &mut Ast) -> Result<(), Vec<CompilationError>> {
		for module in ast.modules.iter_mut() {
			self.analyse_module(module);
		}

		if self.errors.len() > 0 {
			Err(self.errors.clone())
		} else {
			Ok(())
		}
	}

	///
	///
	///
	pub fn analyse_module(&mut self, module: &mut Module) {
		for definition in module.definitions.iter_mut() {
			self.analyse_definition(definition);
		}
	}

	///
	///
	///
	pub fn analyse_definition(&mut self, definition: &mut Definition) {
		match definition {
			Definition::Function(ref mut function) => {
				self.analyse_function(function);
			}
		}
	}

	///
	///
	///
	pub fn analyse_function(&mut self, function: &mut Function) {
		let name = function.get_name();
		let signature = FunctionSignature::new_from_function(function.clone());

		if self.signatures.contains_key(&name) {
			self.errors.push(lookup_error(ErrorCode::E0401,
				function.get_location(),
				format!("Function {} is already defined",
					function.get_name())));
			return;
		}

		self.signatures.insert(name, signature);

		let index = self.symbol_table.new_sub_table(false);
		self.symbol_path.push(index);

		let path = VecDeque::from_iter(self.symbol_path.clone());

		for argument in function.arguments.iter() {
			match self.symbol_table.push_argument(argument.clone(), path.clone()) {
				Ok(_) => {},
				Err(error) => {
					self.errors.push(error);
				}
			}
		}

		self.cur_ret_type = function.return_type;
		self.analyse_block(&mut function.body, None);

		if function.return_type != TypeHint::None && !self.has_returned {
			let error = type_error(ErrorCode::E0200,
				function.get_location(),
				format!("Function {} declares a return type but does not return a value.",
					function.get_name()));
			self.errors.push(error);
		}

		self.cur_ret_type = TypeHint::None;
		self.has_returned = false;

		self.symbol_path.pop();
	}

	///
	///
	///
	fn analyse_block(&mut self, block: &mut Block, loop_id: Option<usize>) {
		for statement in block.statements.iter_mut() {
			self.analyse_statement(statement, loop_id);
		}
	}

	///
	///
	///
	fn analyse_statement(&mut self, statement: &mut Statement, loop_id: Option<usize>) {
		match statement {
			Statement::Let(let_stmt) => {
				match self.analyse_expression(&mut let_stmt.value) {
					Ok(type_hint) => {
						if type_hint != let_stmt.type_hint {
							self.errors.push(type_error(ErrorCode::E0207,
								let_stmt.get_location(),
								format!("Let statement value for {} does not match the given type hint",
									let_stmt.get_name())))
						}
					},
					Err(_) => {},
				}

				let path = VecDeque::from_iter(self.symbol_path.clone());

				match self.symbol_table.push_local(let_stmt.clone(), path) {
					Ok(_) => {},
					Err(error) => {
						self.errors.push(error);
					}
				}
			},
			Statement::Return(ref mut expr) => {
				self.has_returned = true;

				if self.cur_ret_type == TypeHint::None {
					let error = type_error(ErrorCode::E0201,
						// TODO: give return it's own location
						expr.get_location(),
						"A value was returned from a function with no return type".to_string());
					self.errors.push(error);
				} else {
					match self.analyse_expression(expr) {
						Ok(type_hint) => {
							if type_hint != self.cur_ret_type {
								self.errors.push(type_error(ErrorCode::E0206,
									// TODO: give return it's own location
									expr.get_location(),
									format!("Unexpected type for return expression. Expected: {}. Found: {}",
										self.cur_ret_type,
										type_hint)));
							}
						},
						Err(_) => {},
					}
				}
			}
			Statement::If(if_stmt) => {
				for branch in if_stmt.branches.iter_mut() {
					if branch.condition.is_assignment() {
						self.errors.push(syntax_error(ErrorCode::E0006,
							branch.get_location(),
							"Assignment expression used for conditional in if statement".to_string()));
					}

					match self.analyse_expression(&mut branch.condition) {
						Ok(type_hint) => {
							if type_hint != TypeHint::Bool {
								self.errors.push(type_error(ErrorCode::E0204,
									branch.get_location(),
									format!("Unexpected type for conditional in if statement. Expected: bool. Found: {}",
										type_hint)));
							}
						},
						Err(_) => {},
					}

					let index = self.symbol_table.new_sub_table(true);
					self.symbol_path.push(index);

					self.analyse_block(&mut branch.consequence, loop_id);

					self.symbol_path.pop();
				}
			}
			Statement::Loop(ref mut loop_stmt) => {
				let index = self.symbol_table.new_sub_table(true);
				self.symbol_path.push(index);

				let loop_id = loop_stmt.get_id();
				self.analyse_block(&mut loop_stmt.body, Some(loop_id));

				self.symbol_path.pop();
			}
			Statement::Break(ctrl) => {
				if loop_id.is_none() {
					self.errors.push(syntax_error(ErrorCode::E0004,
						ctrl.get_location(),
						"Break statement found outside of loop".to_string()));
				} else {
					ctrl.set_loop_id(loop_id.unwrap());
				}
			}
			Statement::Continue(ctrl) => {
				if loop_id.is_none() {
					self.errors.push(syntax_error(ErrorCode::E0005,
						ctrl.get_location(),
						"Continue statement found outside of loop".to_string()));
				} else {
					ctrl.set_loop_id(loop_id.unwrap());
				}
			}
			Statement::Expression(ref mut expr) => {
				match self.analyse_expression(expr) {
					_ => {},
				}
			}
		}
	}

	///
	///
	///
	fn analyse_expression(&mut self, expression: &mut Expression) -> Result<TypeHint, ()> {
		match expression {
			Expression::Literal(literal) => {
				Ok(literal.type_hint)
			},
			Expression::Identifier(ref mut ident) => {
				let path = VecDeque::from_iter(self.symbol_path.clone());

				match self.symbol_table.lookup_symbol(ident, path) {
					Ok(type_hint) => {
						Ok(type_hint)
					},
					Err(error) => {
						self.errors.push(error);
						Err(())
					},
				}
			},
			Expression::Prefix(ref mut prefix) => {
				if prefix.right.is_assignment() {
					self.errors.push(syntax_error(ErrorCode::E0006,
						prefix.right.get_location(),
						"Assignment expression given as prefix expression operand".to_string()));
					return Err(())
				}

				match self.analyse_expression(&mut prefix.right) {
					Ok(type_hint) => {
						// TODO: check type is compatible with the operator
						Ok(type_hint)
					},
					Err(_) => Err(()),
				}
			},
			Expression::Infix(ref mut infix) => {
				match infix.expand_assignment() {
					Ok(_) => {},
					Err(error) => {
						self.errors.push(error);
					}
				}

				if infix.left.is_assignment() {
					self.errors.push(syntax_error(ErrorCode::E0006,
						infix.left.get_location(),
						"Assignment expression given as infix expression operand".to_string()));
					return Err(());
				}

				if infix.right.is_assignment() {
					self.errors.push(syntax_error(ErrorCode::E0006,
						infix.right.get_location(),
						"Assignment expression given as infix expression operand".to_string()));
					return Err(());
				}

				match self.analyse_expression(&mut infix.left) {
					Ok(left_type_hint) => {
						match self.analyse_expression(&mut infix.right) {
							Ok(right_type_hint) => {
								if left_type_hint != right_type_hint {
									self.errors.push(type_error(ErrorCode::E0205,
										infix.get_location(),
										format!("Mismatched types for infix expression operands: {} != {}",
											left_type_hint,
											right_type_hint)))
								}

								// TODO: check types are compatible with operator

								Ok(left_type_hint)
							},
							Err(_) => Err(()),
						}
					},
					Err(_) => Err(()),
				}
			},
			Expression::Call(call) => {
				let name = call.get_name();
				let mut arg_types: Vec<TypeHint> = Vec::new();

				for arg in call.arguments.iter_mut() {
					match self.analyse_expression(arg) {
						Ok(type_hint) => {
							arg_types.push(type_hint);
						},
						Err(_) => {},
					}

					if arg.is_assignment() {
						self.errors.push(syntax_error(ErrorCode::E0006,
							arg.get_location(),
							"Assignment expression given as function argument".to_string()));
						return Err(())
					}
				}

				// exit early as having the wrong number will present false positives
				if arg_types.len() != call.arguments.len() {
					return Err(())
				}


				if self.signatures.contains_key(&name) {
					let signature = &self.signatures[&name].clone();
					let signature_args = signature.args().clone();

					if signature_args.len() != arg_types.len() {
						let error = type_error(ErrorCode::E0202,
							call.get_location(),
							format!("Expected {} arguments when calling function: {}. Found {} arguments",
								signature_args.len(),
								call.get_name(),
								arg_types.len()));
						self.errors.push(error);
					} else {
						for (index, expected) in signature_args.iter().enumerate() {
							match self.analyse_expression(&mut call.arguments[index]) {
								Ok(actual) => {
									if actual != *expected {
										self.errors.push(type_error(ErrorCode::E0203,
											call.arguments[index].get_location(),
											format!("Unexpected type for argument {} in function call {}. Expected: {}, found: {}",
												index + 1,
												call.get_name(),
												expected,
												actual)))
									}
								},
								Err(_) => {},
							}
						}
					}

					Ok(signature.ret_type())
				} else {
					let error = lookup_error(ErrorCode::E0400,
						call.get_location(),
						format!("Function {} is not defined before it was called",
							call.get_name()));
					self.errors.push(error);

					Err(())
				}
			}
		}
	}
}

#[cfg(test)]
mod test {
	use crate::compiler::parser::Parser;
	use super::*;

	//
	//
	//
	fn analyse(input: &str) -> Result<Ast, Vec<CompilationError>> {
		let mut parser = Parser::new(input);

		match parser.parse() {
			Ok(ref mut ast) => {
				let mut analyser = Analyser::new();

				match analyser.analyse_ast(ast) {
					Ok(_) => {
						Ok(ast.clone())
					},
					Err(errors) => Err(errors),
				}

			},
			Err(errors) => {
				for error in errors.iter() {
					println!("{}", error);
				}

				panic!("Unexpected error(s) during parsing");
			}
		}
	}

	#[test]
	fn test_simple() {
		let res = analyse("fn x() {}");
		assert!(res.is_ok());
	}

	#[test]
	fn test_undefined_function_call() {
		let res = analyse("fn x() { invalid(); }");
		assert!(res.is_err());
	}

	#[test]
	fn test_defined_function_call() {
		let res = analyse("fn exists() {} fn x() { exists(); }");
		assert!(res.is_ok());
	}

	#[test]
	fn test_missing_return_statement() {
		let res = analyse("fn x() -> i32 {}");
		assert!(res.is_err());
	}

	#[test]
	fn test_with_return_statement() {
		let res = analyse("fn x() -> i32 { return 5; }");
		assert!(res.is_ok());
	}

	#[test]
	fn test_missing_return_type() {
		let res = analyse("fn x() { return 5; }");
		assert!(res.is_err());
	}

	#[test]
	fn test_incorrect_return_type() {
		let res = analyse("fn x() -> bool { return 5; }");
		assert!(res.is_err());
	}

	#[test]
	fn test_redefined_function() {
		let res = analyse("fn x() {} fn x() {}");
		assert!(res.is_err());
	}

	#[test]
	fn test_incorrect_num_args() {
		let res = analyse("fn x() { x(5); }");
		assert!(res.is_err());
	}

	#[test]
	fn test_incorrect_type_args() {
		let res = analyse("fn x(a: i32) { x(true); }");
		assert!(res.is_err());
	}

	#[test]
	fn test_break_outside_loop() {
		let res = analyse("fn x() { break; }");
		assert!(res.is_err());
	}

	#[test]
	fn test_break_inside_loop() {
		let res = analyse("fn x() { loop { break; } }");
		assert!(res.is_ok());
	}

	#[test]
	fn test_continue_outside_loop() {
		let res = analyse("fn x() { continue; }");
		assert!(res.is_err());
	}

	#[test]
	fn test_continue_inside_loop() {
		let res = analyse("fn x() { loop { continue; } }");
		assert!(res.is_ok());
	}

	#[test]
	fn test_conditional_non_bool() {
		let res = analyse("fn x() { if 1 {} }");
		assert!(res.is_err());
	}

	#[test]
	fn test_conditional_bool() {
		let res = analyse("fn x() { if true {} }");
		assert!(res.is_ok());
	}

	#[test]
	fn test_conditional_assignment() {
		let res = analyse("fn x() { if x = true {} }");
		assert!(res.is_err());
	}

	#[test]
	fn test_infix_assignment() {
		let res = analyse("fn x() { 5 == x = 5; }");
		assert!(res.is_err());
	}

	#[test]
	fn test_prefix_assignment() {
		let res = analyse("fn x() { -a = true; }");
		assert!(res.is_err());
	}

	#[test]
	fn test_defined_argument() {
		let res = analyse("fn x(a: i32) { a; }");
		assert!(res.is_ok());
	}

	#[test]
        fn test_defined_local() {
                let res = analyse("fn x() { let a: i32 = 5; a; }");
                assert!(res.is_ok());
        }

	#[test]
        fn test_undefined_symbol() {
                let res = analyse("fn x() { a; }");
                assert!(res.is_err());
        }

	#[test]
        fn test_defined_argument_access_in_block() {
                let res = analyse("fn x(a: i32) { if true { a; } }");
                assert!(res.is_ok());
        }

	#[test]
        fn test_defined_local_access_in_block() {
                let res = analyse("fn x() { let a: i32 = 5; if true { a; } }");
                assert!(res.is_ok());
        }

	#[test]
        fn test_defined_local_in_block() {
                let res = analyse("fn x() { if true { let a: i32 = 5; a; } }");
	        //assert!(res.is_ok());

		match res {
			Ok(_) => {},
			Err(errors) => {
				for error in errors.iter() {
					println!("{}", error);
				}

				panic!("Unexpected failure");
			},
		}
        }

	#[test]
        fn test_defined_local_in_block_access_outside_block() {
                let res = analyse("fn x() { if true { let a: i32 = 5; } a; }");
                assert!(res.is_err());
        }
}
