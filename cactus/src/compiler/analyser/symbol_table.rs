//!
//!
//!

use std::collections::VecDeque;

use crate::error::{CompilationError, ErrorCode, lookup_error, internal_error};
use crate::location::Location;
use crate::compiler::parser::{Argument, TypeHint, Let, TAstNode};


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
	items: Vec<SymbolItem>,
	argument_offset: usize,
	local_offset: usize,
}

impl SymbolTable {
	///
	///
	///
	pub fn new(local_offset: usize) -> SymbolTable {
		SymbolTable {
			items: Vec::new(),
			argument_offset: 0,
			local_offset: local_offset,
		}
	}

	//
	//
	//
	fn contains(&self, name: String) -> bool {
		for item in self.items.iter() {
			match item {
				SymbolItem::Symbol(symbol) => {
					if symbol.name == name {
						return true;
					}
				}
				// no need to check sub-tables
				SymbolItem::Table(_) => {},
			};
		}

		false
	}

	///
	///
	///
	pub fn new_sub_table(&mut self, inherif_offset: bool) -> usize {
		let offset = if inherif_offset { self.local_offset } else { 0 };
		let table = SymbolTable::new(offset);
		let item = SymbolItem::Table(table);
		let index = self.items.len();

		self.items.push(item);

		index
	}

	/// Add an argument to the symbol table.
	///
	///
	pub fn push_argument(&mut self, argument: Argument, path: VecDeque<usize>) -> Result<usize, CompilationError> {
		if path.len() > 0 {
			let mut sub_path = path.clone();
			let sub_index = sub_path.pop_front().unwrap();

			match self.items[sub_index] {
				SymbolItem::Table(ref mut table) => table.push_argument(argument, sub_path),
				_ => {
					Err(internal_error(ErrorCode::E1006,
						Location::end(),
						format!("Expected table at index: {:?}",
							sub_index)))
				},
			}
		} else {
			if self.contains(argument.get_name()) {
				Err(lookup_error(ErrorCode::E0402,
					argument.get_location(),
					format!("Argument {} already defined",
						argument.get_name())))
			} else {
				let symbol = Symbol::new(argument.get_name(), SymbolType::Argument, argument.get_type_hint());
				let item = SymbolItem::Symbol(symbol);

				self.items.push(item);
				self.argument_offset += 1; // TODO: test this

				Ok(self.argument_offset - 1)
			}
		}
	}

	/// Add an argument to the symbol table.
	///
	///
	pub fn push_local(&mut self, let_stmt: Let, path: VecDeque<usize>) -> Result<usize, CompilationError> {
		if path.len() > 0 {
			let mut sub_path = path.clone();
			let sub_index = sub_path.pop_front().unwrap();

			match self.items[sub_index] {
				SymbolItem::Table(ref mut table) => table.push_local(let_stmt, sub_path),
				_ => {
					Err(internal_error(ErrorCode::E1006,
						Location::end(),
						format!("Expected table at index: {:?}",
							sub_index)))
				},
			}
		} else {
			if self.contains(let_stmt.get_name()) {
				Err(lookup_error(ErrorCode::E0403,
					let_stmt.get_location(),
					format!("Local {} already defined",
						let_stmt.get_name())))
			} else {
				let symbol = Symbol::new(let_stmt.get_name(), SymbolType::Local, let_stmt.get_type_hint());
				let item = SymbolItem::Symbol(symbol);

				self.items.push(item);
				self.local_offset += 1; // TODO: test this

				Ok(self.local_offset - 1)
			}
		}
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum SymbolItem {
	Table(SymbolTable),
	Symbol(Symbol)
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
	name: String,
	symbol_type: SymbolType,
	type_hint: TypeHint,
}

impl Symbol {
	///
	///
	///
	pub fn new(name: String, symbol_type: SymbolType, type_hint: TypeHint) -> Symbol {
		Symbol {
			name: name,
			symbol_type: symbol_type,
			type_hint: type_hint,
		}
	}
}

///
///
///
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SymbolType {
	Function,
	Argument,
	Local,
	// for the initial state of identifiers in the AST
	Unknown,
}


/*

	///
	///
	///
	pub fn lookup_symbol(&self, name: String, path: VecDeque<usize>, is_function: bool) -> Result<TypeHint, CompilationError> {
		for (index, item) in self.items.iter().enumerate() {
			match item {
				SymbolItem::Symbol(symbol) => {
					if symbol.name == name {
						return Ok(symbol.type_hint);
					}
				}
				SymbolItem::Table(sub_table) => {
					// functions calls should never need to check sub-tables
					// as we don't allow nested function definitions
					if is_function {
						continue;
					}

					if path.len() > 0 && path[0] == index {
						let mut sub_path = path.clone();
						sub_path.pop_front();

						return sub_table.lookup_symbol(name.clone(), sub_path, false);
					}
				}
			}
		}

		return Err(lookup_error!(
			ErrorCode::E0403,
			Location::end(),
			"Symbol {:?} was used before it was defined",
			name
		));
	}
}
*/

#[cfg(test)]
mod test {
	/*
	use super::*;

	//
	#[test]
	fn test_push_argument() {
		let mut table = SymbolTable::new(0);
		let expected = SymbolTable {
			items: vec![
				SymbolItem::Table(SymbolTable {
					items: vec![
						SymbolItem::Symbol(Symbol {
							name: "example".to_string(),
							symbol_type: SymbolType::Argument,
							type_hint: TypeHint::Int32,
						})
					],
					argument_offset: 1,
					local_offset: 0,
				})
			],
			argument_offset: 0,
			local_offset: 0,
		};

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table(false);
		path.push_back(sub_table_index);

		let name = "example".to_string();
		let res = table.push_argument(name, path, TypeHint::Int32);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_argument_redefined() {
		let mut table = SymbolTable::new(0);

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table(false);
		path.push_back(sub_table_index);

		let res = table.push_argument("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_ok());

		let res = table.push_argument("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_err());
	}

	//
	#[test]
	fn test_push_local() {
		let mut table = SymbolTable::new(0);
		let expected = SymbolTable {
			items: vec![
				SymbolItem::Table(SymbolTable {
					items: vec![
						SymbolItem::Symbol(Symbol {
							name: "example".to_string(),
							symbol_type: SymbolType::Local,
							type_hint: TypeHint::Int32,
						})
					],
					argument_offset: 0,
					local_offset: 1,
				})
			],
			argument_offset: 0,
			local_offset: 0,
		};

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table(false);
		path.push_back(sub_table_index);

		let name = "example".to_string();
		let res = table.push_local(name, path, TypeHint::Int32);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_local_redefined() {
		let mut table = SymbolTable::new(0);

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table(false);
		path.push_back(sub_table_index);

		let res = table.push_local("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_ok());

		let res = table.push_local("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_err());
	}

	#[test]
	fn test_lookup_symbol_function() {
		let table = SymbolTable {
			items: vec![
				SymbolItem::Symbol(Symbol {
					name: "example".to_string(),
					symbol_type: SymbolType::Local,
					type_hint: TypeHint::Int32,
				}),
				SymbolItem::Table(SymbolTable {
					items: vec![
						SymbolItem::Symbol(Symbol {
							name: "test".to_string(),
							symbol_type: SymbolType::Local,
							type_hint: TypeHint::Int32,
						}),
					],
					argument_offset: 0,
					local_offset: 0,
				}),
			],
			argument_offset: 0,
			local_offset: 0,
		};

		let mut path: VecDeque<usize> = VecDeque::new();

		// without a path, only the function lookup should pass
		assert!(table.lookup_symbol("example".to_string(), path.clone(), true).is_ok());
		assert!(table.lookup_symbol("test".to_string(), path.clone(), false).is_err());

		path.push_back(1);

		// with a path, both lookups should pass
		assert!(table.lookup_symbol("example".to_string(), path.clone(), true).is_ok());
		assert!(table.lookup_symbol("test".to_string(), path.clone(), false).is_ok());

		// check that lookups for non-existent symbols fail
		assert!(table.lookup_symbol("not_found".to_string(), path.clone(), true).is_err());
		assert!(table.lookup_symbol("not_found".to_string(), path.clone(), false).is_err());
	}
	*/
}
