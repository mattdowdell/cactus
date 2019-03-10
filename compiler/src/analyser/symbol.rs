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
	analyser::type_checker::TypeHint,
};


/// A table that holds all symbols in a Cactus program.
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
	pub items: Vec<SymbolItem>,
	argument_offset: usize,
	local_offset: usize,
}

impl SymbolTable {
	/// Initialise a new `SymbolTable`.
	///
	///
	pub fn new() -> SymbolTable {
		SymbolTable {
			items: Vec::new(),
			argument_offset: 0,
			local_offset: 0,
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

	/// Add a new `SymbolTable` to the existing table and return the new table's index.
	///
	///
	pub fn sub_table(&mut self) -> usize {
		let table = SymbolTable::new();
		let item = SymbolItem::Table(table);

		self.items.push(item);

		// return the index of the new table
		self.items.len() - 1
	}

	/// Add a function to the `SymbolTable`.
	///
	///
	pub fn push_function(&mut self, name: String, return_type: TypeHint) -> Result<(), CompilationError> {
		if self.contains(name.clone()) {
			Err(lookup_error!(
				ErrorCode::E0400,
				Location::end(),
				"Function {:?} already defined",
				name
			))
		} else {
			let symbol = Symbol::new(name, SymbolType::Function, return_type);
			let item = SymbolItem::Symbol(symbol);

			self.items.push(item);

			Ok(())
		}
	}

	/// Add an argument to the `SymbolTable`.
	///
	///
	pub fn push_argument(&mut self, name: String, path: VecDeque<usize>, type_hint: TypeHint) -> Result<usize, CompilationError> {
		if path.len() > 0 {
			let mut sub_path = path.clone();
			let sub_index = sub_path.pop_front().unwrap();

			match self.items[sub_index] {
				SymbolItem::Table(ref mut table) => table.push_argument(name, sub_path, type_hint),
				_ => panic!("Internal error: Expected table at index: {:?}", sub_index),
			}
		} else {
			if self.contains(name.clone()) {
				Err(lookup_error!(
					ErrorCode::E0401,
					Location::end(),
					"Argument {:?} already defined",
					name
				))
			} else {
				let symbol = Symbol::new(name, SymbolType::Argument, type_hint);
				let item = SymbolItem::Symbol(symbol);

				self.items.push(item);
				self.argument_offset += 1; // TODO: test this

				Ok(self.argument_offset - 1)
			}
		}
	}

	/// Add a local variable to the `SymbolTable`.
	///
	///
	pub fn push_local(&mut self, name: String, path: VecDeque<usize>, type_hint: TypeHint) -> Result<usize, CompilationError> {
		if path.len() > 0 {
			let mut sub_path = path.clone();
			let sub_index = sub_path.pop_front().unwrap();

			match self.items[sub_index] {
				SymbolItem::Table(ref mut table) => table.push_local(name, sub_path, type_hint),
				_ => panic!("Internal error: Expected table at index: {:?}", sub_index),
			}
		} else {
			if self.contains(name.clone()) {
				Err(lookup_error!(
					ErrorCode::E0401,
					Location::end(),
					"Local variable {:?} already defined",
					name
				))
			} else {
				let symbol = Symbol::new(name, SymbolType::Local, type_hint);
				let item = SymbolItem::Symbol(symbol);

				self.items.push(item);
				self.local_offset += 1; // TODO: test this

				Ok(self.local_offset - 1)
			}
		}
	}

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

/// An item in the symbol table.
///
/// Items can either be a sub-table or a symbol.
#[derive(Clone, Debug, PartialEq)]
pub enum SymbolItem {
	Symbol(Symbol),
	Table(SymbolTable),
}


/// A single symbol in the symbol table.
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
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SymbolType {
	Function,
	Argument,
	Local,
	Unknown,
}


#[cfg(test)]
mod test {
	use super::*;

	//
	#[test]
	fn test_push_function() {
		let mut table = SymbolTable::new();
		let expected = SymbolTable {
			items: vec![
				SymbolItem::Symbol(Symbol {
					name: "example".to_string(),
					symbol_type: SymbolType::Function,
					type_hint: TypeHint::Int32,
				})
			],
			argument_offset: 0,
			local_offset: 0,
		};

		let name = "example".to_string();
		let res = table.push_function(name, TypeHint::Int32);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_function_redefined() {
		let mut table = SymbolTable::new();

		let res = table.push_function("example".to_string(), TypeHint::Int32);
		assert!(res.is_ok());

		let res = table.push_function("example".to_string(), TypeHint::Int32);
		assert!(res.is_err());
	}

	//
	#[test]
	fn test_push_argument() {
		let mut table = SymbolTable::new();
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
		let sub_table_index = table.sub_table();
		path.push_back(sub_table_index);

		let name = "example".to_string();
		let res = table.push_argument(name, path, TypeHint::Int32);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_argument_redefined() {
		let mut table = SymbolTable::new();

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table();
		path.push_back(sub_table_index);

		let res = table.push_argument("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_ok());

		let res = table.push_argument("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_err());
	}

	//
	#[test]
	fn test_push_local() {
		let mut table = SymbolTable::new();
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
		let sub_table_index = table.sub_table();
		path.push_back(sub_table_index);

		let name = "example".to_string();
		let res = table.push_local(name, path, TypeHint::Int32);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_local_redefined() {
		let mut table = SymbolTable::new();

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table();
		path.push_back(sub_table_index);

		let res = table.push_local("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_ok());

		let res = table.push_local("example".to_string(), path.clone(), TypeHint::Int32);
		assert!(res.is_err());
	}

	//
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
}
