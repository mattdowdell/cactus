//!
//!
//!

use std::collections::VecDeque;


/// A table that holds all symbols in a Cactus program.
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
	pub items: Vec<SymbolItem>,
}

impl SymbolTable {
	/// Initialise a new `SymbolTable`.
	///
	///
	pub fn new() -> SymbolTable {
		SymbolTable {
			items: Vec::new(),
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
	pub fn push_function(&mut self, name: String) -> Result<(), String> {
		if self.contains(name.clone()) {
			Err(format!("Error: Function {:?} already defined", name))
		} else {
			let symbol = Symbol::new(name, SymbolType::Function);
			let item = SymbolItem::Symbol(symbol);

			self.items.push(item);

			Ok(())
		}
	}

	/// Add an argument to the `SymbolTable`.
	///
	///
	pub fn push_argument(&mut self, name: String, path: VecDeque<usize>) -> Result<(), String> {
		if path.len() > 0 {
			let mut sub_path = path.clone();
			let sub_index = sub_path.pop_front().unwrap();

			match self.items[sub_index] {
				SymbolItem::Table(ref mut table) => table.push_argument(name, sub_path),
				_ => panic!("Internal error: Expected table at index: {:?}", sub_index),
			}
		} else {
			if self.contains(name.clone()) {
				Err(format!("Error: Argument {:?} already defined", name))
			} else {
				let symbol = Symbol::new(name, SymbolType::Argument);
				let item = SymbolItem::Symbol(symbol);

				self.items.push(item);

				Ok(())
			}
		}
	}

	/// Add a local variable to the `SymbolTable`.
	///
	///
	pub fn push_local(&mut self, name: String, path: VecDeque<usize>) -> Result<(), String> {
		if path.len() > 0 {
			let mut sub_path = path.clone();
			let sub_index = sub_path.pop_front().unwrap();

			match self.items[sub_index] {
				SymbolItem::Table(ref mut table) => table.push_local(name, sub_path),
				_ => panic!("Internal error: Expected table at index: {:?}", sub_index),
			}
		} else {
			if self.contains(name.clone()) {
				Err(format!("Error: Local variable {:?} already defined", name))
			} else {
				let symbol = Symbol::new(name, SymbolType::Local);
				let item = SymbolItem::Symbol(symbol);

				self.items.push(item);

				Ok(())
			}
		}
	}

	///
	///
	///
	pub fn lookup_symbol(&self, name: String, path: VecDeque<usize>, is_function: bool) -> bool {
		for (index, item) in self.items.iter().enumerate() {
			match item {
				SymbolItem::Symbol(symbol) => {
					if symbol.name == name {
						return true;
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

						if sub_table.lookup_symbol(name.clone(), sub_path, false) {
							return true;
						}
					}
				}
			}
		}

		return false;
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
}

impl Symbol {
	///
	///
	///
	pub fn new(name: String, symbol_type: SymbolType) -> Symbol {
		Symbol {
			name: name,
			symbol_type: symbol_type,
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
				})
			],
		};

		let name = "example".to_string();
		let res = table.push_function(name);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_function_redefined() {
		let mut table = SymbolTable::new();

		let res = table.push_function("example".to_string());
		assert!(res.is_ok());

		let res = table.push_function("example".to_string());
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
						})
					],
				})
			],
		};

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table();
		path.push_back(sub_table_index);

		let name = "example".to_string();
		let res = table.push_argument(name, path);

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

		let res = table.push_argument("example".to_string(), path.clone());
		assert!(res.is_ok());

		let res = table.push_argument("example".to_string(), path.clone());
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
						})
					],
				})
			],
		};

		let mut path = VecDeque::new();
		let sub_table_index = table.sub_table();
		path.push_back(sub_table_index);

		let name = "example".to_string();
		let res = table.push_local(name, path);

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

		let res = table.push_local("example".to_string(), path.clone());
		assert!(res.is_ok());

		let res = table.push_local("example".to_string(), path.clone());
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
				}),
				SymbolItem::Table(SymbolTable {
					items: vec![
						SymbolItem::Symbol(Symbol {
							name: "test".to_string(),
							symbol_type: SymbolType::Local,
						}),
					],
				}),
			],
		};

		let mut path: VecDeque<usize> = VecDeque::new();

		// without a path, only the function lookup should pass
		assert!(table.lookup_symbol("example".to_string(), path.clone(), true));
		assert!(!table.lookup_symbol("test".to_string(), path.clone(), false));

		path.push_back(1);

		// with a path, both lookups should pass
		assert!(table.lookup_symbol("example".to_string(), path.clone(), true));
		assert!(table.lookup_symbol("test".to_string(), path.clone(), false));

		// check that lookups for non-existent symbols fail
		assert!(!table.lookup_symbol("not_found".to_string(), path.clone(), true));
		assert!(!table.lookup_symbol("not_found".to_string(), path.clone(), false));
	}
}
