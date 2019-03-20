//!
//!
//!

use std::collections::VecDeque;

use crate::error::{CompilationError, ErrorCode, lookup_error, internal_error};
use crate::location::Location;
use crate::compiler::parser::{Argument, Identifier, TypeHint, Let, TAstNode};


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
	pub fn new_sub_table(&mut self, inherit_offset: bool, path: VecDeque<usize>) -> Result<usize, CompilationError> {
		if path.len() > 0 {
			let mut sub_path = path.clone();
			let sub_index = sub_path.pop_front().unwrap();

			match self.items[sub_index] {
				SymbolItem::Table(ref mut table) => table.new_sub_table(inherit_offset, sub_path),
				_ => {
					Err(internal_error(ErrorCode::E1006,
						Location::end(),
						format!("Expected table at index: {:?}",
							sub_index)))
				},
			}
		} else {
			let offset = if inherit_offset { self.local_offset } else { 0 };
			let table = SymbolTable::new(offset);
			let item = SymbolItem::Table(table);
			let index = self.items.len();

			self.items.push(item);

			Ok(index)
		}
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

	///
	///
	///
	pub fn lookup_symbol(&self, ident: &mut Identifier, path: VecDeque<usize>) -> Result<TypeHint, CompilationError> {
		for (index, item) in self.items.iter().enumerate() {
			match item {
				SymbolItem::Symbol(symbol) => {
					if symbol.name == ident.get_name() {
						ident.set_symbol_type(symbol.symbol_type);
						// TODO: set offset as well

						return Ok(symbol.type_hint);
					}
				}
				SymbolItem::Table(sub_table) => {
					if path.len() > 0 && path[0] == index {
						let mut sub_path = path.clone();
						sub_path.pop_front();

						return sub_table.lookup_symbol(ident, sub_path);
					}
				}
			}
		}

		Err(lookup_error(ErrorCode::E0404,
			ident.get_location(),
			format!("Symbol {:?} was used before it was defined",
				ident.get_name())))
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


#[cfg(test)]
mod test {
	use crate::compiler::parser::{Expression, Literal, LiteralValue};
	use super::*;

	const LITERAL: Expression = Expression::Literal(
		Literal {
			type_hint: TypeHint::Int32,
			value: LiteralValue::True,
			location: Location {
				line: 1,
				column: 1,
			}
		}
	);

	macro_rules! loc {
		($line:tt, $column:tt) => (
			Location::new($line, $column)
		)
	}

	macro_rules! ident {
		($value:tt) => (
			Identifier::new($value.to_string(), loc!(1, 1))
		)
	}

	macro_rules! arg {
		($value:tt) => (
			Argument::new(ident!($value), TypeHint::Bool)
		)
	}

	macro_rules! let_stmt {
		($value:tt) => (
			Let::new(ident!($value), TypeHint::Bool, LITERAL, loc!(1, 1))
		)
	}

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
							type_hint: TypeHint::Bool,
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
		let sub_table_index = match table.new_sub_table(false, path.clone()) {
			Ok(index) => index,
			Err(error) => panic!("{}", error),
		};
		path.push_back(sub_table_index);

		let res = table.push_argument(arg!("example"), path);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_argument_redefined() {
		let mut table = SymbolTable::new(0);

		let mut path = VecDeque::new();
		let sub_table_index = match table.new_sub_table(false, path.clone()) {
			Ok(index) => index,
			Err(error) => panic!("{}", error),
		};
		path.push_back(sub_table_index);

		let res = table.push_argument(arg!("example"), path.clone());
		assert!(res.is_ok());

		let res = table.push_argument(arg!("example"), path.clone());
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
							type_hint: TypeHint::Bool,
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
		let sub_table_index = match table.new_sub_table(false, path.clone()) {
			Ok(index) => index,
			Err(error) => panic!("{}", error),
		};
		path.push_back(sub_table_index);

		let res = table.push_local(let_stmt!("example"), path);

		assert!(res.is_ok());
		assert_eq!(table, expected);
	}

	//
	#[test]
	fn test_push_local_redefined() {
		let mut table = SymbolTable::new(0);

		let mut path = VecDeque::new();
		let sub_table_index = match table.new_sub_table(false, path.clone()) {
			Ok(index) => index,
			Err(error) => panic!("{}", error),
		};
		path.push_back(sub_table_index);

		let res = table.push_local(let_stmt!("example"), path.clone());
		assert!(res.is_ok());

		let res = table.push_local(let_stmt!("example"), path.clone());
		assert!(res.is_err());
	}

	/*
	#[test]
	#[ignore]
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
