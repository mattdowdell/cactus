//!
//!
//!

use std::collections::HashMap;

use parser::ast::{
	Ast,
	Definition,
	Block,
	Statement,
};


/// A table that holds all symbols in a Cactus program.
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
	items: HashMap<String, SymbolItem>,
	block_index: usize,
	address_index: usize,
}

impl SymbolTable {
	/// Initialise a new `SymbolTable`.
	///
	///
	pub fn new() -> SymbolTable {
		SymbolTable {
			items: HashMap::new(),
			block_index: 0,
			address_index: 0,
		}
	}

	//
	//
	//
	fn next_address(&mut self) -> usize {
		let ret = self.address_index;
		self.address_index += 1;

		ret
	}

	///
	///
	///
	pub fn new_block(&mut self) -> (String, SymbolTable) {
		let name = format!("block{}", self.block_index);
		let table = SymbolTable::new();

		self.block_index += 1;

		(name, table)
	}

	/// Add an item to the symbol table.
	///
	///
	pub fn insert(&mut self, name: String, item: SymbolItem) {
		self.items.insert(name, item);
	}

	/// Add a function argument to the symbol table.
	///
	///
	pub fn insert_argument(&mut self, name: String) {
		let symbol = Symbol::new(SymbolType::Argument, self.next_address());
		let item = SymbolItem::Symbol(symbol);

		self.insert(name, item);
	}

	/// Add a local variable to the symbol table.
	///
	///
	pub fn insert_local(&mut self, name: String) {
		let symbol = Symbol::new(SymbolType::Local, self.next_address());
		let item = SymbolItem::Symbol(symbol);

		self.insert(name, item);
	}

	/// Add a sub-symbol table to the symbol table.
	///
	///
	pub fn insert_table(&mut self, name: String, table: SymbolTable) {
		let item = SymbolItem::Table(table);

		self.insert(name, item);
	}

	///
	///
	///
	pub fn convert_ast(&mut self, ast: Ast) -> Result<(), String> {
		for module in ast.modules.iter() {
			for definition in module.definitions.iter() {
				self.convert_definition(definition.clone())?;
			}
		}

		Ok(())
	}

	//
	//
	//
	fn convert_definition(&mut self, definition: Definition) -> Result<(), String> {
		match definition {
			Definition::Import(_) => Err("Imports are not yet supported".to_string()),
			Definition::Function(function) => {
				let name = function.identifier.name.clone();
				let mut sub_table = SymbolTable::new();

				for arg in function.arguments.iter() {
					let arg_name = arg.identifier.name.clone();
					sub_table.insert_argument(arg_name);
				}

				sub_table.convert_block(function.body)?;

				self.insert_table(name, sub_table);

				Ok(())
			},
			_ => Ok(()),
		}
	}

	//
	//
	//
	fn convert_block(&mut self, block: Block) -> Result<(), String> {
		for statement in block.statements.iter() {
			self.convert_statement(statement.clone())?;
		}

		Ok(())
	}

	//
	//
	//
	fn convert_statement(&mut self, statement: Statement) -> Result<(), String> {
		match statement {
			Statement::Break
			| Statement::Continue => Ok(()),
			Statement::Return(_)     => Ok(()),
			Statement::Expression(_) => Ok(()),

			Statement::Loop(block)      => {
				let (name, mut sub_table) = self.new_block();
				sub_table.convert_block(block.clone())?;
				self.insert_table(name, sub_table);

				Ok(())
			}

			Statement::Let(ident, _, _) => {
				let name = ident.name.clone();
				self.insert_local(name);

				Ok(())
			},

			Statement::If(_, conseq, other, alt) => {
				let (name, mut sub_table) = self.new_block();
				sub_table.convert_block(conseq.clone())?;
				self.insert_table(name, sub_table);

				for (_, conseq) in other.iter() {
					let (name, mut sub_table) = self.new_block();
					sub_table.convert_block(conseq.clone())?;
					self.insert_table(name, sub_table);
				}

				match alt {
					Some(block) => {
						let (name, mut sub_table) = self.new_block();
						sub_table.convert_block(block.clone())?;
						self.insert_table(name, sub_table);
					}
					_ => {}
				}

				Ok(())
			},

		}
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
	symbol_type: SymbolType,
	address: usize,
}

impl Symbol {
	///
	///
	///
	pub fn new(symbol_type: SymbolType, address: usize) -> Symbol {
		Symbol {
			symbol_type: symbol_type,
			address: address,
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
