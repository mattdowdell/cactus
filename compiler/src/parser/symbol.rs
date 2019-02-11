//!
//!
//!

use std::collections::HashMap;
use std::fmt;


// Test that a number is a power of two
// From: <https://stackoverflow.com/a/600306>
fn is_power_two(x: usize) -> bool {
	x > 0 && (x & (x - 1) == 0)
}


/// A representation of a symbol table in Cactus.
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
	pub symbols: HashMap<usize, SymbolValue>,
	pub index: usize,
	pointer_byte_size: usize,
}


impl SymbolTable {
	/// Initialise a new `SymbolTable`.
	///
	///
	pub fn new(pointer_byte_size: usize, start: usize) -> SymbolTable {
		// check pointer_byte_size is a power of two
		if !is_power_two(pointer_byte_size) {
			panic!("Given pointer byte size is not a power of two: {}", pointer_byte_size);
		}

		SymbolTable {
			symbols: HashMap::new(),
			index: start,
			pointer_byte_size: pointer_byte_size,
		}
	}

	pub fn set_index(&mut self, index: usize) {
		self.index = index;
	}

	///
	///
	///
	pub fn new_function(&self, name: String) -> Function {
		Function::new(name, self.pointer_byte_size)
	}

	//
	//
	//
	pub fn align_index(&mut self) {
		let offset = self.index % self.pointer_byte_size;

		if offset != 0 {
			self.index += self.pointer_byte_size - offset
		}
	}

	/// Insert a symbol into the symbol table.
	///
	/// Normally symbols are inserted at the next word boundary, where a word is the size of a
	/// pointer. For example, if the target architecture is 32-bit, then a pointer is 4 bytes and
	/// thus a boundary occurs at indexes 4, 8, 12, etc.
	///
	/// There is one exception to this rule. If the size of the symbol is less than the size of a
	/// pointer and the current index plus the size of the symbol does not overflow the next word
	/// boundary, then the index will not be aligned and the symbol will be inserted at the next
	/// available index. This allows memory to be utilised more efficiently and is used by
	/// structure packing.
	pub fn insert_symbol(&mut self, symbol: Symbol) -> usize {
		let size = symbol.size;
		let value = SymbolValue::Symbol(symbol);

		if
			size >= self.pointer_byte_size ||
			(self.index % 4) + size > self.pointer_byte_size
		{
			self.align_index();
		}

		// save the current index as the place we inserted the symbol at
		let ret = self.index;

		// insert the symbol and increment the index to the next unused location
		self.symbols.insert(ret, value);
		self.index += size;

		// if we're adding something larger than the size of a pointer
		// move to the next word boundary straight away
		if size >= self.pointer_byte_size {
			self.align_index();
		}

		ret
	}

	///
	///
	///
	pub fn insert_function(&mut self, function: Function) -> usize {
		let size = function.size;
		let value = SymbolValue::Function(function);

		self.align_index();

		// save the current index as the place we inserted the symbol at
		let ret = self.index;

		// insert the symbol and increment the index to the next unused location
		self.symbols.insert(ret, value);
		self.index += size;

		ret
	}

	///
	///
	///
	pub fn insert_local(&mut self, name: String, size: usize) -> usize {
		let symbol = Symbol::new(name, SymbolScope::Local, size);
		self.insert_symbol(symbol)
	}

	pub fn extend(&mut self, table: SymbolTable) {
		self.symbols.extend(table.symbols);
		self.index = table.index;
	}
}


impl fmt::Display for SymbolTable {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut ret = String::new();

		for (key, val) in &self.symbols {
			ret = format!("{}{} = {}\n", ret, key, val);
		}

		write!(f, "{}", ret)
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum SymbolValue {
	Symbol(Symbol),
	Function(Function)
}


impl fmt::Display for SymbolValue {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			SymbolValue::Symbol(symbol) => write!(f, "{}", symbol),
			SymbolValue::Function(function) => write!(f, "{}", function),
		}
	}
}


///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SymbolScope {
	Global,
	Local,
	Function,
	Argument,
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
	name: String,
	scope: SymbolScope,
	size: usize,
}


impl Symbol {
	///
	///
	///
	pub fn new(name: String, scope: SymbolScope, size: usize) -> Symbol {
		Symbol {
			name: name,
			scope: scope,
			size: size,
		}
	}
}


impl fmt::Display for Symbol {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{} | {} | {:?}", self.name, self.size, self.scope)
	}
}

///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
	name: String,
	scope: SymbolScope,
	size: usize,
	pub table: SymbolTable,
}


impl Function {
	/// Initialise a new `Function`.
	///
	///
	pub fn new(name: String, pointer_byte_size: usize) -> Function {
		Function {
			name: name,
			scope: SymbolScope::Function,
			size: 0,
			table: SymbolTable::new(pointer_byte_size, 0),
		}
	}

	pub fn align_index(&mut self) {
		self.table.align_index();
		self.size = self.table.index;
	}

	///
	///
	///
	pub fn insert_argument(&mut self, name: String, size: usize) -> usize {
		let symbol = Symbol::new(name, SymbolScope::Argument, size);

		self.size += size;
		self.table.insert_symbol(symbol)
	}

	///
	///
	///
	pub fn extend(&mut self, table: SymbolTable) {
		self.table.extend(table);
		self.size = self.table.index;
	}
}


impl fmt::Display for Function {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{} | {} | {:?}\n{}", self.name, self.size, self.scope, self.table)
	}
}


#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_is_power_two() {
		// pointer sizes for 8, 16, 32 and 64-bit architectures
		let data: [usize; 4] = [1, 2, 4, 8];

		for i in data.iter() {
			assert!(is_power_two(*i));
		}
	}
}
