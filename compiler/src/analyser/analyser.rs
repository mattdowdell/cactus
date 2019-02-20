//!
//!
//!

use parser::ast::Ast;
use analyser::symbol::SymbolTable;


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Analyser {
	ast: Ast,
	symbol_table: SymbolTable,
}

impl Analyser {
	///
	///
	///
	pub fn new(ast: Ast) -> Analyser {
		Analyser {
			ast: ast,
			symbol_table: SymbolTable::new(),
		}
	}

	///
	///
	///
	pub fn populate_symbol_table(&mut self) -> Result<SymbolTable, String> {
		self.symbol_table.convert_ast(self.ast.clone())?;
		Ok(self.symbol_table.clone())
	}
}


