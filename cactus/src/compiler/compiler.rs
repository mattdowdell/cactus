//!
//!
//!

use crate::{
	error::CompilationError,
	parser::parser::Parser,
	analyser::analyser::Analyser,
	flowgraph::{
		flowgraph::FlowGraph,
		instruction::Instruction,
	},
};


pub struct Compiler {}

impl Compiler {
	///
	///
	///
	pub fn new() -> Compiler {
		Compiler {}
	}

	///
	///
	///
	pub fn compile(&self, input: &str) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut parser = Parser::new(input);
		let ast = parser.parse()?;

		let mut analyser = Analyser::new(ast);
		let ast = analyser.analyse_ast()?;

		let mut flowgraph = FlowGraph::new();

		flowgraph.convert_ast(ast);
		flowgraph.to_basic_blocks();
		flowgraph.follow_graph("main".to_string());

		Ok(flowgraph.flatten_basic_blocks())
	}
}
