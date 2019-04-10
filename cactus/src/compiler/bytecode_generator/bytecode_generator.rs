//! The interface for generating bytecode from the AST.

use crate::bytecode::Instruction;
use crate::compiler::error::CompilationError;
use crate::compiler::parser::Ast;

use super::ast::TToBytecode;

/// A representation of the byte code generator.
#[derive(Clone, Debug, PartialEq)]
pub struct BytecodeGenerator {}

impl BytecodeGenerator {
	/// Create a new instance of `BytecodeGenerator`.
	pub fn new() -> BytecodeGenerator {
		BytecodeGenerator {}
	}

	/// Convert the given ASt to bytecode instructions.
	pub fn convert_ast(&self, ast: Ast) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		ast.to_bytecode()
	}
}

#[cfg(test)]
mod test {
	use crate::compiler::parser::Parser;
	use crate::compiler::analyser::Analyser;

	use crate::bytecode::{Literal, Symbol};

	use super::*;

	fn generate(input: &str) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut parser = Parser::new(input);
		let mut analyser = Analyser::new();
		let generator = BytecodeGenerator::new();

		let mut ast = parser.parse()?;
		analyser.analyse_ast(&mut ast)?;

		let instructions = generator.convert_ast(ast)?;
		Ok(instructions)
	}

	#[test]
	fn test_main_function() {
		let res = generate("fn main() {}");
		let expected = vec![
			Instruction::Labeldef("main".to_string()),
			Instruction::Halt,
		];

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), expected);
	}

	#[test]
	fn test_let_statement() {
		let res = generate("fn x() { let a: i32 = 1; let b: i32 = 2; let c: i32 = 3; }");
		let expected = vec![
			Instruction::Labeldef("x".to_string()),
			Instruction::Push(Literal::Symbol(Symbol::Locals)),
			Instruction::Push(Literal::Integer("0".to_string())),
			Instruction::Push(Literal::Integer("1".to_string())),
			Instruction::Storeidx,
			Instruction::Push(Literal::Symbol(Symbol::Locals)),
			Instruction::Push(Literal::Integer("1".to_string())),
			Instruction::Push(Literal::Integer("2".to_string())),
			Instruction::Storeidx,
			Instruction::Push(Literal::Symbol(Symbol::Locals)),
			Instruction::Push(Literal::Integer("2".to_string())),
			Instruction::Push(Literal::Integer("3".to_string())),
			Instruction::Storeidx,
			Instruction::Return,
		];

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), expected);
	}

	#[test]
	fn test_return_statement() {
		let res = generate("fn x() -> i32 { return 5; }\nfn y() -> bool { return true; }\nfn z() -> f32 { return 5.0; }");
		let expected = vec![
			Instruction::Labeldef("x".to_string()),
			Instruction::Push(Literal::Integer("5".to_string())),
			Instruction::Movret,
			Instruction::Return,

			Instruction::Labeldef("y".to_string()),
			Instruction::Push(Literal::Integer("0".to_string())),
			Instruction::Movret,
			Instruction::Return,

			Instruction::Labeldef("z".to_string()),
			Instruction::Push(Literal::Float("5.0".to_string())),
			Instruction::Movret,
			Instruction::Return,
		];

		assert!(res.is_ok());
		assert_eq!(res.unwrap(), expected);
	}

	#[test]
	fn test_call_expression() {
		let res = generate("fn x() {}\nfn y() -> i32 { return 5; }\nfn main() { x(); y(); }");
		let expected = vec![
			Instruction::Labeldef("x".to_string()),
			Instruction::Return,

			Instruction::Labeldef("y".to_string()),
			Instruction::Push(Literal::Integer("5".to_string())),
			Instruction::Movret,
			Instruction::Return,

			Instruction::Labeldef("main".to_string()),
			Instruction::Pushaddr("x".to_string()),
			Instruction::Subcall,

			Instruction::Pushaddr("y".to_string()),
			Instruction::Subcall,
			Instruction::Pushret,
			Instruction::Halt,
		];

		match res {
			Ok(output) => {
				assert_eq!(output, expected);
			},
			Err(errors) => {
				for err in errors.iter() {
					println!("{}", err);
				}

				panic!("Unexpected test failure");
			}
		}
	}
}
