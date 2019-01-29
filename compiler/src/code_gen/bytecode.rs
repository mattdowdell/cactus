//!
//!
//!

use parser::ast;

pub trait ToBytecode {
	fn to_bytecode(&self) -> Vec<Instruction>;
}


pub trait BytecodeNode {
	fn to_code(&self) -> String;
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
	pub instructions: Vec<Instruction>,
}


impl Module {
	///
	///
	///
	pub fn new() -> Module {
		Module {
			instructions: Vec::new(),
		}
	}

	///
	///
	///
	pub fn extend(&mut self, instructions: Vec<Instruction>) {
		self.instructions.extend(instructions);
	}
}


impl BytecodeNode for Module {
	///
	///
	///
	fn to_code(&self) -> String {
		let mut ret = String::new();

		for instruction in &self.instructions {
			ret = format!("{}{}\n", ret, instruction.to_code());
		}

		ret
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
	AddressOf(String),
	Label(String),
	Load,
	Movret,
	Mul,
	Nop,
	Outln,
	Push(Argument),
	Return,
	Store,
}


impl BytecodeNode for Instruction {
	fn to_code(&self) -> String {
		match self {
			Instruction::AddressOf(val) => format!("\t&{};", val),
			Instruction::Label(val)     => format!("{}:", val),
			Instruction::Push(arg)     => format!("\tPUSH {};", arg.to_code()),

			Instruction::Load   => "\tLOAD;".to_string(),
			Instruction::Movret => "\tMOVRET;".to_string(),
			Instruction::Mul    => "\tMUL;".to_string(),
			Instruction::Nop    => "\tNOP;".to_string(),
			Instruction::Outln  => "\tOUTLN;".to_string(),
			Instruction::Return => "\tRETURN;".to_string(),
			Instruction::Store  => "\tSTORE;".to_string(),
		}
	}
}


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum Argument {
	Args,
	Int32(i32),
	Float(f32),
}


impl Argument {
	///
	///
	///
	pub fn from_literal(literal: ast::Literal) -> Argument {
		match literal {
			ast::Literal::Int32(val) => Argument::Int32(val),
			ast::Literal::Float(val) => Argument::Float(val),
			ast::Literal::Boolean(val)  => {
				let bool_val = if val { 1 } else { 0 };
				Argument::Int32(bool_val)
			}
		}
	}
}

impl BytecodeNode for Argument {
	fn to_code(&self) -> String {
		match self {
			Argument::Args => "ARGS".to_string(),
			Argument::Int32(val) => format!("{}", val),
			Argument::Float(val) => format!("{}", val),
		}
	}
}


impl ToBytecode for ast::Statement {
	fn to_bytecode(&self) -> Vec<Instruction> {
		let mut ret: Vec<Instruction> = Vec::new();

		match self {
			/*ast::Statement::Let(ident, _type_hint, expr) => {
				ret.push(Instruction::Label(ident.value.clone()));
				ret.push(Instruction::Nop);
			},*/
			ast::Statement::Return(expr) => {
				match expr {
					ast::Expression::Identifier(ref ident) => {
						ret.push(Instruction::AddressOf(ident.value.clone()));
						ret.push(Instruction::Load);
					},
					ast::Expression::Literal(literal) => {
						let arg = Argument::from_literal(*literal);
						ret.push(Instruction::Push(arg));
					},
					_ => ret.extend(expr.to_bytecode()),
				};

				ret.push(Instruction::Movret);
				ret.push(Instruction::Return);
			},
			ast::Statement::Print(expr) => {
				match expr {
					ast::Expression::Identifier(ref ident) => {
						ret.push(Instruction::AddressOf(ident.value.clone()));
						ret.push(Instruction::Load);
					},
					ast::Expression::Literal(literal) => {
						let arg = Argument::from_literal(*literal);
						ret.push(Instruction::Push(arg));
					},
					_ => ret.extend(expr.to_bytecode()),
				};

				ret.push(Instruction::Outln);
			},
			ast::Statement::Expression(expr) => {
				match expr {
					ast::Expression::Identifier(ref ident) => {
						ret.push(Instruction::AddressOf(ident.value.clone()));
						ret.push(Instruction::Load);
					},
					ast::Expression::Literal(literal) => {
						let arg = Argument::from_literal(*literal);
						ret.push(Instruction::Push(arg));
					},
					_ => ret.extend(expr.to_bytecode()),
				};
			},
			ast::Statement::Block(block) => {
				for statement in &block.statements {
					ret.extend(statement.to_bytecode());
				}
			},
			_ => {},
		}

		ret
	}
}


impl ToBytecode for ast::Expression {
	fn to_bytecode(&self) -> Vec<Instruction> {
		let mut ret: Vec<Instruction> = Vec::new();

		match self {
			// ast::Expression::Identifier(ident) => {},
			// ast::Expression::Literal(literal) => {},
			// ast::Expression::Prefix(op, right) => {},
			ast::Expression::Infix(left, op, right) => {
				match **left {
					ast::Expression::Identifier(ref ident) => {
						ret.push(Instruction::AddressOf(ident.value.clone()));
						ret.push(Instruction::Load);
					},
					ast::Expression::Literal(literal) => {
						let arg = Argument::from_literal(literal);
						ret.push(Instruction::Push(arg));
					},
					_ => {},
				}

				match **right {
					ast::Expression::Identifier(ref ident) => {
						ret.push(Instruction::AddressOf(ident.value.clone()));
						ret.push(Instruction::Load);
					},
					ast::Expression::Literal(literal) => {
						let arg = Argument::from_literal(literal);
						ret.push(Instruction::Push(arg));
					},
					_ => {},
				}

				match op {
					ast::Operator::Multiply => {
						ret.push(Instruction::Mul);
					}
					_ => {}
				}
			},
			ast::Expression::Function(ident, params, _type_hint, statement) => {
				// function name
				ret.push(Instruction::Label(ident.value.clone()));

				for param in params {
					ret.push(Instruction::Label(param.identifier.value.clone()));
					ret.push(Instruction::AddressOf(param.identifier.value.clone()));
					ret.push(Instruction::Push(Argument::Args));
					ret.push(Instruction::Load);
					ret.push(Instruction::Store);
				}

				let block_instrs: Vec<Instruction> = statement.to_bytecode();
				ret.extend(block_instrs);
			},
			_ => {},
		}

		ret
	}
}


#[cfg(test)]
mod test {
	use super::*;
	use parser::parser::Parser;

	// Test that a function in Cactus can be converted to bytecode.
	#[test]
	fn test_function() {
		let input = "fn square(n: i32) -> i32 { return n * n; }";
		let expected = "square:\nn:\n\t&n;\n\tPUSH ARGS;\n\tLOAD;\n\tSTORE;\n\t&n;\n\tLOAD;\n\t&n;\n\tLOAD;\n\tMUL;\n\tMOVRET;\n\tRETURN;\n";
		let mut parser = Parser::new(input);

		parser.parse();

		let bytecode = parser.module.statements[0].to_bytecode();
		let mut module = Module::new();

		module.extend(bytecode);
		assert_eq!(module.to_code(), expected);
	}

	#[test]
	#[ignore]
	fn test_function_call() {
		let input = "square(5);";
		let expected = "";
		let mut parser = Parser::new(input);

		parser.parse();

		if parser.has_errors() {
			for error in parser.errors {
				println!("{}", error);
			}
		}

		let mut module = Module::new();

		for statement in parser.module.statements {
			module.extend(statement.to_bytecode());
		}

		assert_eq!(module.to_code(), expected);
	}

	// Test that a print statement in Cactus can be converted to bytecode.
	#[test]
	fn test_print_statement() {
		let input = "print 5;";
		let expected = "\tPUSH 5;\n\tOUTLN;\n";
		let mut parser = Parser::new(input);

		parser.parse();

		let bytecode = parser.module.statements[0].to_bytecode();
		let mut module = Module::new();

		module.extend(bytecode);
		assert_eq!(module.to_code(), expected);
	}
}
