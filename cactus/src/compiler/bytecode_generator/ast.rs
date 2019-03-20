//!
//!
//!

use crate::error::CompilationError;
use crate::bytecode::{Instruction, Symbol};
use crate::compiler::analyser::SymbolType;
use crate::compiler::parser::{
	Ast,
	Module,
	Definition,
	Function,
	Block,
	Statement,
	Let,
	If,
	Loop,
	Expression,
	Literal,
	LiteralValue,
	Identifier,
	Prefix,
	Infix,
	Call,
	Operator,
	TypeHint,
};

use super::label::{Label, LabelType};


///
///
///
pub trait TToBytecode {
	///
	///
	///
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>>;
}


impl TToBytecode for Ast {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions: Vec<Instruction> = Vec::new();
		let mut errors: Vec<CompilationError> = Vec::new();

		for module in self.modules.iter() {
			match module.to_bytecode() {
				Ok(instrs) => {
					instructions.extend(instrs);
				},
				Err(errs) => {
					errors.extend(errs);
				},
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(instructions)
		}
	}
}

impl TToBytecode for Module {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions: Vec<Instruction> = Vec::new();
		let mut errors: Vec<CompilationError> = Vec::new();

		for definition in self.definitions.iter() {
			match definition.to_bytecode() {
				Ok(instrs) => {
					instructions.extend(instrs);
				},
				Err(errs) => {
					errors.extend(errs);
				},
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(instructions)
		}
	}
}

impl TToBytecode for Definition {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		match self {
			Definition::Function(func) => func.to_bytecode(),
		}
	}
}

impl TToBytecode for Function {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions: Vec<Instruction> = Vec::new();
		let mut errors: Vec<CompilationError> = Vec::new();

		instructions.push(Instruction::Labeldef(self.get_name()));

		match self.body.to_bytecode() {
			Ok(instrs) => {
				instructions.extend(instrs);
			},
			Err(errs) => {
				errors.extend(errs);
			}
		}

		// return statements automatically insert a return instruction
		// but those that don't return won't so do that here instead
		// unless it's the main function, in which case we should halt instead
		if self.return_type == TypeHint::None {
			if self.get_name() == "main" {
				instructions.push(Instruction::Halt);
			} else {
				instructions.push(Instruction::Return);
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(instructions)
		}
	}
}

impl TToBytecode for Block {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions: Vec<Instruction> = Vec::new();
		let mut errors: Vec<CompilationError> = Vec::new();

		for statement in self.statements.iter() {
			match statement.to_bytecode() {
				Ok(instrs) => {
					instructions.extend(instrs);
				},
				Err(errs) => {
					errors.extend(errs);
				},
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(instructions)
		}
	}
}

impl TToBytecode for Statement {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		match self {
			Statement::Let(let_stmt)   => let_stmt.to_bytecode(),
			Statement::If(if_stmt)     => if_stmt.to_bytecode(),
			Statement::Loop(loop_stmt) => loop_stmt.to_bytecode(),
			Statement::Return(expr) => {
				let mut instructions = expr.to_bytecode()?;
				instructions.push(Instruction::Movret);
				instructions.push(Instruction::Return);

				Ok(instructions)
			},
			Statement::Continue(ctrl) => {
				let mut instructions = Vec::new();
				let label = Label::new(LabelType::LoopStart, ctrl.get_loop_id());

				instructions.push(Instruction::Pushaddr(format!("{}", label)));
				instructions.push(Instruction::Jmp);

				Ok(instructions)
			},
			Statement::Break(ctrl) => {
				let mut instructions = Vec::new();
				let label = Label::new(LabelType::LoopEnd, ctrl.get_loop_id());

				instructions.push(Instruction::Pushaddr(format!("{}", label)));
				instructions.push(Instruction::Jmp);

				Ok(instructions)

			},
			Statement::Expression(expr) => {
				expr.to_bytecode()
			},
		}
	}
}

impl TToBytecode for Let {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions = Vec::new();

		instructions.push(Instruction::push_symbol(Symbol::Locals));
		instructions.push(Instruction::push_offset(self.identifier.get_offset()));

		instructions.extend(self.value.to_bytecode()?);

		instructions.push(Instruction::Storeidx);

		Ok(instructions)
	}
}

impl TToBytecode for Loop {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions = Vec::new();

		let loop_start = Label::new(LabelType::LoopStart, self.get_id());
		let loop_end = Label::new(LabelType::LoopEnd, self.get_id());

		instructions.push(Instruction::Labeldef(format!("{}", loop_start)));
		instructions.extend(self.body.to_bytecode()?);
		instructions.push(Instruction::Labeldef(format!("{}", loop_end)));

		Ok(instructions)
	}
}

impl TToBytecode for If {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions = Vec::new();

		let if_end = Label::new(LabelType::IfEnd, self.get_id());

		for (index, branch) in self.branches.iter().enumerate() {
			let if_start = Label::new(LabelType::IfStart, self.get_id());
			let if_body = Label::new(LabelType::IfBody, self.get_id());

			instructions.push(Instruction::Labeldef(format!("{}", if_start)));
			instructions.extend(branch.condition.to_bytecode()?);

			if self.branches.len() > (index + 1) {
				let next_start = Label::new(LabelType::IfStart, self.branches[index + 1].get_id());

				instructions.push(Instruction::Pushaddr(format!("{}", next_start)));
				instructions.push(Instruction::Jmpnz);
			} else {
				instructions.push(Instruction::Pushaddr(format!("{}", if_end)));
				instructions.push(Instruction::Jmpnz);
			}

			instructions.push(Instruction::Pushaddr(format!("{}", if_body)));
			instructions.push(Instruction::Jmp);
		}

		instructions.push(Instruction::Labeldef(format!("{}", if_end)));

		Ok(instructions)
	}
}

impl TToBytecode for Expression {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		match self {
			Expression::Literal(lit)      => lit.to_bytecode(),
			Expression::Identifier(ident) => ident.to_bytecode(),
			Expression::Prefix(prefix)    => prefix.to_bytecode(),
			Expression::Infix(infix)      => infix.to_bytecode(),
			Expression::Call(call)        => call.to_bytecode(),
		}
	}
}

impl TToBytecode for Literal {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let instr = match &self.value {
			LiteralValue::Int32(val) => Instruction::push_integer(val.clone()),
			LiteralValue::Float(val) => Instruction::push_float(val.clone()),
			LiteralValue::True       => Instruction::push_integer("0".to_string()),
			LiteralValue::False      => Instruction::push_integer("1".to_string()),
		};

		Ok(vec![instr])
	}
}

impl TToBytecode for Identifier {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions = Vec::new();
		let symbol = match self.get_symbol_type() {
			SymbolType::Argument => Symbol::Args,
			SymbolType::Local    => Symbol::Locals,

			_ => unimplemented!(),
		};
		let offset = self.get_offset();

		instructions.push(Instruction::push_symbol(symbol));
		instructions.push(Instruction::push_offset(offset));
		instructions.push(Instruction::Loadidx);

		Ok(instructions)
	}
}

impl TToBytecode for Infix {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions = Vec::new();

		if self.is_assignment() {
			let ident = match *self.left.clone() {
				Expression::Identifier(ident) => ident,
				_ => panic!("TODO: throw internal error"),
			};
			let symbol = match ident.get_symbol_type() {
				SymbolType::Argument => Symbol::Args,
				SymbolType::Local    => Symbol::Locals,

				_ => unimplemented!(),
			};
			let offset = ident.get_offset();

			instructions.push(Instruction::push_symbol(symbol));
			instructions.push(Instruction::push_offset(offset));
			instructions.extend(self.right.to_bytecode()?);
			instructions.push(Instruction::Storeidx);
		} else {
			instructions.extend(self.left.to_bytecode()?);
			instructions.extend(self.right.to_bytecode()?);
			instructions.extend(self.operator.to_bytecode()?);
		}

		Ok(instructions)
	}
}

impl TToBytecode for Prefix {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions = Vec::new();

		instructions.extend(self.right.to_bytecode()?);
		instructions.extend(self.operator.to_bytecode()?);

		Ok(instructions)
	}
}

impl TToBytecode for Call {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		let mut instructions = Vec::new();

		for arg in self.arguments.iter() {
			instructions.extend(arg.to_bytecode()?);
			instructions.push(Instruction::Pusharg);
		}

		instructions.push(Instruction::Pushaddr(self.get_name()));
		instructions.push(Instruction::Subcall);

		// TODO: be smarter about when we do this
		//       it will cause errors for functions that don't return anything
		//       maybe supplement the call structure during analysing
		//       so it knows the return type
		//       (and if it's used as well)
		instructions.push(Instruction::Pushret);

		Ok(instructions)
	}
}

impl TToBytecode for Operator {
	fn to_bytecode(&self) -> Result<Vec<Instruction>, Vec<CompilationError>> {
		match self {
			// prefix operators
			Operator::Not => Ok(vec![Instruction::Not]),
			Operator::UnaryMinus => unimplemented!(),
			Operator::BitCompl => Ok(vec![Instruction::Compl]),

			// infix operators
			Operator::Plus               => Ok(vec![Instruction::Add]),
			Operator::Minus              => Ok(vec![Instruction::Minus]),
			Operator::Multiply           => Ok(vec![Instruction::Mul]),
			Operator::Divide             => Ok(vec![Instruction::Div]),
			Operator::Modulo             => Ok(vec![Instruction::Rem]),
			Operator::BitAnd             => Ok(vec![Instruction::And]),
			Operator::BitOr              => Ok(vec![Instruction::Or]),
			Operator::BitXor             => unimplemented!(),
			Operator::BitLeftShift       => unimplemented!(),
			Operator::BitRightShift      => unimplemented!(),
			Operator::Equal              => Ok(vec![Instruction::Eq]),
			Operator::NotEqual           => Ok(vec![Instruction::Neq]),
			Operator::LessThan           => Ok(vec![Instruction::Lt]),
			Operator::LessThanOrEqual    => Ok(vec![Instruction::Leq]),
			Operator::GreaterThan        => Ok(vec![Instruction::Gt]),
			Operator::GreaterThanOrEqual => Ok(vec![Instruction::Geq]),
			Operator::And                => unimplemented!(),
			Operator::Or                 => unimplemented!(),

			// assignment operators
			Operator::Assign => unimplemented!(),

			Operator::PlusAssign
			| Operator::MinusAssign
			| Operator::MultiplyAssign
			| Operator::DivideAssign
			| Operator::ModuloAssign
			| Operator::BitAndAssign
			| Operator::BitOrAssign
			| Operator::BitXorAssign
			| Operator::BitLeftShiftAssign
			| Operator::BitRightShiftAssign => {
				// internal error
				unimplemented!()
			}
		}
	}
}
