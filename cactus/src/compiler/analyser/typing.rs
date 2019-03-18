//!
//!
//!

use crate::compiler::parser::{Function, TypeHint};


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionSignature {
	arguments: Vec<TypeHint>,
	return_type: TypeHint,
}

impl FunctionSignature {
	///
	///
	///
	pub fn new(arguments: Vec<TypeHint>, return_type: TypeHint) -> FunctionSignature {
		FunctionSignature {
			arguments: arguments,
			return_type: return_type,
		}
	}

	///
	///
	///
	pub fn new_from_function(function: Function) -> FunctionSignature {
		let mut arg_types: Vec<TypeHint> = Vec::new();

		for arg in function.arguments.iter() {
			arg_types.push(arg.get_type_hint());
		}

		FunctionSignature::new(arg_types, function.return_type)
	}

	///
	///
	///
	pub fn args(&self) -> Vec<TypeHint> {
		self.arguments.clone()
	}

	///
	///
	///
	pub fn ret_type(&self) -> TypeHint {
		self.return_type
	}
}
