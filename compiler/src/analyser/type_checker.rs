//!
//!
//!

use crate::{
	error::{
		CompilationError,
		ErrorCode,
		ErrorType,
	},
	location::Location,
	parser::ast::{
		Function,
		Type,
	},
};


///
///
///
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionSignature {
	pub arguments: Vec<TypeHint>,
	pub return_type: TypeHint,
}

impl FunctionSignature {
	///
	///
	///
	pub fn new(func: Function) -> Result<(String, FunctionSignature), CompilationError> {
		let mut arguments: Vec<TypeHint> = Vec::new();

		for arg in func.arguments.iter() {
			arguments.push(
				TypeHint::from_ast_type(arg.type_hint.clone())?
			);
		}

		let return_type = TypeHint::from_optional_ast_type(func.return_type)?;

		Ok((
			func.identifier.name.clone(),
			FunctionSignature {
				arguments: arguments,
				return_type: return_type,
			}
		))
	}
}

///
///
///
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TypeHint {
	Int32,
	Uint32,
	Float,
	Bool,
	None,
}

impl TypeHint {
	///
	///
	///
	pub fn from_ast_type(ast_type: Type) -> Result<TypeHint, CompilationError> {
		match ast_type {
			Type::Int32  => Ok(TypeHint::Int32),
			Type::Uint32 => Ok(TypeHint::Uint32),
			Type::Float  => Ok(TypeHint::Float),
			Type::Bool   => Ok(TypeHint::Bool),

			Type::Custom(_) => Err(not_implemented_error!(
				ErrorCode::E0800,
				Location::end(),
				"Custom types are not yet supported for type checking"
			)),

			Type::Unknown => Err(internal_error!(
				ErrorCode::E1000,
				Location::end(),
				"Unexpected \"Type::Unknown\" when converting from \"ast::Type\" to \"type_checker::TypeHint\""
			)),
		}
	}

	///
	///
	///
	pub fn from_optional_ast_type(ast_type: Option<Type>) -> Result<TypeHint, CompilationError> {
		if ast_type.is_some() {
			TypeHint::from_ast_type(ast_type.unwrap())
		} else {
			Ok(TypeHint::None)
		}
	}
}


#[cfg(test)]
mod test {
	use super::*;
	use crate::parser::ast::Identifier;

	#[test]
	fn test_typehint_from_ast_type_primitive() {
		let data = vec![
			(Type::Int32, TypeHint::Int32),
			(Type::Uint32, TypeHint::Uint32),
			(Type::Float, TypeHint::Float),
			(Type::Bool, TypeHint::Bool),
		];

		for (input, expected) in data.iter() {
			let output = TypeHint::from_ast_type(input.clone());

			assert!(output.is_ok());
			assert_eq!(output.unwrap(), *expected);
		}
	}

	#[test]
	fn test_typehint_from_ast_type_custom() {
		let custom_type = Type::Custom(Box::new(Identifier::new("example".to_string())));

		let output = TypeHint::from_ast_type(custom_type);
		assert!(output.is_err());

		let error = format!("{}", output.err().unwrap());
		assert_eq!(
			error,
			"E0800: Not Implemented Error: Custom types are not yet supported for type checking"
		);
	}

	#[test]
	fn test_typehint_from_ast_type_unknown() {
		let unknown_type = Type::Unknown;

		let output = TypeHint::from_ast_type(unknown_type);
		assert!(output.is_err());

		let error = format!("{}", output.err().unwrap());
		assert_eq!(
			error,
			"E1000: Internal Error: Unexpected \"Type::Unknown\" when converting from \"ast::Type\" to \"type_checker::TypeHint\""
		);
	}

	#[test]
	fn test_typehint_from_optional_ast_type() {
		let data = vec![
			(None, TypeHint::None),
			(Some(Type::Int32), TypeHint::Int32),
		];

		for (input, expected) in data.iter() {
			let output = TypeHint::from_optional_ast_type(input.clone());

			assert!(output.is_ok());
			assert_eq!(output.unwrap(), *expected);
		}
	}
}
