//! Data structures and functions to use during type checking.

use crate::location::Location;
use crate::compiler::error::{CompilationError, ErrorCode, type_error, internal_error};
use crate::compiler::parser::{Function, TypeHint, Operator};


/// A representation of a function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionSignature {
	arguments: Vec<TypeHint>,
	return_type: TypeHint,
}

impl FunctionSignature {
	/// Create a new instance of `FunctionSignature`.
	pub fn new(arguments: Vec<TypeHint>, return_type: TypeHint) -> FunctionSignature {
		FunctionSignature {
			arguments: arguments,
			return_type: return_type,
		}
	}

	/// Create a new instance of `FunctionSignature` from an AST function node.
	pub fn new_from_function(function: Function) -> FunctionSignature {
		let mut arg_types: Vec<TypeHint> = Vec::new();

		for arg in function.arguments.iter() {
			arg_types.push(arg.get_type_hint());
		}

		FunctionSignature::new(arg_types, function.return_type)
	}

	/// Getter for the function's arguments.
	pub fn args(&self) -> Vec<TypeHint> {
		self.arguments.clone()
	}

	/// Getter for the function's return type.
	pub fn ret_type(&self) -> TypeHint {
		self.return_type
	}
}

/// Check that the given prefix operator is compatible with the given operand.
///
/// If it is compatible, the resulting type of the operation will be returned.
pub fn check_prefix_operator(operator: Operator, right: TypeHint, location: Location) -> Result<TypeHint, CompilationError> {
	match operator {
		Operator::Not => {
			match right {
				TypeHint::Bool => Ok(TypeHint::Bool),

				TypeHint::Int32
				| TypeHint::Float
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0208,
						location,
						format!("Prefix operator {} ({:?}) cannot be used with an operand of type {} ({:?})",
							operator, operator,
							right, right)))
				}
			}
		}
		Operator::UnaryMinus => {
			match right {
				TypeHint::Int32
				| TypeHint::Float => Ok(right),

				TypeHint::Bool
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0208,
						location,
						format!("Prefix operator {} ({:?}) cannot be used with an operand of type {} ({:?})",
							operator, operator,
							right, right)))
				}
			}
		}
		Operator::BitCompl => {
			match right {
				TypeHint::Int32 => Ok(TypeHint::Int32),

				TypeHint::Float
				| TypeHint::Bool
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0208,
						location,
						format!("Prefix operator {} ({:?}) cannot be used with an operand of type {} ({:?})",
							operator, operator,
							right, right)))
				}
			}
		}

		_ => {
			Err(internal_error(ErrorCode::E1007,
				location,
				format!("Non-prefix operator found: {} ({:?})",
					operator, operator)))
		}
	}
}

/// Check that the given infix operator is compatible with the given operands.
///
/// If it is compatible, the resulting type of the operation will be returned.
pub fn check_infix_operator(operator: Operator, left: TypeHint, location: Location) -> Result<TypeHint, CompilationError> {
	match operator {
		Operator::Plus
		| Operator::Minus
		| Operator::Multiply
		| Operator::Divide
		| Operator::Modulo => {
			match left {
				// TODO: support floats?
				TypeHint::Int32 => Ok(left),

				TypeHint::Float
				| TypeHint::Bool
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0209,
						location,
						format!("Infix operator {} ({:?}) cannot be used with an operands of type {} ({:?})",
							operator, operator,
							left, left)))
				}
			}
		}

		Operator::BitAnd
		| Operator::BitOr
		| Operator::BitXor
		| Operator::BitLeftShift
		| Operator::BitRightShift => {
			match left {
				TypeHint::Int32 => Ok(left),

				TypeHint::Float
				| TypeHint::Bool
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0209,
						location,
						format!("Infix operator {} ({:?}) cannot be used with an operands of type {} ({:?})",
							operator, operator,
							left, left)))
				}
			}
		}

		Operator::Equal
		| Operator::NotEqual => {
			match left {
				TypeHint::Int32
				| TypeHint::Bool => Ok(TypeHint::Bool),

				TypeHint::Float
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0209,
						location,
						format!("Infix operator {} ({:?}) cannot be used with an operands of type {} ({:?})",
							operator, operator,
							left, left)))
				}
			}
		}

		Operator::LessThan
		| Operator::LessThanOrEqual
		| Operator::GreaterThan
		| Operator::GreaterThanOrEqual => {
			match left {
				// TODO: support floats?
				TypeHint::Int32 => Ok(TypeHint::Bool),

				TypeHint::Float
				| TypeHint::Bool
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0209,
						location,
						format!("Infix operator {} ({:?}) cannot be used with an operands of type {} ({:?})",
							operator, operator,
							left, left)))
				}
			}
		}

		Operator::And
		| Operator::Or => {
			match left {
				TypeHint::Bool => Ok(TypeHint::Bool),

				TypeHint::Int32
				| TypeHint::Float
				| TypeHint::None => {
					Err(type_error(ErrorCode::E0209,
						location,
						format!("Infix operator {} ({:?}) cannot be used with an operands of type {} ({:?})",
							operator, operator,
							left, left)))
				}
			}
		}

		// assignment is special in that it doesn't return anything
		// so this stubs it out for future use
		Operator::Assign => {
			match left {
				TypeHint::Bool
				| TypeHint::Int32
				| TypeHint::Float => Ok(TypeHint::None),

				TypeHint::None => {
					Err(type_error(ErrorCode::E0209,
						location,
						format!("Infix operator {} ({:?}) cannot be used with an operands of type {} ({:?})",
							operator, operator,
							left, left)))
				}
			}
		},

		_ => {
			Err(internal_error(ErrorCode::E1008,
				location,
				format!("Non-infix operator found: {} ({:?})",
					operator, operator)))
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	const LOCATION: Location = Location {
		line: 1,
		column: 1,
	};

	#[test]
	fn test_check_prefix_operator() {
		let data = vec![
			(Operator::Not, TypeHint::Bool, Some(TypeHint::Bool)),
			(Operator::Not, TypeHint::Int32, None),
			(Operator::Not, TypeHint::Float, None),
			(Operator::Not, TypeHint::None, None),

			(Operator::UnaryMinus, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::UnaryMinus, TypeHint::Float, Some(TypeHint::Float)),
			(Operator::UnaryMinus, TypeHint::Bool, None),
			(Operator::UnaryMinus, TypeHint::None, None),

			(Operator::BitCompl, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::BitCompl, TypeHint::Float, None),
			(Operator::BitCompl, TypeHint::Bool, None),
			(Operator::BitCompl, TypeHint::None, None),
		];

		for (operator, operand, expected) in data.iter() {
			let res = check_prefix_operator(*operator, *operand, LOCATION);

			if expected.is_some() {
				match res {
					Ok(output) => {
						assert_eq!(output, expected.unwrap());
					},
					Err(error) => {
						println!("{}", error);
						panic!("Unexpected test failure");
					}
				}
			} else {
				match res {
					Ok(output) => {
						panic!("Unexpected test success. Found {} ({:?}) for operator {} ({:?}), operand {} ({:?})",
							output, output,
							operator, operator,
							operand, operand);
					},
					Err(_) => {}
				}
			}
		}
	}

	#[test]
	fn test_check_infix_operator() {
		let data = vec![
			(Operator::Plus, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::Plus, TypeHint::Float, None),
			(Operator::Plus, TypeHint::Bool, None),
			(Operator::Plus, TypeHint::None, None),

			(Operator::Minus, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::Minus, TypeHint::Float, None),
			(Operator::Minus, TypeHint::Bool, None),
			(Operator::Minus, TypeHint::None, None),

			(Operator::Multiply, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::Multiply, TypeHint::Float, None),
			(Operator::Multiply, TypeHint::Bool, None),
			(Operator::Multiply, TypeHint::None, None),

			(Operator::Divide, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::Divide, TypeHint::Float, None),
			(Operator::Divide, TypeHint::Bool, None),
			(Operator::Divide, TypeHint::None, None),

			(Operator::Modulo, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::Modulo, TypeHint::Float, None),
			(Operator::Modulo, TypeHint::Bool, None),
			(Operator::Modulo, TypeHint::None, None),

			(Operator::BitAnd, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::BitAnd, TypeHint::Float, None),
			(Operator::BitAnd, TypeHint::Bool, None),
			(Operator::BitAnd, TypeHint::None, None),

			(Operator::BitOr, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::BitOr, TypeHint::Float, None),
			(Operator::BitOr, TypeHint::Bool, None),
			(Operator::BitOr, TypeHint::None, None),

			(Operator::BitXor, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::BitXor, TypeHint::Float, None),
			(Operator::BitXor, TypeHint::Bool, None),
			(Operator::BitXor, TypeHint::None, None),

			(Operator::BitLeftShift, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::BitLeftShift, TypeHint::Float, None),
			(Operator::BitLeftShift, TypeHint::Bool, None),
			(Operator::BitLeftShift, TypeHint::None, None),

			(Operator::BitRightShift, TypeHint::Int32, Some(TypeHint::Int32)),
			(Operator::BitRightShift, TypeHint::Float, None),
			(Operator::BitRightShift, TypeHint::Bool, None),
			(Operator::BitRightShift, TypeHint::None, None),

			(Operator::Equal, TypeHint::Bool, Some(TypeHint::Bool)),
			(Operator::Equal, TypeHint::Int32, Some(TypeHint::Bool)),
			(Operator::Equal, TypeHint::Float, None),
			(Operator::Equal, TypeHint::None, None),

			(Operator::NotEqual, TypeHint::Bool, Some(TypeHint::Bool)),
			(Operator::NotEqual, TypeHint::Int32, Some(TypeHint::Bool)),
			(Operator::NotEqual, TypeHint::Float, None),
			(Operator::NotEqual, TypeHint::None, None),

			(Operator::LessThan, TypeHint::Int32, Some(TypeHint::Bool)),
			(Operator::LessThan, TypeHint::Float, None),
			(Operator::LessThan, TypeHint::Bool, None),
			(Operator::LessThan, TypeHint::None, None),

			(Operator::LessThanOrEqual, TypeHint::Int32, Some(TypeHint::Bool)),
			(Operator::LessThanOrEqual, TypeHint::Float, None),
			(Operator::LessThanOrEqual, TypeHint::Bool, None),
			(Operator::LessThanOrEqual, TypeHint::None, None),

			(Operator::GreaterThan, TypeHint::Int32, Some(TypeHint::Bool)),
			(Operator::GreaterThan, TypeHint::Float, None),
			(Operator::GreaterThan, TypeHint::Bool, None),
			(Operator::GreaterThan, TypeHint::None, None),

			(Operator::GreaterThanOrEqual, TypeHint::Int32, Some(TypeHint::Bool)),
			(Operator::GreaterThanOrEqual, TypeHint::Float, None),
			(Operator::GreaterThanOrEqual, TypeHint::Bool, None),
			(Operator::GreaterThanOrEqual, TypeHint::None, None),

			(Operator::And, TypeHint::Bool, Some(TypeHint::Bool)),
			(Operator::And, TypeHint::Int32, None),
			(Operator::And, TypeHint::Float, None),
			(Operator::And, TypeHint::None, None),

			(Operator::Or, TypeHint::Bool, Some(TypeHint::Bool)),
			(Operator::Or, TypeHint::Int32, None),
			(Operator::Or, TypeHint::Float, None),
			(Operator::Or, TypeHint::None, None),

			(Operator::Assign, TypeHint::Bool, Some(TypeHint::None)),
			(Operator::Assign, TypeHint::Int32, Some(TypeHint::None)),
			(Operator::Assign, TypeHint::Float, Some(TypeHint::None)),
			(Operator::Assign, TypeHint::None, None),
		];

		for (operator, operand, expected) in data.iter() {
			let res = check_infix_operator(*operator, *operand, LOCATION);

			if expected.is_some() {
				match res {
					Ok(output) => {
						assert_eq!(output, expected.unwrap());
					},
					Err(error) => {
						println!("{}", error);
						panic!("Unexpected test failure");
					}
				}
			} else {
				match res {
					Ok(output) => {
						panic!("Unexpected test success. Found {} ({:?}) for operator {} ({:?}), operand {} ({:?})",
							output, output,
							operator, operator,
							operand, operand);
					},
					Err(_) => {}
				}
			}
		}
	}
}
