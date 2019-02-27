//!
//!
//!

pub struct TypeChecker {
	// TODO
}

pub struct FunctionSignature {
	name: String,
	arguments: Vec<TypeHint>,
	return_type: TypeHint,
}

pub enum TypeHint {
	Int32,
	Uint32,
	Bool,
	None,
}

