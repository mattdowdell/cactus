//!
//!
//!

/// All possible types of token. Includes delimiters, comments, literals, identifiers
/// and instructions.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
	// delimiter
	Colon,
	Semicolon,
	Ampersand,

	// comments
	InlineComment,
	BlockComment,

	// identifier, literals and symbols
	Ident,
	Integer,
	Float,
	String,
	Args,
	Locals,

	// instructions
	Nop,
	Halt,
	Push,
	Pop,
	Dup,
	Swap,
	Movret,
	Pushret,
	Alloca,
	Pusharg,
	Dumpstack,
	Dumpframe,
	Out,
	Outln,
	In,
	Store,
	Storeidx,
	Load,
	Loadidx,
	Eq,
	Neq,
	Leq,
	Geq,
	Lt,
	Gt,
	Compl,
	Minus,
	Add,
	Div,
	Rem,
	Mul,
	And,
	Or,
	Not,
	Jmpnz,
	Jmp,
	Subcall,
	Return,
}
