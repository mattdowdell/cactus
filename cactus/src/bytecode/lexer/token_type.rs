//! The token types for the bytecode.

use std::fmt;

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
	Leqf,
	Geq,
	Geqf,
	Lt,
	Ltf,
	Gt,
	Gtf,
	Compl,
	Minus,
	Minusf,
	Add,
	Addf,
	Div,
	Divf,
	Rem,
	Remf,
	Mul,
	Mulf,
	And,
	Or,
	Not,
	Jmpnz,
	Jmp,
	Subcall,
	Return,
	Lshift,
	Rshift,
	Xor,
}

impl fmt::Display for TokenType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			// these have associated values, so fall back to the debug representation
			TokenType::InlineComment
			| TokenType::BlockComment
			| TokenType::Ident
			| TokenType::Integer
			| TokenType::Float
			| TokenType::String => write!(f, "{:?}", self),

			// these have a single possible value
			TokenType::Colon     => write!(f, ":"),
			TokenType::Semicolon => write!(f, ";"),
			TokenType::Ampersand => write!(f, "&"),
			TokenType::Args      => write!(f, "ARGS"),
			TokenType::Locals    => write!(f, "LOCALS"),
			TokenType::Nop       => write!(f, "NOP"),
			TokenType::Halt      => write!(f, "HALT"),
			TokenType::Push      => write!(f, "PUSH"),
			TokenType::Pop       => write!(f, "POP"),
			TokenType::Dup       => write!(f, "DUP"),
			TokenType::Swap      => write!(f, "SWAP"),
			TokenType::Movret    => write!(f, "MOVRET"),
			TokenType::Pushret   => write!(f, "PUSHRET"),
			TokenType::Pusharg   => write!(f, "PUSHARG"),
			TokenType::Dumpstack => write!(f, "DUMPSTACK"),
			TokenType::Dumpframe => write!(f, "DUMPFRAME"),
			TokenType::Out       => write!(f, "OUT"),
			TokenType::Outln     => write!(f, "OUTLN"),
			TokenType::In        => write!(f, "IN"),
			TokenType::Store     => write!(f, "STORE"),
			TokenType::Storeidx  => write!(f, "STOREIDX"),
			TokenType::Load      => write!(f, "LOAD"),
			TokenType::Loadidx   => write!(f, "LOADIDX"),
			TokenType::Eq        => write!(f, "EQ"),
			TokenType::Neq       => write!(f, "NEQ"),
			TokenType::Leq       => write!(f, "LEQ"),
			TokenType::Leqf      => write!(f, "LEQF"),
			TokenType::Geq       => write!(f, "GEQ"),
			TokenType::Geqf      => write!(f, "GEQF"),
			TokenType::Lt        => write!(f, "LT"),
			TokenType::Ltf       => write!(f, "LTF"),
			TokenType::Gt        => write!(f, "GT"),
			TokenType::Gtf       => write!(f, "GTF"),
			TokenType::Compl     => write!(f, "COMPL"),
			TokenType::Minus     => write!(f, "MINUS"),
			TokenType::Minusf    => write!(f, "MINUSF"),
			TokenType::Add       => write!(f, "ADD"),
			TokenType::Addf      => write!(f, "ADDF"),
			TokenType::Div       => write!(f, "DIV"),
			TokenType::Divf      => write!(f, "DIVF"),
			TokenType::Rem       => write!(f, "REM"),
			TokenType::Remf      => write!(f, "REMF"),
			TokenType::Mul       => write!(f, "MUL"),
			TokenType::Mulf      => write!(f, "MULF"),
			TokenType::And       => write!(f, "AND"),
			TokenType::Or        => write!(f, "OR"),
			TokenType::Not       => write!(f, "NOT"),
			TokenType::Jmpnz     => write!(f, "JMPNZ"),
			TokenType::Jmp       => write!(f, "JMP"),
			TokenType::Subcall   => write!(f, "SUBCALL"),
			TokenType::Return    => write!(f, "RETURN"),
			TokenType::Lshift    => write!(f, "LSHIFT"),
			TokenType::Rshift    => write!(f, "RSHIFT"),
			TokenType::Xor       => write!(f, "XOR"),

		}
	}
}
