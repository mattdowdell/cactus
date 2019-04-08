//! A helper for creating labels in bytecode.

use std::fmt;

/// A representation of a label in bytecode.
#[derive(Clone, Debug, PartialEq)]
pub struct Label {
	label_type: LabelType,
	block_id: usize,
}

impl Label {
	/// Create a new instance of `Label`.
	pub fn new(label_type: LabelType, block_id: usize) -> Label {
		Label {
			label_type: label_type,
			block_id: block_id,
		}
	}
}

impl fmt::Display for Label {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}_{}", self.label_type, self.block_id)
	}
}

/// The possible label types.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LabelType {
	LoopStart,
	LoopEnd,
	IfStart,
	IfBody,
	IfEnd,
}

impl fmt::Display for LabelType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			LabelType::LoopStart => write!(f, "loop_start"),
			LabelType::LoopEnd   => write!(f, "loop_end"),
			LabelType::IfStart   => write!(f, "if_start"),
			LabelType::IfBody    => write!(f, "if_body"),
			LabelType::IfEnd     => write!(f, "if_end"),
		}
	}
}
