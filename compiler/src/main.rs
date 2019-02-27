//!
//!
//!

#[macro_use]
extern crate clap;
extern crate cactus;

use std::{
	fs::File,
	io::Read,
};
use clap::{App, Arg};
use cactus::{
	parser::ast::Ast,
	parser::parser::Parser,
	analyser::analyser::Analyser,
};

fn main() {
	let matches = App::new("Cactus compiler")
		.version(crate_version!())
		.author("Matt Dowdell <mdowdell244@gmail.com>")
		.about("Entry point for compiling Cactus programs")
		.arg(
			Arg::with_name("FILENAME")
				.help("Sets the input file(s) to use")
				.required(true)
				.multiple(true)
		).get_matches();


	let mut contents = String::new();

	for filename in matches.values_of("FILENAME").unwrap() {
		let f = File::open(filename);

		if f.is_err() {
			eprintln!("Unable to open file: {}", filename);
			std::process::exit(1);
		}


		f.unwrap()
			.read_to_string(&mut contents)
			.expect("Unable to read file");
	}

	compile(&contents);
}


fn compile(input: &str) {
	let mut parser = Parser::new(input);
	let res = parser.parse();

	if res.is_err() {
		let errors = res.err().unwrap();

		for error in errors.iter() {
			eprintln!("{}", error);
		}

		std::process::exit(1);
	}

	let module = res.unwrap();
	let mut ast = Ast::new();
	ast.push(module);

	let mut analyser = Analyser::new(ast);

	match analyser.populate_symbol_table() {
		Ok(table) => {
			dbg!(table);
			println!("No errors detected!");
		},
		Err(errors) => {
			for error in errors.iter() {
				eprintln!("{}", error);
			}

			std::process::exit(1);
		}
	}
}
