//!
//!
//!

#[macro_use]
extern crate clap;

use std::fs::File;
use std::io::Read;

use clap::{App, Arg};
use cactus::Compiler;


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

	let compiler = Compiler::new();

	match compiler.compile(&contents) {
		Ok(instructions) => {
			for instr in instructions.iter() {
				println!("{}", instr);
			}
		},
		Err(errors) => {
			for error in errors.iter() {
				eprintln!("{}", error);
			}

			std::process::exit(1);
		}
	}
}
