//!
//!
//!

#[macro_use]
extern crate clap;

use std::fs::File;
use std::io::Read;

use clap::{App, Arg};
use cactus::Interpreter;


fn main() {
	let matches = App::new("Maude interpreter")
		.version(crate_version!())
		.author("Matt Dowdell <mdowdell244@gmail.com>")
		.about("Entry point for interpreting Maude programs")
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

	let interpreter = Interpreter::new();

	match interpreter.interpret(&contents) {
		Ok(_) => {
			println!("Evaluation completed without errors");
		},
		Err(errors) => {
			for error in errors.iter() {
				eprintln!("{}", error);
			}

			std::process::exit(1);
		}
	}
}
