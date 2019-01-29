//!
//!
//!

#[macro_use]
extern crate clap;
extern crate compiler;

use std::{
	env,
	error::Error,
	fs::File,
	io::{Read, Write},
};
use clap::{App, Arg};
use compiler::{
	parser::parser::Parser,
	code_gen::bytecode::{
		Module,
		ToBytecode,
		BytecodeNode,
	},
};

fn main() {
	let matches = App::new("Cactus compiler")
		.version(crate_version!())
		.author("Matt Dowdell <mdowdell244@gmail.com>")
		.about("Entry point for compiling Cactus programs")
		.arg(Arg::with_name("output")
			.help("The output file to create. Defaults to \"output.smac\"")
			.takes_value(true)
			.short("o")
			.long("output")
		)
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

	let output = matches.value_of("output").unwrap_or("output.smac");

	match compile(&contents) {
		Ok(bytecode) => save_to_file(output, &bytecode),
		Err(_) => std::process::exit(1),
	};
}


fn compile(input: &str) -> Result<String, ()> {
	let mut parser = Parser::new(input);

	parser.parse();

	if parser.has_errors() {
		for error in parser.errors {
			eprintln!("{}", error);
		}

		return Err(());
	}

	let mut module = Module::new();

	for statement in parser.module.statements {
		let bytecode = statement.to_bytecode();
		module.extend(bytecode);
	}

	Ok(module.to_code())
}

fn save_to_file(filename: &str, content: &str) {
	let mut path = env::current_dir().unwrap();
	path.push(format!("{}", filename));
    let display = path.display();

	let mut file = match File::create(&path) {
     	Err(why) => panic!("Failed to create {}: {}", display, why.description()),
        Ok(file) => file,
    };

    match file.write_all(content.as_bytes()) {
        Err(why) => {
            panic!("Unable to write to {}: {}", display, why.description())
        },
        Ok(_) => {},
    }
}
