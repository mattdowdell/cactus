//! Integration tests for examples for Maude.

use std::{
	env, fmt,
	io::Write,
	process::{Command, Output, Stdio},
};

//
//
//
#[derive(Debug, PartialEq)]
enum Examples {
	Compl,
	Cond,
	Div,
	Dumpstack,
	Ex1,
	Ex2,
	Ex3,
	Ex4,
	Ex5,
	Ex6,
	Fib,
	Flush,
	Lab,
	Leq,
	Mul,
	Neq,
	SubcallReturn,
	SubroutineAdd,
	Swap,
}

impl fmt::Display for Examples {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Examples::Compl => write!(f, "examples/bytecode/compl.smac"),
			Examples::Cond => write!(f, "examples/bytecode/cond.smac"),
			Examples::Div => write!(f, "examples/bytecode/div.smac"),
			Examples::Dumpstack => write!(f, "examples/bytecode/dumpstack.smac"),
			Examples::Ex1 => write!(f, "examples/bytecode/ex1.smac"),
			Examples::Ex2 => write!(f, "examples/bytecode/ex2.smac"),
			Examples::Ex3 => write!(f, "examples/bytecode/ex3.smac"),
			Examples::Ex4 => write!(f, "examples/bytecode/ex4.smac"),
			Examples::Ex5 => write!(f, "examples/bytecode/ex5.smac"),
			Examples::Ex6 => write!(f, "examples/bytecode/ex6.smac"),
			Examples::Fib => write!(f, "examples/bytecode/fib.smac"),
			Examples::Flush => write!(f, "examples/bytecode/flush.smac"),
			Examples::Lab => write!(f, "examples/bytecode/lab.smac"),
			Examples::Leq => write!(f, "examples/bytecode/leq.smac"),
			Examples::Mul => write!(f, "examples/bytecode/mul.smac"),
			Examples::Neq => write!(f, "examples/bytecode/neq.smac"),
			Examples::SubcallReturn => write!(f, "examples/bytecode/subcall_return.smac"),
			Examples::SubroutineAdd => write!(f, "examples/bytecode/subroutine_add.smac"),
			Examples::Swap => write!(f, "examples/bytecode/swap.smac"),
		}
	}
}

fn run_example(example: Examples) -> Output {
	let mut path = env::current_dir().unwrap();

	path.push(format!("{}", example));
	assert!(
		path.is_file(),
		format!("Path is not a file: {}", path.display())
	);

	let output = Command::new("cargo")
		.arg("run")
		.arg("--bin")
		.arg("maude")
		.arg("--")
		.arg(path.to_str().unwrap())
		.output()
		.expect("Failed to run example");

	println!("status: {}", output.status);
	println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
	println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
	assert!(output.status.success());

	output
}

#[test]
fn test_compl() {
	let output = run_example(Examples::Compl);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"NOTZERO".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_cond() {
	let output = run_example(Examples::Cond);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"ZERO".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_div() {
	let output = run_example(Examples::Div);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"1".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_dumpstack() {
	let output = run_example(Examples::Dumpstack);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"[Integer(10), Integer(20)]".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_ex1() {
	let output = run_example(Examples::Ex1);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"x + y = 3".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_ex2() {
	let output = run_example(Examples::Ex2);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"1".to_string(),
		"2".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_ex3() {
	let output = run_example(Examples::Ex3);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"Hello,World".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_ex4() {
	let output = run_example(Examples::Ex4);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"4".to_string(),
		"3".to_string(),
		"2".to_string(),
		"1".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_ex5() {
	let output = run_example(Examples::Ex5);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"x = 10".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_ex6() {
	let output = run_example(Examples::Ex6);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"12".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_fib() {
	let mut path = env::current_dir().unwrap();

	path.push(format!("{}", Examples::Fib));
	assert!(
		path.is_file(),
		format!("Path is not a file: {}", path.display())
	);

	let mut child = Command::new("cargo")
		.arg("run")
		.arg("--bin")
		.arg("maude")
		.arg("--")
		.arg(path.to_str().unwrap())
		.stdin(Stdio::piped())
		.stdout(Stdio::piped())
		.stderr(Stdio::piped())
		.spawn()
		.unwrap();

	child.stdin.as_mut().unwrap().write_all(b"5\r").unwrap();
	let output = child.wait_with_output().expect("Failed to wait for child");

	println!("status: {}", output.status);
	println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
	println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
	assert!(output.status.success());

	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"Enter a number: fib(5) = 5".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_flush() {
	let mut path = env::current_dir().unwrap();

	path.push(format!("{}", Examples::Flush));
	assert!(
		path.is_file(),
		format!("Path is not a file: {}", path.display())
	);

	let mut child = Command::new("cargo")
		.arg("run")
		.arg("--bin")
		.arg("maude")
		.arg("--")
		.arg(path.to_str().unwrap())
		.stdin(Stdio::piped())
		.stdout(Stdio::piped())
		.stderr(Stdio::piped())
		.spawn()
		.unwrap();

	child.stdin.as_mut().unwrap().write_all(b"5\r").unwrap();
	let output = child.wait_with_output().expect("Failed to wait for child");

	println!("status: {}", output.status);
	println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
	println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
	assert!(output.status.success());

	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"Enter n: You entered: 5".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_lab() {
	let output = run_example(Examples::Lab);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"10".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_leq() {
	let output = run_example(Examples::Leq);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"0".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_mul() {
	let output = run_example(Examples::Mul);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"100".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_neq() {
	let output = run_example(Examples::Neq);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"ZERO".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_subcall_return() {
	let output = run_example(Examples::SubcallReturn);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"1".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_subroutine_add() {
	let output = run_example(Examples::SubroutineAdd);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"30".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}

#[test]
fn test_swap() {
	let output = run_example(Examples::Swap);
	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut lines = stdout.lines();
	let expected = vec![
		"[Integer(20), Integer(10)]".to_string(),
		"[Integer(10), Integer(20)]".to_string(),
		"Evaluation completed without errors".to_string(),
	];

	for exp in expected.iter() {
		assert_eq!(lines.next().unwrap(), exp);
	}
}
