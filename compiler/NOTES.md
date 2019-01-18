
```
let <ident> = <expr>;
let <ident>: <type> = <expr>;
let <ident>: bool = <expr>;

return <expr>;

fn <ident>() {}
fn <ident>(<ident>: <type>) {}
fn <ident>(<ident>: <type>) -> <type> {}

struct <ident> {
	<ident>: <type>,
	<ident>: <type>,
}

enum <ident> {
	<ident> = <literal>,
	<ident> = <literal>,
}

for <ident> in <expr> {}
```
```
fn fib(n: i32) -> i32 {
	let a = 1;
	let b = 1;

	for i in 3..=n {
		let c: i32 = a + b;
		a = b;
		b = c;
	}

	return b;
}
```
