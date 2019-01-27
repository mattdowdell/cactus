# Poster

## High-Level Language

```rust
/**
 * Calculate the square of a number
 */
fn square(n: i32) -> i32 {
	return n * n;
}
```

## Bytecode
```
square:
	PUSH ARGS;
	LOAD;
	PUSH ARGS;
	LOAD;
	MUL;
	MOVRET;
	RETURN;
```

## Assembly
```assembly
square:
	push	rbp
	mov		rbp, rsp
	mov		DWORD PTR [rbp-4], edi
	mov		eax, DWORD PTR [rbp-4]
	imul	eax, DWORD PTR [rbp-4]
	pop		rbp
	ret
```
