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

## Notes
- programming lan-guages were designed that could be translated from what humans understand to what machines understand
- Low-level languages, such as assembly, are very close
- which is some ways are relatively specialist, - not sure if this sentence makes sense?
- A small example of its syntax can be seen above (right)
- The next step is to convert the assembly output to be for the embedded system, which will be the ARM v7-M in-struction set.
