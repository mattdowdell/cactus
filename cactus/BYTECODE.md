# maude

A simple pushdown stack virtual machine based on [smachine](https://bitbucket.org/benedict_gaster/smachine).

Examples can be found in the [`examples/bytecode`](./examples/bytecode) directory.

## Syntax
Maude programs start at a label named `main` and should finish with a `HALT` instruction.

```
main:
    HALT;
```

The language is defined by the following rules:

- Labels can be any ASCII alphabetic character (`A-Z` or `a-z`) or an underscore (`_`) followed by the same or any ASCII digit (`0-9`).
- A label definition should be followed by a colon (`:`).
- Instructions are always uppercase and are followed by a semicolon (`;`).
- Comments can appear anywhere in a program with the following exceptions:
    - Comments cannot be inserted between a label definition and the following colon.
    - Comments cannot be inserted between an instruction and the following semicolon.

A demonstration of these rules is as follows:

```
{- This is a block comment

   It can span multiple lines -}

main:
    -- This is an inline comment that finishes at the end of the line
    HALT;

LABEL1:
    NOP;

label2:
    NOP;

_label3:
    NOP;
```

See [§Grammar](#grammar) for the full grammar. The `examples` directory also has a number of example programs using various instructions.

A syntax definition for Sublime Text 3 can be found in [`Maude.sublime-syntax`](./Maude.sublime-syntax). To install this definition, copy it to the user packages directory:

- Linux: `~/.config/sublime-text/Packages/User/`
- Windows: ?

## Grammar
```
module : comment* | inst ';' comment?

comment: `-- .*\n
       | '{-' .* '-}'

inst : `NOP`
     | `HALT`
     | `PUSH` literal
     | `POP`
     | `DUP`
     | `SWAP`
     | `DUMPSTACK`
     | `OUT`
     | `OUTLN`
     | `IN`
     | labeldef
     | `&`id
     | `STORE`
     | `LOAD`
     | `EQ`
     | `NEQ`
     | `LEQ`
     | `GEQ`
     | `LT`
     | `GT`
     | `COMPL`
     | `MINUS`
     | `ADD`
     | `DIV`
     | `REM`
     | `MUL`
     | `AND`
     | `OR`
     | `NOT`
     | `JMPNZ`
     | `JMP`
     | `SUBCALL`
     | `RETURN`

labeldef : id `:`

literal : int | double | string

int : -?[0-9]+

double : -?[0-9]+'.'[0-9]+

string : \"[0-9A-Za-z!#$%&'()*+,-./:;<=>?@[]^_`{|}~ ]*\"
```

## Instructions
### Machine Control
#### NOP
The `NOP` instruction does nothing. It is most often used as a placeholder after a label.

```
example:
    NOP;
```

#### HALT
The `HALT` instruction must be used at the end of a program.

```
main:
    HALT;
```

### Stack Control
The following instruction implement stack control, i.e. adding values to or removing values from the stack. If there are insufficient values on the stack for the instruction to complete, an error will be raised and the program will exit.

The stack is a LIFO data structure, i.e. last in, first out, where the last value added to the stack will be the next to be removed from it.

#### PUSH
The `PUSH` instruction adds a literal value to the top of the stack. A literal can be one of:

- Integer
- Double
- String
- Character (not implemented)

It can also be a symbol:

- `LOCALS`: The locals pointer for a function.
- `ARGS`: The arguments pointer for a function.

```
main:
    PUSH 5;
    HALT;
```

#### POP
The `POP` instruction removes the value on the top of the stack. This instruction discards whatever value it finds so it is only useful for removing unwanted values.

```
main:
    PUSH 5;
    POP;
    HALT;
```

#### DUP
The `DUP` instruction duplicates the top value on the stack.

```
main:
    PUSH 5;
    DUP;
    -- now we can pop twice
    POP;
    POP;
    HALT;
```

#### SWAP
The `SWAP` instruction swaps the top 2 values on the stack.

```
main:
    PUSH 5;
    PUSH 10;
    SWAP;
    POP;     -- removes 5
    POP;     -- removes 10
    HALT;
```

#### DUMPSTACK
The `DUMPSTACK` instruction outputs the contents of the stack to `STDOUT`. This does not remove values from the stack. This instruction is only useful for inspecting the stack during debugging.

```
main:
    PUSH 5;
    DUMPSTACK;
    HALT;
```

### Return Register
Return register instructions are used for setting and getting the return register. The return register contains the value returned by a function.

In the below example written in C, the return register would be set to `1`:

```c
int example(void) {
    return 1;
}
```

These instructions are intended to be used with the instructions [`SUBCALL`](#subcall) and [`RETURN`](#return).

#### MOVRET
The `MOVRET` instruction takes the value currently stored in the return register and pushes it onto the stack. If the return register does not have a value stored in it, an error will be raised.

```
main:
    &example;
    SUBCALL; -- call the example function
    MOVRET;  -- take the value in the return register and push it onto the stack.
    OUTLN;
    HALT;

example:
    PUSH 1;  -- push the integer 1 onto the stack
    PUSHRET; -- pop the top value on the stack and store it in the return register
    RETURN;  -- exit the function
```

#### PUSHRET
The `PUSHRET` instruction removes the top value from the top of the stack and stores it in the return register.

```
main:
    &example;
    SUBCALL; -- call the example function
    MOVRET;  -- take the value in the return register and push it onto the stack.
    OUTLN;
    HALT;

example:
    PUSH 1;  -- push the integer 1 onto the stack
    PUSHRET; -- pop the top value on the stack and store it in the return register
    RETURN;  -- exit the function
```

### Arguments
#### ALLOCA
_Not implemented_

```
pops A, allocate A space in the local array
```

#### PUSHARG
The `PUSHARG` instruction removes the top item from the stack and pushes it onto the argument stack for the next subroutine call.

Note that the callee argument stack is empty on entry to the function and cleared on returning from a subcall. _TODO: clarify this._

For more information see:

- [§`SUBCALL`](#subcall)
- [§`RETURN`](#return)

```
main:
    PUSH 5;
    PUSHARG;
    &example;
    SUBCALL;
    HALT;

example:
    RETURN;
```

```
pop A, pushes A onto the argument stack for the next subroutine call. Note that the callee argument stack is empty on entry to the function and cleared on returning from a SUBCALL)
```

### IO Control
#### OUT
The `OUT` instruction outputs the top value on the stack to `STDOUT`.

```
main:
    PUSH 5;
    OUT;    -- outputs "5"
    HALT;
```

#### OUTLN
The `OUT` instruction outputs the top value on the stack to `STDOUT` followed by a newline.

```
main:
    PUSH 5;
    OUTLN;  -- outputs "5\n"
    HALT;
```

#### IN
The `IN` instruction reads an integer value from `STDIN` and adds it to the top of the stack. An error will be raised if a non-integer is supplied.


```
main:
    IN;    -- reads an integer
    OUTLN; -- outputs "<int>\n"
    HALT;
```

### Locations and Addresses
#### Label Definition
A label definition names a location in the program that can be moved to. For more information on how to move to locations, see [§Control Flow](#control-flow).

```
main:       -- defines the "main" label
    HALT;

location_1: -- defines the "location_1" label
    NOP;

location_2: -- defines the "location_2" label
    NOP;
```

#### Push Address
The push address instruction pushes the address of a named location onto the stack. The address can then be used by [§Control Flow](#control-flow) instructions or [§Memory Operations](#memory-operations).

A named location must be defined before it's location can be taken.

```
main:
    &example;
    HALT;

example:
    NOP;
```

### Memory Operations
Memory operation instructions can be used to store values in or load values from specific addresses as opposed to adding values to or removing values from the top of the stack.

Addresses can be locations within the instruction stream or indexes within the stack.

#### STORE
The `STORE` instruction removes an address and value from the stack, where `A` is the address and `B` is the value. `B` is then written to the address given by `A`.

```
main:
    PUSH 5;
    &example;
    STORE;    -- store the integer 5 in the address of "example"
    HALT;

example:
    NOP;
```

#### STOREIDX
The `STOREIDX` instruction removes a symbol, an integer and value from the stack, where `A` is the symbol, `B` is the integer and `C` is the value. A pointer is calculated by adding the integer `B` to the pointer given by the symbol `A`. `C` is then written to the new pointer. As the `ARGS` and `LOCALS` symbols are pointers to the start of their respective arrays, this instruction allows manipulation of multiple objects outside of the global stack.

The below example is the equivalent to the following C code:

```c
int main(int argc, char **argv) {
    int result = add(1, 2);
    printf("%d\n");

    return 0;
}

int add(int a, int b) {
    int result = a + b;
    return result;
}
```

```
main:
    PUSH 1;
    PUSHARG;     -- push 1 onto the callee arguments stack
    PUSH 2;
    PUSHARG;     -- push 2 onto the callee arguments stack

    &add;
    SUBCALL;     -- call the "add" function

    MOVRET;      -- push the value in the return register onto the stack
    OUTLN;       -- output the result of the addition to STDOUT

    HALT;

add:
    PUSH ARGS;
    PUSH 0;
    LOADIDX;     -- push ARGS[0] onto the stack

    PUSH ARGS;
    PUSH 1;
    LOADIDX;     -- push ARGS[1] onto the stack

    ADD;         -- add ARGS[0] to ARGS[1]

    PUSH LOCALS;
    SWAP;
    PUSH 0;
    SWAP;
    STOREIDX;    -- store the result of the addition in LOCALS[0]

    PUSH LOCALS;
    LOAD;        -- push LOCALS[0] onto the stack

    PUSHRET;     -- store the result of the addition in the return register

    RETURN;
```

```
pop A, pop B, pop C, write C to address (A+B)
```

#### LOAD
The `LOAD` instruction removes an address from the stack, loads a value from that address and pushes that value to the top of the stack.

```
main:
    -- store the integer 5 in the address of "example"
    PUSH 5;
    &example;
    STORE;

    -- load the integer 5 from the address of "example"
    &example;
    LOAD;

    HALT;

example:
    NOP;
```

#### LOADIDX
The `LOADIDX` instruction removes an integer and symbol from the stack, where `A` is the index and `B` is the symbol.  The integer `A` is then added to the pointer given by the symbol `B` and the value at the new pointer is pushed onto the top of the stack. As the `ARGS` and `LOCALS` symbols are pointers to the start of their respective arrays, this instruction allows access to multiple arguments.

The below example is equivalent to the following C code:

```c
#include <stdio.h>

int main(int argc, char **argv) {
    example(1, 2);
    return 0;
}

void example(int a, int b) {
    printf("%d\n", a);
    printf("%d\n", b);
}
```

```
main:
    PUSH 1;
    PUSHARG;
    PUSH 2;
    PUSHARG;
    &example;
    SUBCALL;
    HALT;

example:
    PUSH ARGS;
    PUSH 0;
    LOADIDX;   -- push ARGS[0] onto the stack
    OUTLN;     -- output ARGS[0] to STDOUT

    PUSH ARGS;
    PUSH 1;
    LOADIDX;   -- push ARGS[1] onto the stack
    OUTLN;     -- output ARGS[1] to STDOUT

    RETURN;
```

### Integer Operations
All integer operations require the values being removed from the stack to be integers. If this is not the case, an error will be raised.

Integer operations fall into 3 categories:

- Boolean operations, i.e. `A == B`.
- Mathematical operations, i.e. `A + B`.
- Bitwise operations, i.e. `A & B`.

#### EQ
The `EQ` instruction removes the top 2 integers from the stack and compares them for equality. If they are equal, `0` will be pushed onto the stack, otherwise `1` will be pushed to the stack.

```
main:
    PUSH 5;
    PUSH 5;
    EQ;
    HALT;
```

#### NEQ
The `NEQ` instruction removes the top 2 integers from the stack and compares them for equality. If they are not equal, `0` will be pushed onto the stack, otherwise `1` will be pushed to the stack.

```
main:
    PUSH 5;
    PUSH 6;
    NEQ;
    HALT;
```

#### LEQ
The `LEQ` instruction removes the top 2 integers from the stack and compares them, where `A` is the first integer removed and `B` is the second. If `A <= B`, `0` will be pushed onto the stack, otherwise `1` will be pushed to the stack.

```
main:
    PUSH 5;
    PUSH 4;
    LEQ;
    HALT;
```

#### GEQ
The `GEQ` instruction removes the top 2 integers from the stack and compares them, where `A` is the first integer removed and `B` is the second. If `A >= B`, `0` will be pushed onto the stack, otherwise `1` will be pushed to the stack.

```
main:
    PUSH 5;
    PUSH 6;
    GEQ;
    HALT;
```

#### LT
The `LT` instruction removes the top 2 integers from the stack and compares them, where `A` is the first integer removed and `B` is the second. If `A < B`, `0` will be pushed onto the stack, otherwise `1` will be pushed to the stack.

```
main:
    PUSH 5;
    PUSH 6;
    LT;
    HALT;
```

#### GT
The `LT` instruction removes the top 2 integers from the stack and compares them, where `A` is the first integer removed and `B` is the second. If `A < B`, `0` will be pushed onto the stack, otherwise `1` will be pushed to the stack.

```
main:
    PUSH 5;
    PUSH 6;
    LT;
    HALT;
```

#### NOT
The `NOT` instruction removes the top integer from the stack, here named `A`. If `A == 0`, `1` is pushed onto the stack. Otherwise, `0` is pushed onto the stack.

```
main:
    PUSH 1;
    NOT;
    HALT;
```

#### MINUS
The `MINUS` instruction removes the top 2 integers from the stack, where `A` is the first integer removed and `B` is the second. The result of `A - B`, is then pushed onto the stack.

```
main:
    PUSH 5;
    PUSH 6;
    MINUS;
    HALT;
```

#### ADD
The `ADD` instruction removes the top 2 integers from the stack, where `A` is the first integer removed and `B` is the second. The result of `A + B`, is then pushed onto the stack.

```
main:
    PUSH 5;
    PUSH 6;
    ADD;
    HALT;
```

#### MUL
The `MUL` instruction removes the top 2 integers from the stack, where `A` is the first integer removed and `B` is the second. The result of `A * B` (multiplication), is then pushed onto the stack.

```
main:
    PUSH 5;
    PUSH 6;
    MUL;
    HALT;
```

#### DIV
The `DIV` instruction removes the top 2 integers from the stack, where `A` is the first integer removed and `B` is the second. The result of `A / B` (division), is then pushed onto the stack. If the division would not normally return an integer, the result will be rounded down to the nearest integer, e.g. `7 / 2 == 3`.

```
main:
    PUSH 5;
    PUSH 30;
    DIV;
    HALT;
```

#### REM
The `REM` instruction removes the top 2 integers from the stack, where `A` is the first integer removed and `B` is the second. The result of `A % B` (modulus), is then pushed onto the stack, e.g. `7 % 2 == 1`

```
main:
    PUSH 5;
    PUSH 31;
    REM;
    HALT;
```

#### AND
The `AND` instruction removes the top 2 integers from the stack, where `A` is the first integer removed and `B` is the second. The result of `A & B` (bitwise and), is then pushed onto the stack, e.g. `7 & 2 == 2`, or:

```
0b0111
0b0010 &
------
0b0010
------
```

```
main:
    PUSH 2;
    PUSH 7;
    AND;
    HALT;
```

#### OR
The `OR` instruction removes the top 2 integers from the stack, where `A` is the first integer removed and `B` is the second. The result of `A | B` (bitwise or), is then pushed onto the stack, e.g. `7 | 2 == 7`, or:

```
0b0111
0b0010 |
------
0b0111
------
```

```
main:
    PUSH 2;
    PUSH 7;
    OR;
    HALT;
```

#### COMPL
The `COMPL` instruction removes the top integer from the stack, here named `A`. The result of `~A` (bitwise not), is then pushed onto the stack, e.g. `~7 == 8`, or:

```
0b0111 ~
------
0b1000
------
```

The above assumes a 4 bit integer, whereas integers in Maude are 32 bits long. Therefore, the `COMPL` operation would actually be:

```
0b00000000000000000000000000000111 ~
----------------------------------
0b11111111111111111111111111111000
----------------------------------

```

Therefore the result is `-8` as integers in Maude are signed.

```
main:
    PUSH 7;
    COMPL;
    HALT;
```

### Control Flow
#### JMP
The `JMP` instruction removes the top value from the stack, here named `A`, which is an address. Control is transferred/moved to the address given by `A`.

```
main:
    &example;
    JMP;

example:
    HALT;
```

#### JMPNZ
The `JMPNZ` instruction removes the top 2 values from the stack, where `A` is the first and an address and `B` is the second and an integer. If `B != 0`, control is transferred/moved to the address given by `A`.

```
main:
    PUSH 1;
    &notzero;
    JMPNZ;

    PUSH "ZERO";
    OUTLN;
    HALT;

notzero:
    PUSH "NOTZERO"
    OUTLN;
    HALT;
```

#### SUBCALL
The `SUBCALL` instruction is responsible for transferring control to an address in a similar way to the `JMP` nstruction. However, the difference is that `SUBCALL` creates a new context known as a frame. Therefore, it is equivalent to calling a function rather than using a [`goto`](https://en.wikipedia.org/wiki/Goto) statement.

The new frame contains the local variables for the function, which for Maude is 3 arrays:

- Callee arguments - Populated by the instruction `PUSHARG`.
- Arguments - Referenced by the symbol `ARGS`.
- Locals - Referenced by the symbol `LOCALS`.

When a new child frame is created, the parent's callee arguments are copied to it, while the child's callee args and locals are initialised to empty arrays. The 3 arrays can only be manipulated by the current context and are inaccessible from outside it. However, the stack itself remains global and can be manipulated from anywhere in a program.

The steps taken by `SUBCALL` can be summarised as:

- Remove the top value from the stack, here named `A`, which must be an address.
- Create a new frame.
- Copy the current frame's callee arguments array to the new frame's argument array.
- Push the address of the instruction following `SUBCALL` onto the return stack to be used by a subsequent `RETURN` instruction.
- Transfer control to `A`, swapping in the context given by the new frame.

Functions called by `SUBCALL` are expected to use a `RETURN` instruction at the end of the function, although they may use a `HALT` instruction to stop execution immediately in a similar way to the `exit` system call.

```
main:
    PUSH 5;
    PUSHARG;
    &print_arg;
    SUBCALL;
    HALT;

-- takes 1 argument and outputs it to STDOUT
print_arg:
    PUSH ARGS;
    LOAD;
    OUTLN;
    RETURN;
```

#### RETURN
The `RETURN` instruction can only be used from within a context invoked by the `SUBCALL` instruction. It is equivalent to exiting from a called function back to the parent function that called it.

The equivalent C code for this instruction would be:

```c
int main(int argc, char **argv) {
    example();

    return 0;
}

void example(void) {
    return; // use the RETURN instruction here
}
```

The instruction performs the following:

- Remove the top value from the return stack, here named `A`, which must be an address. This will be the address of th instruction following the `SUBCALL` instruction that called the current function.
- Reclaim the current frame's callee arguments, arguments and locals arrays, i.e. delete the current frame.
- Reload the parent frame and empty it's callee arguments array.
- Transfer control to A.

```
main:
    PUSH 5;
    PUSHARG;
    &print_arg;
    SUBCALL;
    HALT;

-- takes 1 argument and outputs it to STDOUT
print_arg:
    PUSH ARGS;
    LOAD;
    OUTLN;
    RETURN;
```
