# Cactus

Cactus is a high-level language that compiles to bytecode. This package provides a compiler for that language and an interpreter for the bytecode.

## Usage

The compiler and interpreter are implemented in Rust. To build, we use Cargo:

```bash
# build the debug versions
cargo build

# build the release versions
cargo build --release
```

The debug versions can be invoked using Cargo:

```bash
# the compiler
cargo run --bin cactusc

# the interpreter
cargo run --bin maude
```

The release versions can be found in the `target/release` directory after the build.

## Tests

To run the tests, we also use cargo:

```bash
cargo test
```
