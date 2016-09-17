# Pragmatic Rust lexer specification.

**WIP**

This is an attempt to specify Rust lexical structure in pragmatic and
declarative way.

Declarativeness achieved by using regular expressions and an intuitive tokenizer
algorithm.

Non regular and non context free parts of the language are handled pragmatically
with a hand written matching function.


```
cargo run < src/lib.rs
```
