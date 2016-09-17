extern crate rustlexspec;

fn main() {
    use std::io::{self, Read};

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).expect("Failed to read stdin");
    let tokens = rustlexspec::tokenize(&buffer).expect("Invalid rust file");

    for token in tokens.iter() {
        println!("{:20} {:<5} {:<5} {:?}",
                 token.token_type.name(),
                 token.span.0, token.span.1,
                 token.text(&buffer));
    }
}
