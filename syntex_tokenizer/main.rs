extern crate rustlexspec;
extern crate syntex_tokenizer;

fn main() {
    rustlexspec::driver(syntex_tokenizer::tokenize);
}
