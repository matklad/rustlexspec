#!/usr/bin/env bash

set -e
cargo run --manifest-path syntex_tokenizer/Cargo.toml < $1 > syntex_tokenizer.txt
cargo run --manifest-path rustlexspec/Cargo.toml < $1 > rustlexspec_tokenizer.txt
diff syntex_tokenizer.txt rustlexspec_tokenizer.txt | head -n 12
