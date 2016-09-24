#!/usr/bin/env bash

set -e

for file in $(find . -type f -name "*.rs"); do
    echo $file
    cargo run --release --manifest-path syntex_tokenizer/Cargo.toml < $file > syntex_tokenizer.txt
    cargo run --release --manifest-path rustlexspec/Cargo.toml < $file > rustlexspec_tokenizer.txt
    diff syntex_tokenizer.txt rustlexspec_tokenizer.txt | head -n 12
done
