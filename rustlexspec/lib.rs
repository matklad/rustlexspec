#[macro_use] extern crate lazy_static;
extern crate regex;

// # **DRAFT** Rust lexical specification. **DRAFT**
//
// This document is the executable specification of the
// lexical structure of the Rust programming language.
//
// It describes an algorithm which transforms a Rust program
// (an utf-8 encoded string) into a sequence of tokens.

// Each token consists of a type and a positive length in bytes.

pub struct Token {
    pub token_type: TokenType,
    pub len: usize,
}

// The total length of all tokens in the sequence produced by
// the algorithm is equal to the length of the input. An empty
// input string produces an empty token sequence.
//
// Each token type is characterized by a name, a regular
// expression and an optional special rule.

#[derive(Clone, Copy)]
pub struct TokenType(
    &'static str,
    &'static str,
    Option<&'static fn(&str) -> Option<usize>>,
);

impl TokenType {
    pub fn name(&self) -> &str { self.0 }
    fn re(&self) -> &str { self.1 }
    fn rule(&self) -> Option<&fn(&str) -> Option<usize>> { self.2 }
}


// The algorithm works by repeatedly matching the start of
// the input against the token types' regular expressions,
// and selecting the one with longest match (keywords are
// preferred to identifiers). Special rules are used to
// determine the boundaries of nested comments and raw
// string literals.

// ## `tokenize` algorithm
//
// The algorithm splits the input string into
// a sequence of tokens. `None` result means that the
// input string contains a lexical error.
pub fn tokenize(input: &str) -> Option<Vec<Token>> {
    // 1. Initialize result with an empty vector
    let mut result = Vec::new();
    // and remaining input with the input text.
    let mut remaining = input;

    let mut total_len = 0;

    // 2. While the remaining input is not empty, repeat.
    while !remaining.is_empty() {
        // 3. Determine the next token type and length using
        // `first_token` algorithm.
        let token = match first_token(remaining) {
            Some((t, l)) => {
                assert!(l > 0);
                Token { token_type: t, len: l }
            },
            // 4. If this fails, report error.
            None => return None,
        };

        total_len += token.len;

        // 5. Advance remaining input.
        remaining = &remaining[token.len..];

        // 6. Add the token to the result
        result.push(token);
    }

    assert_eq!(total_len, input.len());
    Some(result)
}


// ## `first_token` algorithm
//
// The algorithm determines the type and the length
// of the token at the start of the input. `None`
// result means that the input contains a lexical error.
fn first_token(input: &str) -> Option<(TokenType, usize)> {
    use regex::Regex;
    assert!(!input.is_empty());

    lazy_static! {
        static ref PATTERNS: Vec<Regex> =
            TOKEN_TYPES.iter()
            .map(|t| Regex::new(&format!("^{}", t.re())).unwrap())
            .collect();
    }

    // 1. Find all tokens whose pattern match the start of
    // the input.
    let matches = PATTERNS.iter().zip(TOKEN_TYPES.iter())
        .filter_map(|(pattern, &token_type)| {
            match pattern.find(input) {
                Some((start, end)) => {
                    assert_eq!(start, 0);
                    Some((token_type, end))
                }
                None => None
            }
        }).collect::<Vec<_>>();

    // 2. Find the longest match.
    let (token_type, length) =
        match matches.iter().max_by_key(|&&(_, length)| length) {
            Some(&(token_type, length)) => (token_type, length),
            // 3. If none of the patterns matched, report
            // error.
            None => return None,
        };

    // It is guaranteed that the longest march is unique,
    // unless it is a clash between a keyword and an
    // identifier.
    let ambigious_matches = matches
        .iter()
        .filter(|&&(t, l)| l == length && t.name() != "identifier")
        .count();
    assert!(ambigious_matches <= 1);

    // 4. If there is a special rule associated with a
    // token, use it to determine the token's span.
    if let Some(rule) = token_type.rule() {
        return match rule(input) {
            Some(length) => Some((token_type, length)),
            None => None,
        }
    }

    return Some((token_type, length))
}


pub static TOKEN_TYPES: [TokenType; 42] = [
    TokenType("identifier", r"(_|\p{XID_Start})\p{XID_Continue}*", None),
    TokenType("_", r"_", None),
    TokenType("lifetime", r"'\p{XID_Continue}+", None),

    TokenType("block_comment", r"/\*", Some(&(block_comment_rule as fn(&str) -> Option<usize>))),
    TokenType("line_comment", r"//.*", None),
    TokenType("whitespace", r"\s+", None),

    TokenType("!", r"!", None),
    TokenType("!=", r"!=", None),
    TokenType("#", r"#", None),
    TokenType("&", r"&", None),
    TokenType("&&", r"&&", None),
    TokenType("(", r"\(", None),
    TokenType(")", r"\)", None),
    TokenType("+", r"\+", None),
    TokenType("+=", r"\+=", None),
    TokenType(",", r",", None),
    TokenType("-=", r"-=", None),
    TokenType("->", r"->", None),
    TokenType(".", r"\.", None),
    TokenType("..", r"\.\.", None),
    TokenType(":", r":", None),
    TokenType("::", r"::", None),
    TokenType(";", r";", None),
    TokenType("<", r"<", None),
    TokenType("<<", r"<<", None),
    TokenType("<=", r"<=", None),
    TokenType("=", r"=", None),
    TokenType("==", r"==", None),
    TokenType("=>", r"=>", None),
    TokenType(">", r">", None),
    TokenType(">=", r">=", None),
    TokenType(">>", r">>", None),
    TokenType("[", r"\[", None),
    TokenType("]", r"\]", None),
    TokenType("{", r"\{", None),
    TokenType("|", r"\|", None),
    TokenType("||", r"\|\|", None),
    TokenType("}", r"\}", None),

    // FIXME
    TokenType("char", r"'.'", None),
    TokenType("raw_string", r##"r#*""##, Some(&(raw_string_rule as fn(&str) -> Option<usize>))),
    TokenType("string", r#""[^"]*""#, None),
    TokenType("integer", r"\d+", None),
];


// ## `block_comment_rule` algorithm
//
// The algorithm matches block comments. Block comments can
// be nested and can't be described by a regular language.
fn block_comment_rule(mut input: &str) -> Option<usize> {
    assert!(input.starts_with("/*"));
    let mut level = 1;
    let mut length = 2;
    input = &input[2..];
    while level > 0 && !input.is_empty() {
        let next_stop = if input.starts_with("/*") {
            level += 1;
            2
        } else if input.starts_with("*/") {
            level -= 1;
            2
        } else {
            input.chars().next().unwrap().len_utf8()
        };
        length += next_stop;
        input = &input[next_stop..];
    }

    Some(length)
}

// ## `raw_string_rule` algorithm
//
// The algorithm matches raw string literals. Raw string
// literal starts with `r` symbol, n (possibly zero) number
// of `#` symbols and `"` symbol. It ends with `"` and n
// hashes. The string between quotes can contain any symbols
// except the `#` followed by n hashes. This language is not
// context free.
fn raw_string_rule(mut input: &str) -> Option<usize> {
    assert!(input.starts_with("r"));
    let mut length = 1;
    input = &input[1..];
    let n_hashes = count_leading_hashes(input);
    length += n_hashes;
    input = &input[n_hashes..];

    assert!(input.starts_with('"'));
    length += 1;
    input = &input[1..];

    while let Some(c) = input.chars().next() {
        input = &input[c.len_utf8()..];
        length += c.len_utf8();
        if c == '"' &&  n_hashes <= count_leading_hashes(input) {
            return Some(length + n_hashes);
        }
    }

    return None;

    fn count_leading_hashes(s: &str) -> usize {
        s.chars().take_while(|&c| c == '#').count()
    }
}


pub fn driver<F: Fn(&str) -> Option<Vec<Token>>>(f: F) {
    use std::io::{self, Read};

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).expect("Failed to read stdin");
    let tokens = f(&buffer).expect("Invalid rust file");

    for token in tokens.iter() {
        println!("{} {}", token.token_type.name(), token.len);
    }
}