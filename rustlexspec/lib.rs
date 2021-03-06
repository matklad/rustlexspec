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

type Token = (&'static str, usize);

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
    pub fn name(&self) -> &'static str { self.0 }
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
                assert!(l > 0, "{}", t.name());
                (t.name(), l)
            },
            // 4. If this fails, report error.
            None => return None,
        };

        total_len += token.1;

        // 5. Advance remaining input.
        remaining = &remaining[token.1..];

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
            .map(|t| Regex::new(&format!("^((?x){})", t.re())).unwrap())
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
    assert!(ambigious_matches <= 1, "ambiguity: {:?}", &input[..20]);

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


pub static TOKEN_TYPES: &'static [TokenType] = &[
    TokenType("identifier", r"(_|\p{XID_Start})\p{XID_Continue}*", None),
    TokenType("_", r"_", None),
    TokenType("comment", r"//([^/!\n].*)?", None),
    TokenType("comment", r"/\*[^*!]", Some(&(block_comment_rule as fn(&str) -> Option<usize>))),
    TokenType("doc_comment", r"///.*", None),
    TokenType("doc_comment", r"//!.*", None),
    TokenType("doc_comment", r"/\*[*!]", Some(&(block_comment_rule as fn(&str) -> Option<usize>))),
    TokenType("lifetime", r"'\p{XID_Continue}+", None),
    TokenType("whitespace", r"\s+", None),
    TokenType("!", r"!", None),
    TokenType("!=", r"!=", None),
    TokenType("#", r"\x23", None),
    TokenType("$", r"\$", None),
    TokenType("%", r"%", None),
    TokenType("%=", r"%=", None),
    TokenType("&", r"&", None),
    TokenType("&=", r"&=", None),
    TokenType("&&", r"&&", None),
    TokenType("(", r"\(", None),
    TokenType(")", r"\)", None),
    TokenType("*", r"\*", None),
    TokenType("*=", r"\*=", None),
    TokenType("+", r"\+", None),
    TokenType("+=", r"\+=", None),
    TokenType(",", r",", None),
    TokenType("-", r"-", None),
    TokenType("-=", r"-=", None),
    TokenType("->", r"->", None),
    TokenType(".", r"\.", None),
    TokenType("..", r"\.\.", None),
    TokenType("...", r"\.\.\.", None),
    TokenType("/", r"/", None),
    TokenType("/=", r"/=", None),
    TokenType(":", r":", None),
    TokenType("::", r"::", None),
    TokenType(";", r";", None),
    TokenType("<", r"<", None),
    TokenType("<<", r"<<", None),
    TokenType("<<=", r"<<=", None),
    TokenType("<=", r"<=", None),
    TokenType("=", r"=", None),
    TokenType("==", r"==", None),
    TokenType("=>", r"=>", None),
    TokenType(">", r">", None),
    TokenType(">=", r">=", None),
    TokenType(">>", r">>", None),
    TokenType(">>=", r">>=", None),
    TokenType("@", r"@", None),
    TokenType("[", r"\[", None),
    TokenType("]", r"\]", None),
    TokenType("^", r"\^", None),
    TokenType("^=", r"\^=", None),
    TokenType("{", r"\{", None),
    TokenType("|", r"\|", None),
    TokenType("|=", r"\|=", None),
    TokenType("||", r"\|\|", None),
    TokenType("}", r"\}", None),
    TokenType("char", r"
        '
        (\x20 | ( \\' | [^'[:space:]] )*)
        '
    ", None),
    TokenType("byte", r"
        b'
        (\x20 | ( \\' | [^'[:space:]] )*)
        '
    ", None),
    TokenType("string", r#"
        "
        ( \\(.|\n) | [^"\\] )*
        "
    "#, None),
    TokenType("raw_string", r##"r\x23*""##, Some(&(raw_string_rule as fn(&str) -> Option<usize>))),
    TokenType("byte_string", r#"
        b"
        ( \\(.|\n) | [^"\\] )*
        "
    "#, None),
    TokenType("raw_byte_string", r##"br\x23*""##, Some(&(raw_string_rule as fn(&str) -> Option<usize>))),
    TokenType("integer", r"\d(\d|_)* (\p{XID_Start}\p{XID_Continue}*)?", None),
    TokenType("float", r"
         \d+ ( \.\d+([eE][+-]?\d+)?
             |      ([eE][+-]?\d+) )

        (\p{XID_Start}\p{XID_Continue}*)?
    ", None),
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
    let mut length = if input.starts_with("r") {
        1
    } else {
        assert!(input.starts_with("br"));
        2
    };

    input = &input[length..];
    let n_hashes = count_leading_hashes(input);
    length += n_hashes;
    input = &input[n_hashes..];

    assert!(input.starts_with('"'));
    length += 1;
    input = &input[1..];

    while let Some(c) = input.chars().next() {
        input = &input[c.len_utf8()..];
        length += c.len_utf8();
        if c == '"' && n_hashes <= count_leading_hashes(input) {
            return Some(length + n_hashes);
        }
    }

    return None;

    fn count_leading_hashes(s: &str) -> usize {
        s.chars().take_while(|&c| c == '#').count()
    }
}

// Command line interface
pub fn driver<F: Fn(&str) -> Option<Vec<(&'static str, usize)>>>(f: F) {
    use std::io::{self, Read};

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).expect("Failed to read stdin");
    let tokens = f(&buffer).expect("Invalid rust file");

    let mut offset = 0;
    for &(name, len) in tokens.iter() {
        println!("{} {} {:?}", name, len, &buffer[offset..offset + len]);
        offset += len;
    }
}

// Tests
pub fn check<F: Fn(&str) -> Option<Vec<(&'static str, usize)>>>(f: F) {
    let text = r###"
// identifiers
abstract	alignof	as	become	box
break	const	continue	crate	do
else	enum	extern	false	final
fn	for	if	impl	in
let	loop	macro	match	mod
move	mut	offsetof	override	priv
proc	pub	pure	ref	return
Self	self	sizeof	static	struct
super	trait	true	type	typeof
unsafe	unsized	use	virtual	where
while	yield

self super Self

abstractx	alignofx	axs	becomex	boxx
break92	const92	continue92	crate92	do92
else92	enum92	extern92	false92	final92
fn92	for92	if92	impl92	in92
let92	loop92	macro92	match92	mod92
move__	mut__	offsetof__	override__	priv__
proc__	pub__	pure__	ref__	return__
Self__	self__	sizeof__	static__	struct__
super__	trait__	true__	type__	typeof__
unsafe__	unsized__	use__	virtual__	where__
while__	yield__

_ __ ___ _привет_мир_

// comments

//comment
// comment
//
// comment // comment
/* comment */ non_comment

/* nested
   /* multi line */
comment */

/* nested // line comment */

/// line doc comment
/// another line
//! inner
/** block /* outter*/ */
/*! block /* inner */ */

// character, string and byte literals

'x' 'lifetime
'\'' ' ' '\t' '\\' '\x20' '\u{007D}' '\n' '\r' '\t' '\0' '\u{a}' '\u{000aAa}'

"" "x" " " "	" "hello" "\"world\""
"\"" "\\" "\x20" "\u{007D}" "\n" "\r" "\t" "\0" "\u{A}" "\u{0a0abc}"

"multi
line"

" with \
 escape"

b'x' b' ' b'\t' b'\0' b'\x7F' b'\\' b'\''

b"x" b" " b"\0" b"\x7F" b"\\" b"Hello WOrld" b"\"\"\""
br##"hello "# World!"##

// numbers

0 1 1_000_000_000 1i32 1__0i64 _1 1_

// operators

+ - * / % ^ & | ! || && << >>
+= -= *= /= %= ^= &= |= &= <<= >>=
== != < > <= >=

// symbols
@ $ . .. ... [ ] ( ) { } :: : ; ,
"###;

    let expected = tokenize(text).expect("Failed to parse canonically");
    let actual = f(text).expect(&format!(
        "Failed to parse `{}`", text
    ));

    let mut o = 0;
    for (&(ename, elen), &(aname, alen)) in expected.iter().zip(actual.iter()) {
        if ename != aname || elen != alen {
            panic!("\nExpected: {} {:?}\n\
                      Actual  : {} {:?}\n",
                    ename, &text[o..o+elen],
                    aname, &text[o..o+alen]);
        }
        o += elen;
    }
    assert_eq!(actual.len(), expected.len());
}
