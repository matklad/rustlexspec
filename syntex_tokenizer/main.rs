extern crate syntex_syntax;
extern crate rustlexspec;


use syntex_syntax::parse::{lexer, ParseSess};
use syntex_syntax::parse::lexer::{Reader, TokenAndSpan};
use rustlexspec::driver;

fn tokenize_rustc(input: &str) -> Option<Vec<rustlexspec::Token>> {

    let sess = ParseSess::new();
    let file_map = sess.codemap().new_filemap("dummy.rs".to_string(), None, input.to_string());
    let handler = sess.span_diagnostic;

    let mut result = Vec::new();
    let mut lexer = lexer::StringReader::new(&handler, file_map);

    while !lexer.is_eof() {
        result.push(map_token(lexer.next_token()));
    }
    result.push(rustlexspec::Token {
        token_type: by_name("whitespace"),
        span: (0, 1),
    });

    return Some(result);

    fn by_name(name: &str) -> rustlexspec::TokenType {
        *rustlexspec::TOKEN_TYPES.iter().find(|t| t.name() == name).unwrap()
    }

    fn map_token(t: TokenAndSpan) -> rustlexspec::Token {
        use syntex_syntax::parse::token::Token::*;
        use syntex_syntax::parse::token::DelimToken::*;
        use syntex_syntax::parse::token::BinOpToken::*;
        let token_name = match t.tok {
            Pound => "#",
            Ident(..) => "identifier",
            Lifetime(..) => "lifetime",
            Whitespace => "whitespace",
            Semi => ";",
            Colon => ":",
            Comma => ",",
            Comment => "line_comment",
            Lt => "<",
            Gt => ">",
            OpenDelim(Paren) => "(",
            CloseDelim(Paren) => ")",
            OpenDelim(Bracket) => "[",
            CloseDelim(Bracket) => "]",
            OpenDelim(Brace) => "{",
            CloseDelim(Brace) => "}",
            BinOp(And) => "&&",
            BinOp(Or) => "||",
            ModSep => "::",
            _ => panic!("Unhandled token {:?}", t.tok)
        };
        rustlexspec::Token {
            token_type: by_name(token_name),
            span: (t.sp.lo.0 as usize, t.sp.hi.0 as usize),
        }
    }
}


fn main() {
    driver(tokenize_rustc);
}
