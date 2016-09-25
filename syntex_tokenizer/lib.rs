extern crate syntex_syntax;
extern crate rustlexspec;

use syntex_syntax::parse::{lexer, ParseSess};
use syntex_syntax::parse::lexer::{Reader, TokenAndSpan};

pub fn tokenize(input: &str) -> Option<Vec<rustlexspec::Token>> {

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
        len: 1,
    });

    return Some(result);

    fn by_name(name: &str) -> rustlexspec::TokenType {
        *rustlexspec::TOKEN_TYPES.iter().find(|t| t.name() == name)
            .expect(&format!("No `{}` token", name))
    }

    fn map_token(t: TokenAndSpan) -> rustlexspec::Token {
        use syntex_syntax::parse::token::Token::*;
        use syntex_syntax::parse::token::DelimToken::*;
        use syntex_syntax::parse::token::BinOpToken::*;
        use syntex_syntax::parse::token::Lit;
        let token_name = match t.tok {
            AndAnd => "&&",
            BinOp(And) => "&",
            BinOp(Minus) => "-",
            BinOp(Or) => "|",
            BinOp(Plus) => "+",
            BinOp(Shl) => "<<",
            BinOp(Shr) => ">>",
            BinOp(Star) => "*",
            BinOp(Slash) => "/",
            BinOp(Percent) => "%",
            BinOp(Caret) => "^",
            BinOpEq(Minus) => "-=",
            BinOpEq(Plus) => "+=",
            CloseDelim(Brace) => "}",
            CloseDelim(Bracket) => "]",
            CloseDelim(Paren) => ")",
            Colon => ":",
            Comma => ",",
            Comment => "comment",
            DocComment(..) => "doc_comment",
            Dot => ".",
            DotDot => "..",
            DotDotDot => "...",
            Eq => "=",
            EqEq => "==",
            FatArrow => "=>",
            Gt => ">",
            Ident(..) => "identifier",
            Le => "<=",
            Lifetime(..) => "lifetime",
            Literal(Lit::Char(_), _) => "char",
            Literal(Lit::Integer(_), _) => "integer",
            Literal(Lit::StrRaw(_, _), _) => "raw_string",
            Literal(Lit::Str_(_), _) => "string",
            Literal(Lit::Float(_), _) => "float",
            Lt => "<",
            Ge => ">=",
            ModSep => "::",
            Ne => "!=",
            Not => "!",
            OpenDelim(Brace) => "{",
            OpenDelim(Bracket) => "[",
            OpenDelim(Paren) => "(",
            OrOr => "||",
            Pound => "#",
            RArrow => "->",
            Semi => ";",
            Underscore => "_",
            Whitespace => "whitespace",
            _ => panic!("Unhandled token {:?}", t.tok)
        };
        rustlexspec::Token {
            token_type: by_name(token_name),
            len: t.sp.hi.0 as usize - t.sp.lo.0 as usize,
        }
    }
}

#[test]
fn check() {
    println!("Here");
    rustlexspec::check(tokenize)
}
