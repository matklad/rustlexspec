extern crate syntex_syntax;
extern crate rustlexspec;


use syntex_syntax::parse::{lexer, ParseSess};
use syntex_syntax::parse::lexer::{Reader, TokenAndSpan};


pub fn tokenize(input: &str) -> Option<Vec<(&'static str, usize)>> {
    let sess = ParseSess::new();
    let file_map = sess.codemap().new_filemap("dummy.rs".to_string(), None, input.to_string());
    let handler = sess.span_diagnostic;

    let mut result = Vec::new();
    let mut lexer = lexer::StringReader::new(&handler, file_map);

    while !lexer.is_eof() {
        result.push(map_token(lexer.next_token()));
    }
    result.push(("whitespace", 1));

    return Some(result);

    fn map_token(t: TokenAndSpan) -> (&'static str, usize) {
        use syntex_syntax::parse::token::Token::*;
        use syntex_syntax::parse::token::DelimToken::*;
        let token_name = match t.tok {
            At => "@",
            Dollar => "$",
            AndAnd => "&&",
            BinOp(op) => {
                use syntex_syntax::parse::token::BinOpToken::*;
                match op {
                    And => "&",
                    Minus => "-",
                    Or => "|",
                    Plus => "+",
                    Shl => "<<",
                    Shr => ">>",
                    Star => "*",
                    Slash => "/",
                    Percent => "%",
                    Caret => "^",
                }
            }
            BinOpEq(op) => {
                use syntex_syntax::parse::token::BinOpToken::*;
                match op {
                    And => "&=",
                    Minus => "-=",
                    Or => "|=",
                    Plus => "+=",
                    Shl => "<<=",
                    Shr => ">>=",
                    Star => "*=",
                    Slash => "/=",
                    Percent => "%=",
                    Caret => "^=",
                }
            }
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
            Literal(lit, _) => {
                use syntex_syntax::parse::token::Lit::*;
                match lit {
                    Char(_) => "char",
                    Byte(_) => "byte",
                    ByteStr(_) => "byte_string",
                    ByteStrRaw(_, _) => "raw_byte_string",
                    Integer(_) => "integer",
                    StrRaw(_, _) => "raw_string",
                    Str_(_) => "string",
                    Float(_) => "float",
                }
            }
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
        (token_name, t.sp.hi.0 as usize - t.sp.lo.0 as usize)
    }
}


#[test]
fn check() {
    rustlexspec::check(tokenize)
}
