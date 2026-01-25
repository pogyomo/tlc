pub mod span;
pub mod token;

use crate::token::{Token, TokenKind};

pub struct Lexer<'src> {
    lexer: logos::SpannedIter<'src, TokenKind<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            lexer: logos::Lexer::new(input).spanned(),
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = match self.lexer.next()? {
            (Ok(kind), range) => Token {
                kind,
                span: range.into(),
            },
            (Err(_), range) => Token {
                kind: TokenKind::Unknown,
                span: range.into(),
            },
        };
        Some(item)
    }
}
