mod decl;
mod expr;
mod stmt;
mod ty;

use crate::{
    ast::{Ident, SourceTree},
    error::Error,
    stream::TokenStream,
};
use tlc_lexer::{
    Lexer,
    span::Span,
    token::{Token, TokenKind},
};

pub struct Parser<'src> {
    stream: TokenStream<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Option<Self> {
        let stream = TokenStream::new(lexer)?;
        Some(Self { stream })
    }

    pub fn parse(mut self) -> Result<SourceTree<'src>, Error> {
        let mut decls = Vec::new();
        while self.stream.peek(0).is_some() {
            decls.push(self.decl()?);
        }
        Ok(SourceTree { decls })
    }
}

impl<'src> Parser<'src> {
    pub(crate) fn ident(&mut self) -> Result<Ident<'src>, Error> {
        self.expect_with(|token| {
            if let TokenKind::Ident(name) = token.kind {
                Some(Ident {
                    name,
                    span: token.span,
                })
            } else {
                None
            }
        })
    }
}

impl<'src> Parser<'src> {
    pub(crate) fn advance_must(&mut self) -> Result<Token<'src>, Error> {
        self.stream
            .advance()
            .ok_or(Error::token_expected(self.stream.last().span))
    }

    #[inline]
    pub(crate) fn retreat(&mut self) {
        self.stream.retreat();
    }

    /// Try parse something, recover state if failed.
    pub(crate) fn try_parse<F: FnOnce(&mut Parser<'src>) -> Result<Option<T>, Error>, T>(
        &mut self,
        action: F,
    ) -> Result<Option<T>, Error> {
        let checkpoint = self.stream.save();
        let value = action(self)?;
        if value.is_none() {
            self.stream.load(checkpoint);
        }
        Ok(value)
    }

    /// Expect and consume token where cond returns true.
    pub(crate) fn expect<P: FnOnce(Token<'src>) -> bool>(&mut self, cond: P) -> Result<(), Error> {
        self.expect_with(|token| if cond(token) { Some(()) } else { None })
    }

    /// Expect and consume token and return its span if cond returns true
    pub(crate) fn expect_span<P: FnOnce(Token<'src>) -> bool>(
        &mut self,
        cond: P,
    ) -> Result<Span, Error> {
        self.expect_with(|token| if cond(token) { Some(token.span) } else { None })
    }

    /// Expect and consume token where cond returns some.
    pub(crate) fn expect_with<P: FnOnce(Token<'src>) -> Option<T>, T>(
        &mut self,
        cond: P,
    ) -> Result<T, Error> {
        match self.stream.peek(0) {
            Some(token) => match cond(token) {
                Some(value) => {
                    self.stream.advance();
                    Ok(value)
                }
                None => Err(Error::unexpected_token(token.span)),
            },
            None => Err(Error::token_expected(self.stream.last().span)),
        }
    }

    /// Consume token if cond returns true.
    pub(crate) fn consume<P: FnOnce(Token<'src>) -> bool>(&mut self, cond: P) -> bool {
        self.consume_with(|token| if cond(token) { Some(()) } else { None })
            .is_some()
    }

    /// Consume token and return its span if cond returns true
    pub(crate) fn consume_span<P: FnOnce(Token<'src>) -> bool>(&mut self, cond: P) -> Option<Span> {
        self.consume_with(|token| if cond(token) { Some(token.span) } else { None })
    }

    /// Consume token if cond returns some.
    pub(crate) fn consume_with<P: FnOnce(Token<'src>) -> Option<T>, T>(
        &mut self,
        cond: P,
    ) -> Option<T> {
        let token = self.stream.peek(0)?;
        let value = cond(token)?;

        self.stream.advance();
        Some(value)
    }
}
