use std::fmt;
use tlc_lexer::span::Span;

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Error {
    #[inline]
    pub fn token_expected(span: Span) -> Self {
        Self {
            kind: ErrorKind::TokenExpected,
            span,
        }
    }

    #[inline]
    pub fn unexpected_token(span: Span) -> Self {
        Self {
            kind: ErrorKind::UnexpectedToken,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    TokenExpected,
    UnexpectedToken,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TokenExpected => f.write_str("token expected"),
            Self::UnexpectedToken => f.write_str("unexpected token"),
        }
    }
}
