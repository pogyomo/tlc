use crate::{
    ast::{
        Ident,
        ty::{Ty, TyKind},
    },
    error::Error,
    parser::Parser,
};
use tlc_lexer::token::TokenKind;

impl<'src> Parser<'src> {
    pub(crate) fn ty(&mut self) -> Result<Ty<'src>, Error> {
        let token = self.advance_must()?;
        match token.kind {
            TokenKind::Ident(s) => {
                let mut path = vec![Ident {
                    name: s,
                    span: token.span,
                }];
                loop {
                    if self.consume(|token| matches!(token.kind, TokenKind::Dot)) {
                        path.push(self.ident()?);
                    } else {
                        break;
                    }
                }
                Ok(Ty {
                    span: token.span.to(path.last().unwrap().span),
                    kind: TyKind::Qualified { path },
                })
            }
            TokenKind::Star => {
                let mutable = !self.consume(|token| matches!(token.kind, TokenKind::Const));
                let of = self.ty()?;
                Ok(Ty {
                    span: token.span.to(of.span),
                    kind: TyKind::Pointer {
                        mutable,
                        of: Box::new(of),
                    },
                })
            }
            TokenKind::LSquare => {
                let size = if self.consume(|token| matches!(token.kind, TokenKind::RSquare)) {
                    None
                } else {
                    let size = self.expr()?;
                    self.expect(|token| matches!(token.kind, TokenKind::RSquare))?;
                    Some(size)
                };
                let mutable = !self.consume(|token| matches!(token.kind, TokenKind::Const));
                let of = self.ty()?;
                Ok(Ty {
                    span: token.span.to(of.span),
                    kind: TyKind::Array {
                        mutable,
                        of: Box::new(of),
                        size,
                    },
                })
            }
            TokenKind::LParen => {
                let mut members = Vec::new();
                let end = loop {
                    if let Some(span) =
                        self.consume_span(|token| matches!(token.kind, TokenKind::RParen))
                    {
                        break span;
                    }
                    members.push(self.ty()?);
                    if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                        continue;
                    } else {
                        break self.expect_span(|token| matches!(token.kind, TokenKind::RParen))?;
                    }
                };
                Ok(Ty {
                    span: token.span.to(end),
                    kind: TyKind::Tuple { members },
                })
            }
            TokenKind::Func => {
                self.expect(|token| matches!(token.kind, TokenKind::LParen))?;

                let mut params = Vec::new();
                let end = loop {
                    if let Some(span) =
                        self.consume_span(|token| matches!(token.kind, TokenKind::RParen))
                    {
                        break span;
                    }
                    params.push(self.ty()?);
                    if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                        continue;
                    } else {
                        break self.expect_span(|token| matches!(token.kind, TokenKind::RParen))?;
                    }
                };

                let (ret_ty, end) = if self.consume(|token| matches!(token.kind, TokenKind::Arrow))
                {
                    let ty = self.ty()?;
                    let end = ty.span;
                    (Some(Box::new(ty)), end)
                } else {
                    (None, end)
                };

                Ok(Ty {
                    span: token.span.to(end),
                    kind: TyKind::Func { params, ret_ty },
                })
            }
            TokenKind::U8 => Ok(Ty {
                span: token.span,
                kind: TyKind::U8,
            }),
            TokenKind::U16 => Ok(Ty {
                span: token.span,
                kind: TyKind::U16,
            }),
            TokenKind::U32 => Ok(Ty {
                span: token.span,
                kind: TyKind::U32,
            }),
            TokenKind::U64 => Ok(Ty {
                span: token.span,
                kind: TyKind::U64,
            }),
            TokenKind::USize => Ok(Ty {
                span: token.span,
                kind: TyKind::USize,
            }),
            TokenKind::I8 => Ok(Ty {
                span: token.span,
                kind: TyKind::I8,
            }),
            TokenKind::I16 => Ok(Ty {
                span: token.span,
                kind: TyKind::I16,
            }),
            TokenKind::I32 => Ok(Ty {
                span: token.span,
                kind: TyKind::I32,
            }),
            TokenKind::I64 => Ok(Ty {
                span: token.span,
                kind: TyKind::I64,
            }),
            TokenKind::ISize => Ok(Ty {
                span: token.span,
                kind: TyKind::ISize,
            }),
            TokenKind::F32 => Ok(Ty {
                span: token.span,
                kind: TyKind::F32,
            }),
            TokenKind::F64 => Ok(Ty {
                span: token.span,
                kind: TyKind::F64,
            }),
            TokenKind::Bool => Ok(Ty {
                span: token.span,
                kind: TyKind::Bool,
            }),
            TokenKind::Char => Ok(Ty {
                span: token.span,
                kind: TyKind::Char,
            }),
            _ => Err(Error::unexpected_token(token.span)),
        }
    }
}
