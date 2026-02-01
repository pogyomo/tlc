use crate::{
    ast::{
        Ident,
        expr::{
            CallArg, CompoundField, Expr, ExprKind, InfixOp, InfixOpKind, PostfixOp, PostfixOpKind,
            UnaryOp, UnaryOpKind,
        },
    },
    error::Error,
    parser::Parser,
};
use tlc_lexer::token::TokenKind;

impl<'src> Parser<'src> {
    pub(crate) fn expr(&mut self) -> Result<Expr<'src>, Error> {
        self.assign()
    }
}

impl<'src> Parser<'src> {
    fn assign(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.bool_or()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Assign => InfixOpKind::Assign,
                TokenKind::AddAssign => InfixOpKind::AddAssign,
                TokenKind::SubAssign => InfixOpKind::SubAssign,
                TokenKind::MulAssign => InfixOpKind::MulAssign,
                TokenKind::DivAssign => InfixOpKind::DivAssign,
                TokenKind::LShiftAssign => InfixOpKind::LShiftAssign,
                TokenKind::RShiftAssign => InfixOpKind::RShiftAssign,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.assign()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn bool_or(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.bool_and()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::AndAnd => InfixOpKind::BoolAnd,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.bool_or()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn bool_and(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.bit_or()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::VertVert => InfixOpKind::BoolOr,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.bool_and()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn bit_or(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.bit_xor()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Vert => InfixOpKind::BitOr,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.bit_or()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn bit_xor(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.bit_and()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Hat => InfixOpKind::BitXor,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.bit_xor()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn bit_and(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.equality()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::And => InfixOpKind::BitAnd,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.bit_and()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn equality(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.relational()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Eq => InfixOpKind::Eq,
                TokenKind::Ne => InfixOpKind::Ne,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.equality()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn relational(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.shift()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Lt => InfixOpKind::Lt,
                TokenKind::Le => InfixOpKind::Le,
                TokenKind::Gt => InfixOpKind::Gt,
                TokenKind::Ge => InfixOpKind::Ge,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.relational()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn shift(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.additive()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::LShift => InfixOpKind::LShift,
                TokenKind::RShift => InfixOpKind::RShift,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.shift()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn additive(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.multiplicative()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Plus => InfixOpKind::Add,
                TokenKind::Minus => InfixOpKind::Sub,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.additive()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn multiplicative(&mut self) -> Result<Expr<'src>, Error> {
        let mut lhs = self.unary()?;
        while let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Star => InfixOpKind::Mul,
                TokenKind::Slash => InfixOpKind::Div,
                TokenKind::Percent => InfixOpKind::Mod,
                _ => return None,
            };
            Some(InfixOp {
                kind,
                span: token.span,
            })
        }) {
            let rhs = self.multiplicative()?;
            lhs = Expr {
                span: lhs.span.to(rhs.span),
                kind: ExprKind::Infix {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expr<'src>, Error> {
        if let Some(op) = self.consume_with(|token| {
            let kind = match token.kind {
                TokenKind::Minus => UnaryOpKind::Neg,
                TokenKind::Inc => UnaryOpKind::Inc,
                TokenKind::Dec => UnaryOpKind::Dec,
                TokenKind::And => UnaryOpKind::Ref,
                TokenKind::Star => UnaryOpKind::Deref,
                TokenKind::Bang => UnaryOpKind::Inv,
                _ => return None,
            };
            Some(UnaryOp {
                kind,
                span: token.span,
            })
        }) {
            let expr = self.unary()?;
            Ok(Expr {
                span: op.span.to(expr.span),
                kind: ExprKind::Unary {
                    op,
                    expr: Box::new(expr),
                },
            })
        } else {
            self.postfix()
        }
    }

    fn postfix(&mut self) -> Result<Expr<'src>, Error> {
        let mut expr = self.primary()?;
        loop {
            if self.consume(|token| matches!(token.kind, TokenKind::LSquare)) {
                let index = self.expr()?;
                let end = self.expect_span(|token| matches!(token.kind, TokenKind::RSquare))?;
                expr = Expr {
                    span: expr.span.to(end),
                    kind: ExprKind::Index {
                        expr: Box::new(expr),
                        index: Box::new(index),
                    },
                };
            } else if self.consume(|token| matches!(token.kind, TokenKind::LParen)) {
                let mut args = Vec::new();
                let end = loop {
                    if let Some(end) =
                        self.consume_span(|token| matches!(token.kind, TokenKind::RParen))
                    {
                        break end;
                    }

                    if self.consume(|token| matches!(token.kind, TokenKind::Type)) {
                        args.push(CallArg::Ty(self.ty()?));
                    } else {
                        args.push(CallArg::Expr(self.expr()?));
                    }

                    if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                        continue;
                    } else {
                        break self.expect_span(|token| matches!(token.kind, TokenKind::RParen))?;
                    }
                };
                expr = Expr {
                    span: expr.span.to(end),
                    kind: ExprKind::Call {
                        func: Box::new(expr),
                        args,
                    },
                };
            } else if self.consume(|token| matches!(token.kind, TokenKind::Dot)) {
                let field = self.ident()?;
                expr = Expr {
                    span: expr.span.to(field.span),
                    kind: ExprKind::Access {
                        expr: Box::new(expr),
                        field,
                    },
                };
            } else if self.consume(|token| matches!(token.kind, TokenKind::As)) {
                let ty = self.ty()?;
                expr = Expr {
                    span: expr.span.to(ty.span),
                    kind: ExprKind::Cast {
                        expr: Box::new(expr),
                        to: Box::new(ty),
                    },
                };
            } else if let Some(op) = self.consume_with(|token| {
                let kind = match token.kind {
                    TokenKind::Inc => PostfixOpKind::Inc,
                    TokenKind::Dec => PostfixOpKind::Dec,
                    _ => return None,
                };
                Some(PostfixOp {
                    kind,
                    span: token.span,
                })
            }) {
                expr = Expr {
                    span: expr.span.to(op.span),
                    kind: ExprKind::Postfix {
                        op,
                        expr: Box::new(expr),
                    },
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr<'src>, Error> {
        let token = self.advance_must()?;
        match token.kind {
            TokenKind::Int(value) => Ok(Expr {
                span: token.span,
                kind: ExprKind::Int { value },
            }),
            TokenKind::Float(value) => Ok(Expr {
                span: token.span,
                kind: ExprKind::Float { value },
            }),
            TokenKind::String(value) => Ok(Expr {
                span: token.span,
                kind: ExprKind::String { value },
            }),
            TokenKind::Character(value) => Ok(Expr {
                span: token.span,
                kind: ExprKind::Character { value },
            }),
            TokenKind::True => Ok(Expr {
                span: token.span,
                kind: ExprKind::Bool { value: true },
            }),
            TokenKind::False => Ok(Expr {
                span: token.span,
                kind: ExprKind::Bool { value: false },
            }),
            TokenKind::Null => Ok(Expr {
                span: token.span,
                kind: ExprKind::Null,
            }),
            TokenKind::Ident(s) => {
                let result = self.try_parse(|parser| {
                    let mut path = vec![Ident {
                        name: s,
                        span: token.span,
                    }];
                    loop {
                        if parser.consume(|token| matches!(token.kind, TokenKind::Dot)) {
                            let Ok(ident) = parser.ident() else {
                                return Ok(None);
                            };
                            path.push(ident);
                        } else {
                            break;
                        }
                    }
                    if parser.consume(|token| matches!(token.kind, TokenKind::LCurly)) {
                        let mut fields = Vec::new();
                        let end = loop {
                            if let Some(span) =
                                parser.consume_span(|token| matches!(token.kind, TokenKind::RCurly))
                            {
                                break span;
                            }
                            fields.push(parser.expr_compound_field()?);
                            if parser.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                                continue;
                            } else {
                                break parser.expect_span(|token| {
                                    matches!(token.kind, TokenKind::RCurly)
                                })?;
                            }
                        };
                        Ok(Some(Expr {
                            span: token.span.to(end),
                            kind: ExprKind::Compound { path, fields },
                        }))
                    } else {
                        Ok(None)
                    }
                })?;
                if let Some(expr) = result {
                    Ok(expr)
                } else {
                    Ok(Expr {
                        span: token.span,
                        kind: ExprKind::Var {
                            name: Ident {
                                name: s,
                                span: token.span,
                            },
                        },
                    })
                }
            }
            TokenKind::LSquare => {
                let mut values = Vec::new();
                let end = loop {
                    if let Some(span) =
                        self.consume_span(|token| matches!(token.kind, TokenKind::RSquare))
                    {
                        break span;
                    }
                    values.push(self.expr()?);
                    if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                        continue;
                    } else {
                        break self
                            .expect_span(|token| matches!(token.kind, TokenKind::RSquare))?;
                    }
                };
                Ok(Expr {
                    span: token.span.to(end),
                    kind: ExprKind::Array { values },
                })
            }
            TokenKind::LParen => {
                if let Some(span) =
                    self.consume_span(|token| matches!(token.kind, TokenKind::RParen))
                {
                    Ok(Expr {
                        span: token.span.to(span),
                        kind: ExprKind::Tuple { values: Vec::new() },
                    })
                } else {
                    let mut expr = self.expr()?;
                    if let Some(end) =
                        self.consume_span(|token| matches!(token.kind, TokenKind::RParen))
                    {
                        expr.span = token.span.to(end);
                        Ok(expr)
                    } else {
                        self.expect(|token| matches!(token.kind, TokenKind::Comma))?;
                        let mut values = vec![expr];
                        let end = loop {
                            if let Some(span) =
                                self.consume_span(|token| matches!(token.kind, TokenKind::RParen))
                            {
                                break span;
                            }
                            values.push(self.expr()?);
                            if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                                continue;
                            } else {
                                break self.expect_span(|token| {
                                    matches!(token.kind, TokenKind::RParen)
                                })?;
                            }
                        };
                        Ok(Expr {
                            span: token.span.to(end),
                            kind: ExprKind::Tuple { values },
                        })
                    }
                }
            }
            _ => Err(Error::unexpected_token(token.span)),
        }
    }

    fn expr_compound_field(&mut self) -> Result<CompoundField<'src>, Error> {
        let name = self.ident()?;
        self.expect(|token| matches!(token.kind, TokenKind::Colon))?;
        let value = self.expr()?;
        Ok(CompoundField {
            span: name.span.to(value.span),
            name,
            value: Box::new(value),
        })
    }
}
