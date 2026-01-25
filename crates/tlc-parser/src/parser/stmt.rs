use crate::{
    ast::{
        Ident,
        stmt::{Bind, BindKind, ForInit, ForInitKind, Stmt, StmtKind},
    },
    error::Error,
    parser::Parser,
};
use tlc_lexer::{span::Span, token::TokenKind};

impl<'src> Parser<'src> {
    pub(crate) fn stmt(&mut self) -> Result<Stmt<'src>, Error> {
        let token = self.advance_must()?;
        match token.kind {
            TokenKind::Var => self.var_let(token.span, true),
            TokenKind::Let => self.var_let(token.span, false),
            TokenKind::If => self.r#if(token.span),
            TokenKind::For => self.r#for(token.span),
            TokenKind::While => self.r#while(token.span),
            TokenKind::Loop => self.r#loop(token.span),
            TokenKind::Break => self.r#break(token.span),
            TokenKind::Continue => self.r#continue(token.span),
            TokenKind::Return => self.r#return(token.span),
            TokenKind::LCurly => self.block(token.span),
            TokenKind::Defer => self.defer(token.span),
            TokenKind::Ident(s) => {
                if self.consume(|token| matches!(token.kind, TokenKind::Colon)) {
                    let label = Ident {
                        name: s,
                        span: token.span,
                    };
                    let stmt = Box::new(self.stmt()?);
                    Ok(Stmt {
                        span: label.span.to(stmt.span),
                        kind: StmtKind::Labeled { label, stmt },
                    })
                } else {
                    self.retreat();
                    let expr = self.expr()?;
                    let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;
                    Ok(Stmt {
                        span: expr.span.to(end),
                        kind: StmtKind::Expr { expr },
                    })
                }
            }
            _ => {
                self.retreat();
                let expr = self.expr()?;
                let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;
                Ok(Stmt {
                    span: expr.span.to(end),
                    kind: StmtKind::Expr { expr },
                })
            }
        }
    }
}

impl<'src> Parser<'src> {
    fn var_let(&mut self, start: Span, mutable: bool) -> Result<Stmt<'src>, Error> {
        let bind = self.bind()?;
        let ty = if self.consume(|token| matches!(token.kind, TokenKind::Colon)) {
            Some(self.ty()?)
        } else {
            None
        };
        let value = if self.consume(|token| matches!(token.kind, TokenKind::Assign)) {
            Some(self.expr()?)
        } else {
            None
        };
        let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;

        Ok(Stmt {
            span: start.to(end),
            kind: StmtKind::VarLet {
                mutable,
                bind,
                ty,
                value,
            },
        })
    }

    fn bind(&mut self) -> Result<Bind<'src>, Error> {
        if let Some(start) = self.consume_span(|token| matches!(token.kind, TokenKind::LParen)) {
            let mut members = Vec::new();
            let end = loop {
                if let Some(span) =
                    self.consume_span(|token| matches!(token.kind, TokenKind::RParen))
                {
                    break span;
                }
                members.push(self.bind()?);
                if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                    continue;
                } else {
                    break self.expect_span(|token| matches!(token.kind, TokenKind::RParen))?;
                }
            };
            Ok(Bind {
                span: start.to(end),
                kind: BindKind::Tuple { members },
            })
        } else if let Some(start) =
            self.consume_span(|token| matches!(token.kind, TokenKind::LSquare))
        {
            let mut members = Vec::new();
            let end = loop {
                if let Some(span) =
                    self.consume_span(|token| matches!(token.kind, TokenKind::RSquare))
                {
                    break span;
                }
                members.push(self.bind()?);
                if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                    continue;
                } else {
                    break self.expect_span(|token| matches!(token.kind, TokenKind::RSquare))?;
                }
            };
            Ok(Bind {
                span: start.to(end),
                kind: BindKind::Array { members },
            })
        } else {
            let name = self.ident()?;
            Ok(Bind {
                span: name.span,
                kind: BindKind::Ident { name },
            })
        }
    }

    fn r#if(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        self.expect(|token| matches!(token.kind, TokenKind::LParen))?;
        let cond = self.expr()?;
        self.expect(|token| matches!(token.kind, TokenKind::RParen))?;
        let body = Box::new(self.stmt()?);
        let (r#else, end) = if self.consume(|token| matches!(token.kind, TokenKind::Else)) {
            let stmt = self.stmt()?;
            let end = stmt.span;
            (Some(Box::new(self.stmt()?)), end)
        } else {
            (None, body.span)
        };

        Ok(Stmt {
            span: start.to(end),
            kind: StmtKind::If { cond, body, r#else },
        })
    }

    fn r#for(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        self.expect(|token| matches!(token.kind, TokenKind::LParen))?;
        let init = if self.consume(|token| matches!(token.kind, TokenKind::Semi)) {
            None
        } else {
            let init = self.for_init()?;
            self.expect(|token| matches!(token.kind, TokenKind::Semi))?;
            Some(init)
        };
        let cond = if self.consume(|token| matches!(token.kind, TokenKind::Semi)) {
            None
        } else {
            let cond = self.expr()?;
            self.expect(|token| matches!(token.kind, TokenKind::Semi))?;
            Some(cond)
        };
        let update = if self.consume(|token| matches!(token.kind, TokenKind::RParen)) {
            None
        } else {
            let update = self.expr()?;
            self.expect(|token| matches!(token.kind, TokenKind::RParen))?;
            Some(update)
        };
        let body = Box::new(self.stmt()?);

        Ok(Stmt {
            span: start.to(body.span),
            kind: StmtKind::For {
                init,
                cond,
                update,
                body,
            },
        })
    }

    fn for_init(&mut self) -> Result<ForInit<'src>, Error> {
        if let Some(start) = self.consume_span(|token| matches!(token.kind, TokenKind::Var)) {
            let name = self.ident()?;
            let (ty, end) = if self.consume(|token| matches!(token.kind, TokenKind::Colon)) {
                let ty = self.ty()?;
                let end = ty.span;
                (Some(self.ty()?), end)
            } else {
                (None, name.span)
            };
            let (value, end) = if self.consume(|token| matches!(token.kind, TokenKind::Assign)) {
                let expr = self.expr()?;
                let end = expr.span;
                (Some(expr), end)
            } else {
                (None, end)
            };

            Ok(ForInit {
                span: start.to(end),
                kind: ForInitKind::VarDecl { name, ty, value },
            })
        } else {
            let expr = self.expr()?;
            Ok(ForInit {
                span: expr.span,
                kind: ForInitKind::Expr { expr },
            })
        }
    }

    fn r#while(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        self.expect(|token| matches!(token.kind, TokenKind::LParen))?;
        let cond = self.expr()?;
        self.expect(|token| matches!(token.kind, TokenKind::RParen))?;
        let body = Box::new(self.stmt()?);

        Ok(Stmt {
            span: start.to(body.span),
            kind: StmtKind::While { cond, body },
        })
    }

    fn r#loop(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        let body = Box::new(self.stmt()?);

        Ok(Stmt {
            span: start.to(body.span),
            kind: StmtKind::Loop { body },
        })
    }

    fn r#break(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        let (label, end) =
            if let Some(span) = self.consume_span(|token| matches!(token.kind, TokenKind::Semi)) {
                (None, span)
            } else {
                let label = Some(self.ident()?);
                let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;
                (label, end)
            };

        Ok(Stmt {
            span: start.to(end),
            kind: StmtKind::Break { label },
        })
    }

    fn r#continue(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        let (label, end) =
            if let Some(span) = self.consume_span(|token| matches!(token.kind, TokenKind::Semi)) {
                (None, span)
            } else {
                let label = Some(self.ident()?);
                let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;
                (label, end)
            };

        Ok(Stmt {
            span: start.to(end),
            kind: StmtKind::Continue { label },
        })
    }

    fn r#return(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        let (value, end) =
            if let Some(span) = self.consume_span(|token| matches!(token.kind, TokenKind::Semi)) {
                (None, span)
            } else {
                let value = self.expr()?;
                let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;
                (Some(value), end)
            };

        Ok(Stmt {
            span: start.to(end),
            kind: StmtKind::Return { value },
        })
    }

    fn block(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        let mut stmts = Vec::new();
        loop {
            if let Some(end) = self.consume_span(|token| matches!(token.kind, TokenKind::RCurly)) {
                return Ok(Stmt {
                    span: start.to(end),
                    kind: StmtKind::Block { stmts },
                });
            }
            stmts.push(self.stmt()?);
        }
    }

    fn defer(&mut self, start: Span) -> Result<Stmt<'src>, Error> {
        let stmt = Box::new(self.stmt()?);
        Ok(Stmt {
            span: start.to(stmt.span),
            kind: StmtKind::Defer { stmt },
        })
    }
}
