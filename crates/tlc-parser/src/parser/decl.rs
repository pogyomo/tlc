use crate::{
    ast::decl::{
        CompoundField, CompoundKind, Decl, DeclKind, EnumField, FuncParam, ProcBody, TestDesc,
    },
    error::Error,
    parser::Parser,
};
use tlc_lexer::{
    span::Span,
    token::{Token, TokenKind},
};

impl<'src> Parser<'src> {
    pub(crate) fn decl(&mut self) -> Result<Decl<'src>, Error> {
        let token = self.advance_must()?;
        match token.kind {
            TokenKind::Import => self.import(token.span),
            TokenKind::Module => self.module(token.span),
            TokenKind::Test => self.decl_test(token.span),
            TokenKind::Pub => {
                let start = token.span;
                let token = self.advance_must()?;
                self.decl_publicable(start, token, true)
            }
            _ => self.decl_publicable(token.span, token, false),
        }
    }

    fn decl_publicable(
        &mut self,
        start: Span,
        token: Token<'src>,
        public: bool,
    ) -> Result<Decl<'src>, Error> {
        match token.kind {
            TokenKind::Struct => self.compound(start, CompoundKind::Struct, public),
            TokenKind::Union => self.compound(start, CompoundKind::Union, public),
            TokenKind::Enum => self.r#enum(start, public),
            TokenKind::Func => self.decl_func(start, public),
            _ => Err(Error::unexpected_token(token.span)),
        }
    }
}

impl<'src> Parser<'src> {
    fn import(&mut self, start: Span) -> Result<Decl<'src>, Error> {
        let mut path = vec![self.ident()?];
        loop {
            if self.consume(|token| matches!(token.kind, TokenKind::Dot)) {
                path.push(self.ident()?);
            } else {
                break;
            }
        }
        let alias = if self.consume(|token| matches!(token.kind, TokenKind::As)) {
            Some(self.ident()?)
        } else {
            None
        };
        let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;

        Ok(Decl {
            span: start.to(end),
            kind: DeclKind::Import { path, alias },
        })
    }

    fn module(&mut self, start: Span) -> Result<Decl<'src>, Error> {
        let mut path = vec![self.ident()?];
        loop {
            if self.consume(|token| matches!(token.kind, TokenKind::Dot)) {
                path.push(self.ident()?);
            } else {
                break;
            }
        }
        let end = self.expect_span(|token| matches!(token.kind, TokenKind::Semi))?;

        Ok(Decl {
            span: start.to(end),
            kind: DeclKind::Module { path },
        })
    }

    fn compound(
        &mut self,
        start: Span,
        kind: CompoundKind,
        public: bool,
    ) -> Result<Decl<'src>, Error> {
        let name = self.ident()?;

        self.expect(|token| matches!(token.kind, TokenKind::LCurly))?;

        let mut fields = Vec::new();
        let end = loop {
            if let Some(span) = self.consume_span(|token| matches!(token.kind, TokenKind::RCurly)) {
                break span;
            }

            fields.push(self.compound_field()?);
            if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                continue;
            } else {
                break self.expect_span(|token| matches!(token.kind, TokenKind::RCurly))?;
            }
        };

        Ok(Decl {
            span: start.to(end),
            kind: DeclKind::Compound {
                public,
                kind,
                name,
                fields,
            },
        })
    }

    fn compound_field(&mut self) -> Result<CompoundField<'src>, Error> {
        let name = self.ident()?;
        self.expect(|token| matches!(token.kind, TokenKind::Colon))?;
        let ty = self.ty()?;
        Ok(CompoundField {
            span: name.span.to(ty.span),
            name,
            ty,
        })
    }

    fn r#enum(&mut self, start: Span, public: bool) -> Result<Decl<'src>, Error> {
        let name = self.ident()?;

        let base_ty = if self.consume(|token| matches!(token.kind, TokenKind::Colon)) {
            Some(self.ty()?)
        } else {
            None
        };

        self.expect(|token| matches!(token.kind, TokenKind::LCurly))?;

        let mut fields = Vec::new();
        let end = loop {
            if let Some(span) = self.consume_span(|token| matches!(token.kind, TokenKind::RCurly)) {
                break span;
            }

            fields.push(self.enum_field()?);
            if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                continue;
            } else {
                break self.expect_span(|token| matches!(token.kind, TokenKind::RCurly))?;
            }
        };

        Ok(Decl {
            span: start.to(end),
            kind: DeclKind::Enum {
                public,
                name,
                base_ty,
                fields,
            },
        })
    }

    fn enum_field(&mut self) -> Result<EnumField<'src>, Error> {
        let name = self.ident()?;
        if self.consume(|token| matches!(token.kind, TokenKind::Assign)) {
            let value = self.expr()?;
            Ok(EnumField {
                span: name.span.to(value.span),
                name,
                value: Some(value),
            })
        } else {
            Ok(EnumField {
                span: name.span,
                name,
                value: None,
            })
        }
    }

    fn decl_func(&mut self, start: Span, public: bool) -> Result<Decl<'src>, Error> {
        let name = self.ident()?;
        self.expect(|token| matches!(token.kind, TokenKind::LParen))?;

        let mut params = Vec::new();
        loop {
            if self.consume(|token| matches!(token.kind, TokenKind::RParen)) {
                break;
            }

            params.push(self.func_param()?);
            if self.consume(|token| matches!(token.kind, TokenKind::Comma)) {
                continue;
            } else {
                self.expect(|token| matches!(token.kind, TokenKind::RParen))?;
                break;
            }
        }

        let ret_ty = if self.consume(|token| matches!(token.kind, TokenKind::Arrow)) {
            Some(self.ty()?)
        } else {
            None
        };

        let body = self.proc_body()?;

        Ok(Decl {
            span: start.to(body.span),
            kind: DeclKind::Func {
                public,
                name,
                params,
                ret_ty,
                body,
            },
        })
    }

    fn func_param(&mut self) -> Result<FuncParam<'src>, Error> {
        let name = self.ident()?;
        self.expect(|token| matches!(token.kind, TokenKind::Colon))?;
        let ty = self.ty()?;
        Ok(FuncParam {
            span: name.span.to(ty.span),
            name,
            ty,
        })
    }

    fn decl_test(&mut self, start: Span) -> Result<Decl<'src>, Error> {
        let desc = self.expect_with(|token| match token.kind {
            TokenKind::String(value) => Some(TestDesc {
                value,
                span: token.span,
            }),
            _ => None,
        })?;

        let body = self.proc_body()?;

        Ok(Decl {
            span: start.to(body.span),
            kind: DeclKind::Test { desc, body },
        })
    }

    fn proc_body(&mut self) -> Result<ProcBody<'src>, Error> {
        let start = self.expect_span(|token| matches!(token.kind, TokenKind::LCurly))?;

        let mut stmts = Vec::new();
        let end = loop {
            if let Some(span) = self.consume_span(|token| matches!(token.kind, TokenKind::RCurly)) {
                break span;
            }

            stmts.push(self.stmt()?);
        };

        Ok(ProcBody {
            stmts,
            span: start.to(end),
        })
    }
}
