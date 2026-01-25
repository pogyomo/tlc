use crate::ast::{Ident, expr::Expr, stmt::Stmt, ty::Ty};
use tlc_lexer::{span::Span, token::RawString};

#[derive(Debug, Clone, PartialEq)]
pub struct Decl<'src> {
    pub span: Span,
    pub kind: DeclKind<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind<'src> {
    Import {
        path: Vec<Ident<'src>>,
        alias: Option<Ident<'src>>,
    },
    Module {
        path: Vec<Ident<'src>>,
    },
    Compound {
        public: bool,
        kind: CompoundKind,
        name: Ident<'src>,
        fields: Vec<CompoundField<'src>>,
    },
    Enum {
        public: bool,
        name: Ident<'src>,
        base_ty: Option<Ty<'src>>,
        fields: Vec<EnumField<'src>>,
    },
    Func {
        public: bool,
        name: Ident<'src>,
        params: Vec<FuncParam<'src>>,
        ret_ty: Option<Ty<'src>>,
        body: ProcBody<'src>,
    },
    Test {
        desc: TestDesc<'src>,
        body: ProcBody<'src>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompoundKind {
    Struct,
    Union,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundField<'src> {
    pub name: Ident<'src>,
    pub ty: Ty<'src>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumField<'src> {
    pub name: Ident<'src>,
    pub value: Option<Expr<'src>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam<'src> {
    pub name: Ident<'src>,
    pub ty: Ty<'src>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestDesc<'src> {
    pub value: RawString<'src>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcBody<'src> {
    pub stmts: Vec<Stmt<'src>>,
    pub span: Span,
}
