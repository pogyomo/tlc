use crate::ast::{Ident, expr::Expr, ty::Ty};
use tlc_lexer::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt<'src> {
    pub span: Span,
    pub kind: StmtKind<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind<'src> {
    VarLet {
        mutable: bool,
        bind: Bind<'src>,
        ty: Option<Ty<'src>>,
        value: Option<Expr<'src>>,
    },
    If {
        cond: Expr<'src>,
        body: Box<Stmt<'src>>,
        r#else: Option<Box<Stmt<'src>>>,
    },
    For {
        init: Option<ForInit<'src>>,
        cond: Option<Expr<'src>>,
        update: Option<Expr<'src>>,
        body: Box<Stmt<'src>>,
    },
    While {
        cond: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
    Loop {
        body: Box<Stmt<'src>>,
    },
    Break {
        label: Option<Ident<'src>>,
    },
    Continue {
        label: Option<Ident<'src>>,
    },
    Return {
        value: Option<Expr<'src>>,
    },
    Block {
        stmts: Vec<Stmt<'src>>,
    },
    Defer {
        stmt: Box<Stmt<'src>>,
    },
    Expr {
        expr: Expr<'src>,
    },
    Labeled {
        label: Ident<'src>,
        stmt: Box<Stmt<'src>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Bind<'src> {
    pub span: Span,
    pub kind: BindKind<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BindKind<'src> {
    Tuple { members: Vec<Bind<'src>> },
    Array { members: Vec<Bind<'src>> },
    Ident { name: Ident<'src> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForInit<'src> {
    pub span: Span,
    pub kind: ForInitKind<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInitKind<'src> {
    VarDecl {
        name: Ident<'src>,
        ty: Option<Ty<'src>>,
        value: Option<Expr<'src>>,
    },
    Expr {
        expr: Expr<'src>,
    },
}
