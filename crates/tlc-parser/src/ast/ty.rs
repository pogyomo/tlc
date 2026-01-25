use crate::ast::{Ident, expr::Expr};
use tlc_lexer::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Ty<'src> {
    pub span: Span,
    pub kind: TyKind<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind<'src> {
    Qualified {
        path: Vec<Ident<'src>>,
    },
    Pointer {
        mutable: bool,
        of: Box<Ty<'src>>,
    },
    Array {
        mutable: bool,
        of: Box<Ty<'src>>,
        size: Option<Expr<'src>>,
    },
    Tuple {
        members: Vec<Ty<'src>>,
    },
    Func {
        params: Vec<Ty<'src>>,
        ret_ty: Option<Box<Ty<'src>>>,
    },
    U8,
    U16,
    U32,
    U64,
    USize,
    I8,
    I16,
    I32,
    I64,
    ISize,
    F32,
    F64,
    Bool,
    Char,
}
