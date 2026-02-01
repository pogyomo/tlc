use crate::ast::{Ident, ty::Ty};
use tlc_lexer::{
    span::Span,
    token::{RawCharacter, RawFloat, RawInt, RawString},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'src> {
    pub span: Span,
    pub kind: ExprKind<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'src> {
    Unary {
        op: UnaryOp,
        expr: Box<Expr<'src>>,
    },
    Infix {
        op: InfixOp,
        lhs: Box<Expr<'src>>,
        rhs: Box<Expr<'src>>,
    },
    Postfix {
        op: PostfixOp,
        expr: Box<Expr<'src>>,
    },
    Access {
        expr: Box<Expr<'src>>,
        field: Ident<'src>,
    },
    Cast {
        expr: Box<Expr<'src>>,
        to: Box<Ty<'src>>,
    },
    Index {
        expr: Box<Expr<'src>>,
        index: Box<Expr<'src>>,
    },
    Var {
        name: Ident<'src>,
    },
    Call {
        func: Box<Expr<'src>>,
        args: Vec<CallArg<'src>>,
    },
    Int {
        value: RawInt<'src>,
    },
    Float {
        value: RawFloat<'src>,
    },
    String {
        value: RawString<'src>,
    },
    Character {
        value: RawCharacter<'src>,
    },
    Bool {
        value: bool,
    },
    Null,
    Array {
        values: Vec<Expr<'src>>,
    },
    Tuple {
        values: Vec<Expr<'src>>,
    },
    Compound {
        path: Vec<Ident<'src>>,
        fields: Vec<CompoundField<'src>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallArg<'src> {
    Expr(Expr<'src>),
    Ty(Ty<'src>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpKind {
    /// `-`
    Neg,
    /// `++`
    Inc,
    /// `--`
    Dec,
    /// `&`
    Ref,
    /// `*`
    Deref,
    /// `!`
    Inv,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InfixOp {
    pub kind: InfixOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOpKind {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `&&`
    BoolAnd,
    /// `||`
    BoolOr,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `<<=`
    LShiftAssign,
    /// `>>=`
    RShiftAssign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PostfixOp {
    pub kind: PostfixOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOpKind {
    /// `++`
    Inc,
    /// `--`
    Dec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundField<'src> {
    pub name: Ident<'src>,
    pub value: Box<Expr<'src>>,
    pub span: Span,
}
