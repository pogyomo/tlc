pub mod decl;
pub mod expr;
pub mod stmt;
pub mod ty;

use crate::ast::decl::Decl;
use tlc_lexer::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident<'src> {
    pub name: &'src str,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceTree<'src> {
    pub decls: Vec<Decl<'src>>,
}
