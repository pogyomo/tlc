use std::num::{ParseFloatError, ParseIntError};

use crate::span::Span;
use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub span: Span,
}

#[derive(Logos, Debug, Clone, Copy, PartialEq)]
#[logos(skip r"([\r\n\s]+)|(//[^\n]*)|(/\*([^*]|\*[^/])*\*/)")]
pub enum TokenKind<'src> {
    /// Identifier, e.g. `ident`
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Ident(&'src str),

    /// Integer, e.g. `10`, `0x2c`
    #[regex(r"(0b[01]+)|(0o[0-7]+)|(0x[0-9a-zA-Z]+)|[0-9]+", |lex| RawInt(lex.slice()))]
    Int(RawInt<'src>),

    /// Floating point number, e.g. `2.3`, `1.3e-2`
    #[regex(r"([0-9]+\.[0-9]+([eE][+-]?[0-9]+)?)|([0-9]+[eE][+-]?[0-9]+)", |lex| RawFloat(lex.slice()))]
    Float(RawFloat<'src>),

    /// String literal, e.g. `"string"`
    #[regex(r#""([^"\\\x00-\x1f]|\\["\\abfnrtv])*""#, |lex| RawString(lex.slice()))]
    String(RawString<'src>),

    /// Character literal, e.g. `'a'`, `'\n'`
    /// May contains multiple character, so must be validated before use it.
    #[regex(r#"'([^'\\\x00-\x1f]|\\['\\abfnrtv])*'"#, |lex| RawCharacter(lex.slice()))]
    Character(RawCharacter<'src>),

    /// `pub`
    #[token("pub")]
    Pub,
    /// `func`
    #[token("func")]
    Func,
    /// `if`
    #[token("if")]
    If,
    /// `else`
    #[token("else")]
    Else,
    /// `let`
    #[token("let")]
    Let,
    /// `var`
    #[token("var")]
    Var,
    /// `for`
    #[token("for")]
    For,
    /// `while`
    #[token("while")]
    While,
    /// `loop`
    #[token("loop")]
    Loop,
    /// `break`
    #[token("break")]
    Break,
    /// `continue`
    #[token("continue")]
    Continue,
    /// `return`
    #[token("return")]
    Return,
    /// `defer`
    #[token("defer")]
    Defer,
    /// `import`
    #[token("import")]
    Import,
    /// `module`
    #[token("module")]
    Module,
    /// `test`
    #[token("test")]
    Test,
    /// `struct`
    #[token("struct")]
    Struct,
    /// `union`
    #[token("union")]
    Union,
    /// `enum`
    #[token("enum")]
    Enum,
    /// `as`
    #[token("as")]
    As,
    /// `u8`
    #[token("u8")]
    U8,
    /// `u16`
    #[token("u16")]
    U16,
    /// `u32`
    #[token("u32")]
    U32,
    /// `u64`
    #[token("u64")]
    U64,
    /// `usize`
    #[token("usize")]
    USize,
    /// `i8`
    #[token("i8")]
    I8,
    /// `i16`
    #[token("i16")]
    I16,
    /// `i32`
    #[token("i32")]
    I32,
    /// `i64`
    #[token("i64")]
    I64,
    /// `isize`
    #[token("isize")]
    ISize,
    /// `f32`
    #[token("f32")]
    F32,
    /// `f64`
    #[token("f64")]
    F64,
    /// `bool`
    #[token("bool")]
    Bool,
    /// `char`
    #[token("char")]
    Char,
    /// `type`
    #[token("type")]
    Type,
    /// `const`
    #[token("const")]
    Const,
    /// `true`
    #[token("true")]
    True,
    /// `false`
    #[token("false")]
    False,
    /// `null`
    #[token("null")]
    Null,

    // Single character punctuations
    /// `(`
    #[token("(")]
    LParen,
    /// `)`
    #[token(")")]
    RParen,
    /// `[`
    #[token("[")]
    LSquare,
    /// `]`
    #[token("]")]
    RSquare,
    /// `{`
    #[token("{")]
    LCurly,
    /// `}`
    #[token("}")]
    RCurly,
    /// `;`
    #[token(";")]
    Semi,
    /// `:`
    #[token(":")]
    Colon,
    /// `,`
    #[token(",")]
    Comma,
    /// `.`
    #[token(".")]
    Dot,
    /// `<`
    #[token("<")]
    Lt,
    /// `>`
    #[token(">")]
    Gt,
    /// `=`
    #[token("=")]
    Assign,
    /// `+`
    #[token("+")]
    Plus,
    /// `-`
    #[token("-")]
    Minus,
    /// `*`
    #[token("*")]
    Star,
    /// `/`
    #[token("/")]
    Slash,
    /// `%`
    #[token("%")]
    Percent,
    /// `&`
    #[token("&")]
    And,
    /// `|`
    #[token("|")]
    Vert,
    /// `^`
    #[token("^")]
    Hat,
    /// `!`
    #[token("!")]
    Bang,

    // Multi character punctuations
    /// `->`
    #[token("->")]
    Arrow,
    /// `++`
    #[token("++")]
    Inc,
    /// `--`
    #[token("--")]
    Dec,
    /// `&&`
    #[token("&&")]
    AndAnd,
    /// `||`
    #[token("||")]
    VertVert,
    /// `<<`
    #[token("<<")]
    LShift,
    /// `>>`
    #[token(">>")]
    RShift,
    /// `==`
    #[token("==")]
    Eq,
    /// `!=`
    #[token("!=")]
    Ne,
    /// `<=`
    #[token("<=")]
    Le,
    /// `>=`
    #[token(">=")]
    Ge,
    /// `+=`
    #[token("+=")]
    AddAssign,
    /// `-=`
    #[token("-=")]
    SubAssign,
    /// `*=`
    #[token("*=")]
    MulAssign,
    /// `/=`
    #[token("/=")]
    DivAssign,
    /// `<<=`
    #[token("<<=")]
    LShiftAssign,
    /// `>>=`
    #[token(">>=")]
    RShiftAssign,

    /// Unknown token
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawInt<'src>(&'src str);

impl<'src> RawInt<'src> {
    #[inline]
    pub fn raw(&self) -> &'src str {
        self.0
    }

    pub fn parse(&self) -> Result<u64, ParseIntError> {
        let s = self.0;
        let (radix, s) = if s.starts_with("0b") {
            (2, s.trim_start_matches("0b"))
        } else if s.starts_with("0o") {
            (8, s.trim_start_matches("0o"))
        } else if s.starts_with("0x") {
            (16, s.trim_start_matches("0x"))
        } else {
            (10, s)
        };
        u64::from_str_radix(s, radix)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawFloat<'src>(&'src str);

impl<'src> RawFloat<'src> {
    #[inline]
    pub fn raw(&self) -> &'src str {
        self.0
    }

    #[inline]
    pub fn parse(&self) -> Result<f64, ParseFloatError> {
        self.0.parse()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawString<'src>(&'src str);

impl<'src> RawString<'src> {
    #[inline]
    pub fn raw(&self) -> &'src str {
        self.0
    }

    #[inline]
    pub fn raw_content(&self) -> &'src str {
        self.0.trim_matches('"')
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawCharacter<'src>(&'src str);

impl<'src> RawCharacter<'src> {
    #[inline]
    pub fn raw(&self) -> &'src str {
        self.0
    }

    #[inline]
    pub fn raw_content(&self) -> &'src str {
        self.0.trim_matches('\'')
    }
}
