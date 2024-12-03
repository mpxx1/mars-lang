use m_span::CodeSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // literals
    Int64(i64),
    Float64(f64),
    CharLit(char),
    StringLit(String),
    Ident(String),

    // keywords
    And,
    Struct,
    Else,
    False,
    Fun,
    For,
    If,
    Null,
    Or,
    Print,
    Return,
    SelfType,
    True,
    Var,
    While,
    Impl,

    // types
    Bool,
    String,
    Char, // ?
    Void,
    I64,
    F64,
    Array(String, usize), // size, type ident

    // single-char tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    // Ampersand,

    // one or two chars
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Returns,    // ->

    // other
    NewLine,
    Eof,
    Bad, // unrecognized
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: CodeSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: CodeSpan) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind.clone()
    }

    pub fn span(&self) -> CodeSpan {
        self.span.clone()
    }

    pub fn extract(self) -> (TokenKind, CodeSpan) {
        (self.kind, self.span)
    }
}
