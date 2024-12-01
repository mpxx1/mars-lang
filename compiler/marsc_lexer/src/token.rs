use marsc_span::CodeSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // digits
    Int64(i64), // 2, 48, -1e4

    // operators
    Plus,  // '+'
    Minus, // '-'
    Star,  // '*'
    Slash, // '/'

    // cmp
    Equals,

    // separators
    LeftBracket,        // '('
    RightBracket,       // ')'
    LeftBrace,          // '{'
    RightBrace,         // '}'
    LeftSquareBracket,  // '['
    RightSquareBracket, // ']'
    NewLine,            // '\n', invisible
    Tab,                // '\t'

    // other
    Bad, // unrecognized
    Eof, // EOF
}


#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: CodeSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: CodeSpan) -> Self {
        Self { kind, span }
    }
}