use marsc_span::CodeSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // digits
    // ..smaller
    Int32(i32), // 2, 48, -1e4
    Int64(i64),
    UInt32(u32),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
    // larger

    // operators
    Assignment, // '='
    Plus,       // '+'
    Minus,      // '-'
    Star,       // '*'
    Slash,      // '/'

    // cmp
    Equals, // '=='

    // separators
    LeftBracket,        // '('
    RightBracket,       // ')'
    LeftBrace,          // '{'
    RightBrace,         // '}'
    LeftSquareBracket,  // '['
    RightSquareBracket, // ']'
    Whitespace,         // ' '
    Tab,                // '\t'
    NewLine,            // '\n', invisible

    // other
    Eof, // EOF
    Bad, // unrecognized
}


#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) kind: TokenKind,
    span: CodeSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: CodeSpan) -> Self {
        Self { kind, span }
    }
}