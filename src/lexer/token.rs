use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralType {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
    Float,
    Char,
    String,
    ByteChar,
    ByteString,
    RawString,
    RawByteString,
}

impl LiteralType {
    pub fn is_raw_string(&self) -> bool {
        matches!(self, Self::RawString | Self::RawByteString)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Literal {
        contents: Range<usize>,
        ty: LiteralType,
        suffix: Option<Range<usize>>,
    },

    EndOfFile,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenError {
    UnexpectedEndOfFile,
    UnexpectedCharacter(char),
    UnterminatedComment,
    ExpectedDigit,
    ExpectedDelimiter(char),
}
