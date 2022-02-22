use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberLiteralType {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
    Float,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CharLiteralType {
    Char,
    String,
    ByteChar,
    ByteString,
    RawString,
    RawByteString,
}

impl CharLiteralType {
    pub fn is_raw(&self) -> bool {
        matches!(self, Self::RawString | Self::RawByteString)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Number {
        digits: Range<usize>,
        ty: NumberLiteralType,
        suffix: Option<Range<usize>>,
    },

    CharLiteral {
        contents: Range<usize>,
        ty: CharLiteralType,
    },

    EndOfFile,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenError {
    UnexpectedEndOfFile,
    UnexpectedCharacter(char),
    UnterminatedComment,
    UnexpectedEndOfComment,
    ExpectedDigit,
    ExpectedDelimiter(char),
}
