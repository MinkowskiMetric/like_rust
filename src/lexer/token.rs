use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub enum NumberLiteralType {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
    Float,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CharLiteralType {
    Char,
    String,
    ByteChar,
    ByteString,
    RawString,
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
