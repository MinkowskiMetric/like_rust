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
pub enum Token {
    Number {
        digits: Range<usize>,
        ty: NumberLiteralType,
        suffix: Option<Range<usize>>,
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
}
