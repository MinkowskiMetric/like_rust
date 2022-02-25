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

    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Caret,        // ^
    Not,          // !
    And,          // &
    Or,           // |
    AndAnd,       // &&
    OrOr,         // ||
    Shl,          // <<
    Shr,          // >>
    PlusEq,       // +=
    MinusEq,      // -=
    StarEq,       // *=
    SlashEq,      // /=
    PercentEq,    // %=
    CaretEq,      // ^=
    AndEq,        // &=
    OrEq,         // |=
    ShlEq,        // <<=
    ShrEq,        // >>=
    Eq,           // =
    EqEq,         // ==
    Ne,           // !=
    Gt,           // >
    Lt,           // <
    Ge,           // >=
    Le,           // <=
    At,           // @
    Underscore,   // _
    Dot,          // .
    DotDot,       // ..
    DotDotDot,    // ...
    DotDotEq,     // ..=
    Comma,        // ,
    Semi,         // ;
    Colon,        // :
    PathSep,      // ::
    RArrow,       // ->
    FatArrow,     // =>
    Pound,        // #
    Dollar,       // $
    Question,     // ?
    OpenParen,    // (
    CloseParen,   // )
    OpenBracket,  // [
    CloseBracket, // ]
    OpenBrace,    // {
    CloseBrace,   // }

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
