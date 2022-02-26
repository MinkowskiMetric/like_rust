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

    Identifier {
        contents: Range<usize>,
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

    As,        // as
    Break,     // break
    Const,     // const
    Continue,  // continue
    Crate,     // crate
    Else,      // else
    Enum,      // enum
    Extern,    // extern
    False,     // false
    Fn,        // fn
    For,       // for
    If,        // if
    Impl,      // impl
    In,        // in
    Let,       // let
    Loop,      // loop
    Match,     // match
    Mod,       // mod
    Move,      // move
    Mut,       // mut
    Pub,       // pub
    Ref,       // ref
    Return,    // return
    SelfValue, // self
    SelfType,  // Self
    Static,    // static
    Super,     // super
    Trait,     // trait
    True,      // true
    Type,      // type
    Unsafe,    // unsafe
    Use,       // use
    Where,     // where
    While,     // while

    Async, // async
    Await, // await
    Dyn,   // dyn

    Abstract, // abstract
    Become,   // become
    Box,      // box
    Do,       // do
    Final,    // final
    Macro,    // macro
    Override, // override
    Priv,     // priv
    Typeof,   // typeof
    Unsized,  // unsized
    Virtual,  // virtual
    Yield,    // yield

    Try, // try

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
