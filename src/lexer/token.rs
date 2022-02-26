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
pub enum TokenType {
    Literal,
    Punctuation,
    Delimiter,
    Identifier,
    Keyword,
    ReservedWord,
    EndOfFile,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DelimiterType {
    Paren,
    Brace,
    Bracket,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DelimiterMode {
    Open,
    Close,
}

impl Token {
    pub fn token_type(&self) -> TokenType {
        match self {
            Self::Literal { .. } => TokenType::Literal,
            Self::Identifier { .. } => TokenType::Identifier,

            Self::Plus |        // +
            Self::Minus |       // -
            Self::Star |        // *
            Self::Slash |       // /
            Self::Percent |     // %
            Self::Caret |       // ^
            Self::Not |         // !
            Self::And |         // &
            Self::Or |          // |
            Self::AndAnd |      // &&
            Self::OrOr |        // ||
            Self::Shl |         // <<
            Self::Shr |         // >>
            Self::PlusEq |      // +=
            Self::MinusEq |     // -=
            Self::StarEq |      // *=
            Self::SlashEq |     // /=
            Self::PercentEq |   // %=
            Self::CaretEq |     // ^=
            Self::AndEq |       // &=
            Self::OrEq |        // |=
            Self::ShlEq |       // <<=
            Self::ShrEq |       // >>=
            Self::Eq |          // =
            Self::EqEq |        // ==
            Self::Ne |          // !=
            Self::Gt |          // >
            Self::Lt |          // <
            Self::Ge |          // >=
            Self::Le |          // <=
            Self::At |          // @
            Self::Underscore |  // _
            Self::Dot |         // .
            Self::DotDot |      // ..
            Self::DotDotDot |   // ...
            Self::DotDotEq |    // ..=
            Self::Comma |       // ,
            Self::Semi |        // ;
            Self::Colon |       // :
            Self::PathSep |     // ::
            Self::RArrow |      // ->
            Self::FatArrow |    // =>
            Self::Pound |       // #
            Self::Dollar |      // $
            Self::Question => TokenType::Punctuation,

            Self::OpenParen |   // (
            Self::CloseParen |  // )
            Self::OpenBracket | // [
            Self::CloseBracket |// ]
            Self::OpenBrace |   // {
            Self::CloseBrace => TokenType::Delimiter,

            Self::As |       // as
            Self::Break |    // break
            Self::Const |    // const
            Self::Continue | // continue
            Self::Crate |    // crate
            Self::Else |     // else
            Self::Enum |     // enum
            Self::Extern |   // extern
            Self::False |    // false
            Self::Fn |       // fn
            Self::For |      // for
            Self::If |       // if
            Self::Impl |     // impl
            Self::In |       // in
            Self::Let |      // let
            Self::Loop |     // loop
            Self::Match |    // match
            Self::Mod |      // mod
            Self::Move |     // move
            Self::Mut |      // mut
            Self::Pub |      // pub
            Self::Ref |      // ref
            Self::Return |   // return
            Self::SelfValue |// self
            Self::SelfType | // Self
            Self::Static |   // static
            Self::Super |    // super
            Self::Trait |    // trait
            Self::True |     // true
            Self::Type |     // type
            Self::Unsafe |   // unsafe
            Self::Use |      // use
            Self::Where |    // where
            Self::While |    // while
            Self::Async |// async
            Self::Await |// await
            Self::Dyn => TokenType::Keyword,

            Self::Abstract |// abstract
            Self::Become |  // become
            Self::Box |     // box
            Self::Do |      // do
            Self::Final |   // final
            Self::Macro |   // macro
            Self::Override |// override
            Self::Priv |    // priv
            Self::Typeof |  // typeof
            Self::Unsized | // unsized
            Self::Virtual | // virtual
            Self::Yield |   // yield
            Self::Try => TokenType::ReservedWord,

            Self::EndOfFile => TokenType::EndOfFile,
        }
    }

    pub fn delimiter(&self) -> Option<(DelimiterMode, DelimiterType)> {
        match self {
            Self::OpenParen => Some((DelimiterMode::Open, DelimiterType::Paren)),
            Self::CloseParen => Some((DelimiterMode::Close, DelimiterType::Paren)),

            Self::OpenBrace => Some((DelimiterMode::Open, DelimiterType::Brace)),
            Self::CloseBrace => Some((DelimiterMode::Close, DelimiterType::Brace)),

            Self::OpenBracket => Some((DelimiterMode::Open, DelimiterType::Bracket)),
            Self::CloseBracket => Some((DelimiterMode::Close, DelimiterType::Bracket)),

            _ => None,
        }
    }

    pub fn delimiter_type(&self) -> Option<DelimiterType> {
        self.delimiter().map(|(_, ty)| ty)
    }

    pub fn delimiter_mode(&self) -> Option<DelimiterMode> {
        self.delimiter().map(|(mode, _)| mode)
    }

    pub fn to_delimiter(mode: DelimiterMode, ty: DelimiterType) -> Self {
        match (mode, ty) {
            (DelimiterMode::Open, DelimiterType::Brace) => Token::OpenBrace,
            (DelimiterMode::Close, DelimiterType::Brace) => Token::CloseBrace,
            (DelimiterMode::Open, DelimiterType::Bracket) => Token::OpenBracket,
            (DelimiterMode::Close, DelimiterType::Bracket) => Token::CloseBracket,
            (DelimiterMode::Open, DelimiterType::Paren) => Token::OpenParen,
            (DelimiterMode::Close, DelimiterType::Paren) => Token::CloseParen,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenError {
    UnexpectedEndOfFile,
    UnexpectedCharacter(char),
    UnterminatedComment,
    ExpectedDigit,
    ExpectedDelimiter(char),
    UnexpectedClosingDelimiter,
    UnclosedDelimiter,
    MismatchedDelimiter,
}
