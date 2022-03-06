use crate::{LiteralType, Span, Token, TokenError};
use core::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum PathType {
    Global,
    Local,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PathComponent {
    Super,
    SelfValue,
    SelfType,
    Crate,
    Identifier { contents: Range<usize> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal {
        contents: Range<usize>,
        ty: LiteralType,
        suffix: Option<Range<usize>>,
    },
    BoolLiteral(bool),
    Path {
        ty: PathType,
        components: Vec<Span<PathComponent>>,
    },
    FieldAccess {
        object: Box<Span<Expression>>,
        identifier: Range<usize>,
    },

    Placeholder,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionError {
    TokenError(Span<TokenError>),
    UnexpectedToken(Span<Token>),
}

impl From<Span<TokenError>> for ExpressionError {
    fn from(s: Span<TokenError>) -> Self {
        Self::TokenError(s)
    }
}
