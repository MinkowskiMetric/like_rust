use crate::{LiteralType, Span, Token, TokenError};
use core::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal {
        contents: Range<usize>,
        ty: LiteralType,
        suffix: Option<Range<usize>>,
    },
    BoolLiteral(bool),

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
