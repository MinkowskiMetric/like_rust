use std::iter::Peekable;

use super::{Expression, ExpressionError};
use crate::{CheckDelimiters, Lexer, Span, Token, TokenError};

type TokenResult = Result<Span<Token>, Span<TokenError>>;

#[must_use]
pub struct ExpressionParser<Iter: Iterator<Item = TokenResult> + Clone> {
    iter: Peekable<Iter>,
    errors: Vec<ExpressionError>,
    end_of_file: Option<Span<Token>>,
}

impl<IntoIter: IntoIterator<Item = TokenResult>> From<IntoIter>
    for ExpressionParser<IntoIter::IntoIter>
where
    IntoIter::IntoIter: Clone,
{
    fn from(s: IntoIter) -> Self {
        Self {
            iter: s.into_iter().peekable(),
            errors: Vec::new(),
            end_of_file: None,
        }
    }
}

impl<Iter: Iterator<Item = TokenResult> + Clone> ExpressionParser<Iter> {
    fn handle_token_errors(&mut self) {
        while let Some(Err(_)) = self.iter.peek() {
            self.errors
                .push(self.iter.next().unwrap().unwrap_err().into())
        }
    }

    fn next_token(&mut self) -> Span<Token> {
        self.handle_token_errors();

        // Safety - The unwrap here is fine because we already handled errors
        let next_token = unsafe { self.iter.next().transpose().unwrap_unchecked() };
        match next_token.map(Span::to_parts) {
            Some((Token::EndOfFile, range)) => self.fix_end_of_file(range),
            None => self.check_end_of_file(),

            Some((t, range)) => Span::from_parts(t, range),
        }
    }

    fn peek_token(&mut self) -> Span<Token> {
        self.handle_token_errors();

        // Safety - The unwrap here is fine because we already handled errors
        let next_token = unsafe { self.iter.peek().cloned().transpose().unwrap_unchecked() };
        match next_token.map(Span::to_parts) {
            Some((Token::EndOfFile, range)) => self.fix_end_of_file(range),
            None => self.check_end_of_file(),

            Some((t, range)) => Span::from_parts(t, range),
        }
    }

    fn fix_end_of_file(&mut self, range: std::ops::Range<usize>) -> Span<Token> {
        assert!(
            self.end_of_file.is_none(),
            "End of file token should only occur once"
        );
        let result = Span::from_parts(Token::EndOfFile, range);
        self.end_of_file = Some(result.clone());
        result
    }

    fn check_end_of_file(&self) -> Span<Token> {
        self.end_of_file
            .as_ref()
            .expect("End of file not seen before end of file")
            .clone()
    }

    pub fn parse_expression(mut self) -> Result<Span<Expression>, Vec<ExpressionError>> {
        let expression = self.expression();

        let next_token = self.next_token();
        if *next_token.value() != Token::EndOfFile {
            self.errors
                .push(ExpressionError::UnexpectedToken(next_token))
        }

        if self.errors.is_empty() {
            Ok(expression)
        } else {
            Err(self.errors)
        }
    }

    fn expression(&mut self) -> Span<Expression> {
        self.primary()
    }

    fn primary(&mut self) -> Span<Expression> {
        match self.next_token().to_parts() {
            // Literals are obviously primaries
            (
                Token::Literal {
                    contents,
                    ty,
                    suffix,
                },
                range,
            ) => Span::from_parts(
                Expression::Literal {
                    contents,
                    ty,
                    suffix,
                },
                range,
            ),
            (Token::True, range) => Span::from_parts(Expression::BoolLiteral(true), range),
            (Token::False, range) => Span::from_parts(Expression::BoolLiteral(false), range),

            (token, range) => {
                self.errors
                    .push(ExpressionError::UnexpectedToken(Span::from_parts(
                        token,
                        range.clone(),
                    )));
                Span::from_parts(Expression::Placeholder, range)
            }
        }
    }
}

pub fn parse_expression<S: IntoIterator<Item = TokenResult>>(
    tokens: S,
) -> Result<Span<Expression>, Vec<ExpressionError>>
where
    S::IntoIter: Clone,
{
    let parser = ExpressionParser::from(tokens);
    parser.parse_expression()
}

pub fn parse_expression_string(s: &str) -> Result<Span<Expression>, Vec<ExpressionError>> {
    let tokens = Lexer::from(s).check_delimiters();
    parse_expression(tokens)
}

#[cfg(test)]
mod test {
    use crate::LiteralType;

    use super::*;

    fn test_failed_expression(code: &str, expected_errors: impl AsRef<[ExpressionError]>) {
        let mut expected_errors = expected_errors.as_ref().iter().enumerate();
        let mut actual_errors = parse_expression_string(code)
            .expect_err("Test unexpectedly succeeded")
            .into_iter()
            .enumerate();

        loop {
            match (actual_errors.next(), expected_errors.next()) {
                (Some((index, actual)), Some((_, expected))) => {
                    assert_eq!(actual, *expected, "Error {} doesn't match", index)
                }
                (Some((index, _)), None) => panic!("Too many actual errors at {}", index),
                (None, Some((index, _))) => panic!("Not enough actual errors at {}", index),
                (None, None) => break,
            }
        }
    }

    fn test_expression(code: &str, expected: Span<Expression>) {
        let actual = parse_expression_string(code).expect("Test expected to succeed");
        assert_eq!(actual, expected, "Expressions do not match")
    }

    #[test]
    fn test_end_of_file() {
        test_failed_expression(
            "",
            [ExpressionError::UnexpectedToken(Span::from_parts(
                Token::EndOfFile,
                0..0,
            ))],
        );
    }

    #[test]
    fn test_numerical_literals() {
        test_expression(
            "0",
            Span::from_parts(
                Expression::Literal {
                    contents: 0..1,
                    ty: LiteralType::Decimal,
                    suffix: None,
                },
                0..1,
            ),
        );
        test_failed_expression(
            "0 1",
            [ExpressionError::UnexpectedToken(Span::from_parts(
                Token::Literal {
                    contents: 2..3,
                    ty: LiteralType::Decimal,
                    suffix: None,
                },
                2..3,
            ))],
        );
    }

    #[test]
    fn test_boolean_literals() {
        test_expression(
            "true",
            Span::from_parts(Expression::BoolLiteral(true), 0..4),
        );
        test_expression(
            "false",
            Span::from_parts(Expression::BoolLiteral(false), 0..5),
        );
    }
}
