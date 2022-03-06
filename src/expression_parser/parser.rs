use std::iter::Peekable;

use super::{expression::PathType, Expression, ExpressionError, PathComponent};
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

    fn current_pos(&mut self) -> usize {
        self.peek_token().range().start
    }

    fn consume_matching_token(&mut self, token: Token) -> bool {
        if *self.peek_token().value() == token {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn fix_end_of_file(&mut self, range: std::ops::Range<usize>) -> Span<Token> {
        // This can occur more than once if we peek at the end of the file
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
        self.call()
    }

    fn call(&mut self) -> Span<Expression> {
        let start_pos = self.current_pos();
        let mut primary = self.primary();

        loop {
            if self.consume_matching_token(Token::Dot) {
                match self.next_token().to_parts() {
                    (Token::Identifier { contents }, _) => {
                        primary = Span::from_parts(
                            Expression::FieldAccess {
                                object: Box::new(primary),
                                identifier: contents,
                            },
                            start_pos..self.current_pos(),
                        )
                    }
                    (token, range) => break self.unexpected_token(token, range),
                }
            } else if self.consume_matching_token(Token::OpenParen) {
                todo!("Method calls not implemented")
            } else {
                break primary;
            }
        }
    }

    fn primary(&mut self) -> Span<Expression> {
        match self.peek_token().value() {
            Token::PathSep
            | Token::Super
            | Token::Crate
            | Token::SelfValue
            | Token::SelfType
            | Token::Lt
            | Token::Identifier { .. } => self.path(),

            _ => self.literal(),
        }
    }

    fn path(&mut self) -> Span<Expression> {
        let start_pos = self.current_pos();

        let ty = if self.consume_matching_token(Token::PathSep) {
            PathType::Global
        } else if self.consume_matching_token(Token::Lt) {
            // This is for type qualified paths like <i32 as PartialCmp>::partial_cmp or something
            todo!("type qualified paths not yet supported")
        } else {
            PathType::Local
        };

        let mut components = Vec::new();

        loop {
            match self.next_token().to_parts() {
                (Token::Super, range) => {
                    components.push(Span::from_parts(PathComponent::Super, range))
                }
                (Token::Crate, range) => {
                    components.push(Span::from_parts(PathComponent::Crate, range))
                }
                (Token::SelfValue, range) => {
                    components.push(Span::from_parts(PathComponent::SelfValue, range))
                }
                (Token::SelfType, range) => {
                    components.push(Span::from_parts(PathComponent::SelfType, range))
                }
                (Token::Identifier { contents }, range) => components.push(Span::from_parts(
                    PathComponent::Identifier { contents },
                    range,
                )),

                (token, range) => break self.unexpected_token(token, range),
            }

            if self.consume_matching_token(Token::PathSep) {
                // We've hit a path separator. Check for generic arguments
                if *self.peek_token().value() == Token::Lt {
                    todo!("generic arguments not yet supported")
                }
            } else {
                let end_pos = self.current_pos();
                break Span::from_parts(Expression::Path { ty, components }, start_pos..end_pos);
            }
        }
    }

    fn literal(&mut self) -> Span<Expression> {
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

            // Literals are always at the end of the chain. If it isn't a literal,
            // it is an error.
            (token, range) => self.unexpected_token(token, range),
        }
    }

    fn unexpected_token(
        &mut self,
        token: Token,
        range: std::ops::Range<usize>,
    ) -> Span<Expression> {
        self.errors
            .push(ExpressionError::UnexpectedToken(Span::from_parts(
                token,
                range.clone(),
            )));
        Span::from_parts(Expression::Placeholder, range)
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

    #[test]
    fn test_paths() {
        test_expression(
            "Self::hello",
            Span::from_parts(
                Expression::Path {
                    ty: PathType::Local,
                    components: vec![
                        Span::from_parts(PathComponent::SelfType, 0..4),
                        Span::from_parts(PathComponent::Identifier { contents: 6..11 }, 6..11),
                    ],
                },
                0..11,
            ),
        );
        test_expression(
            "::Self::hello",
            Span::from_parts(
                Expression::Path {
                    ty: PathType::Global,
                    components: vec![
                        Span::from_parts(PathComponent::SelfType, 2..6),
                        Span::from_parts(PathComponent::Identifier { contents: 8..13 }, 8..13),
                    ],
                },
                0..13,
            ),
        );
    }

    #[test]
    fn test_field_access() {
        test_expression(
            "one::two::three.hello",
            Span::from_parts(
                Expression::FieldAccess {
                    object: Box::new(Span::from_parts(
                        Expression::Path {
                            ty: PathType::Local,
                            components: vec![
                                Span::from_parts(
                                    PathComponent::Identifier { contents: 0..3 },
                                    0..3,
                                ),
                                Span::from_parts(
                                    PathComponent::Identifier { contents: 5..8 },
                                    5..8,
                                ),
                                Span::from_parts(
                                    PathComponent::Identifier { contents: 10..15 },
                                    10..15,
                                ),
                            ],
                        },
                        0..15,
                    )),
                    identifier: 16..21,
                },
                0..21,
            ),
        );
        test_expression(
            "one::two::three.hello.world",
            Span::from_parts(
                Expression::FieldAccess {
                    object: Box::new(Span::from_parts(
                        Expression::FieldAccess {
                            object: Box::new(Span::from_parts(
                                Expression::Path {
                                    ty: PathType::Local,
                                    components: vec![
                                        Span::from_parts(
                                            PathComponent::Identifier { contents: 0..3 },
                                            0..3,
                                        ),
                                        Span::from_parts(
                                            PathComponent::Identifier { contents: 5..8 },
                                            5..8,
                                        ),
                                        Span::from_parts(
                                            PathComponent::Identifier { contents: 10..15 },
                                            10..15,
                                        ),
                                    ],
                                },
                                0..15,
                            )),
                            identifier: 16..21,
                        },
                        0..21,
                    )),
                    identifier: 22..27,
                },
                0..27,
            ),
        );
    }
}
