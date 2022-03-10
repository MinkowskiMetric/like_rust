use super::{expression::PathType, Expression, ExpressionError, PathComponent};
use crate::{
    lexer::{DelimitedTokenReader, IntoDelimitedTokenReader},
    Span, Token,
};

#[must_use]
pub(crate) struct ExpressionParser<Reader: DelimitedTokenReader> {
    reader: Reader,
    errors: Vec<ExpressionError>,
}

impl<IntoReader: IntoDelimitedTokenReader> From<IntoReader>
    for ExpressionParser<IntoReader::Reader>
{
    fn from(s: IntoReader) -> Self {
        Self {
            reader: s.into_delimited_token_reader(),
            errors: Vec::new(),
        }
    }
}

impl<Reader: DelimitedTokenReader + Clone> Clone for ExpressionParser<Reader> {
    fn clone(&self) -> Self {
        Self {
            reader: self.reader.clone(),
            errors: self.errors.clone(),
        }
    }
}

impl<Reader: DelimitedTokenReader> ExpressionParser<Reader> {
    #[allow(dead_code)]
    pub fn parse_expression(mut self) -> Result<Span<Expression>, Vec<ExpressionError>> {
        let expression = self.expression();

        let next_token = self.reader.next_token();
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
        let start_pos = self.reader.current_pos();
        let mut primary = self.primary();

        loop {
            if self.reader.consume_matching_token(Token::Dot) {
                match self.reader.next_token().to_parts() {
                    (Token::Identifier { contents }, _) => {
                        primary = Span::from_parts(
                            Expression::FieldAccess {
                                object: Box::new(primary),
                                identifier: contents,
                            },
                            start_pos..self.reader.current_pos(),
                        )
                    }
                    (token, range) => break self.unexpected_token(token, range),
                }
            } else if self.reader.consume_matching_token(Token::OpenParen) {
                todo!("Method calls not implemented")
            } else {
                break primary;
            }
        }
    }

    fn primary(&mut self) -> Span<Expression> {
        match self.reader.peek_token().value() {
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
        let start_pos = self.reader.current_pos();

        let ty = if self.reader.consume_matching_token(Token::PathSep) {
            PathType::Global
        } else if self.reader.consume_matching_token(Token::Lt) {
            // This is for type qualified paths like <i32 as PartialCmp>::partial_cmp or something
            todo!("type qualified paths not yet supported")
        } else {
            PathType::Local
        };

        let mut components = Vec::new();

        loop {
            match self.reader.next_token().to_parts() {
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

            if self.reader.consume_matching_token(Token::PathSep) {
                // We've hit a path separator. Check for generic arguments
                if *self.reader.peek_token().value() == Token::Lt {
                    todo!("generic arguments not yet supported")
                }
            } else {
                let end_pos = self.reader.current_pos();
                break Span::from_parts(Expression::Path { ty, components }, start_pos..end_pos);
            }
        }
    }

    fn literal(&mut self) -> Span<Expression> {
        match self.reader.next_token().to_parts() {
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

#[cfg(test)]
mod test {
    use crate::{lexer::tokenizer::Lexer, LiteralType};

    use super::*;

    fn test_failed_expression(code: &str, expected_errors: impl AsRef<[ExpressionError]>) {
        let mut actual_errors =
            ExpressionParser::from(Lexer::from(code).into_delimited_token_reader())
                .parse_expression()
                .expect_err("Test unexpectedly did not fail")
                .into_iter()
                .enumerate();
        let mut expected_errors = expected_errors.as_ref().iter().enumerate();

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
        let actual = ExpressionParser::from(Lexer::from(code).into_delimited_token_reader())
            .parse_expression()
            .expect("test expected to succeed");
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
