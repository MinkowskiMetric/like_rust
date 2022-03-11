use super::{expression::PathType, Expression, ExpressionError, PathComponent};
use crate::{
    lexer::{DelimitedTokenReader, IntoDelimitedTokenReader, TokenReader},
    Span, Token,
};

pub(crate) fn expression(
    tokens: &mut impl DelimitedTokenReader,
    errors: &mut Vec<ExpressionError>,
) -> Span<Expression> {
    call(tokens, errors)
}

fn call(
    tokens: &mut impl DelimitedTokenReader,
    errors: &mut Vec<ExpressionError>,
) -> Span<Expression> {
    let start_pos = tokens.current_pos();
    let mut primary = primary(tokens, errors);

    loop {
        if tokens.consume_matching_token(Token::Dot) {
            match tokens.next_token().to_parts() {
                (Token::Identifier { contents }, _) => {
                    primary = Span::from_parts(
                        Expression::FieldAccess {
                            object: Box::new(primary),
                            identifier: contents,
                        },
                        start_pos..tokens.current_pos(),
                    )
                }
                (token, range) => {
                    errors.push(ExpressionError::UnexpectedToken(Span::from_parts(
                        token, range,
                    )));
                    break Span::from_parts(
                        Expression::Placeholder,
                        start_pos..tokens.current_pos(),
                    );
                }
            }
        } else if tokens.consume_matching_token(Token::OpenParen) {
            todo!("Method calls not implemented")
        } else {
            break primary;
        }
    }
}

fn primary(
    tokens: &mut impl DelimitedTokenReader,
    errors: &mut Vec<ExpressionError>,
) -> Span<Expression> {
    match tokens.peek_token().value() {
        Token::PathSep
        | Token::Super
        | Token::Crate
        | Token::SelfValue
        | Token::SelfType
        | Token::Lt
        | Token::Identifier { .. } => path(tokens, errors),

        _ => literal(tokens, errors),
    }
}

fn path(
    tokens: &mut impl DelimitedTokenReader,
    errors: &mut Vec<ExpressionError>,
) -> Span<Expression> {
    let start_pos = tokens.current_pos();

    let ty = if tokens.consume_matching_token(Token::PathSep) {
        PathType::Global
    } else if tokens.consume_matching_token(Token::Lt) {
        // This is for type qualified paths like <i32 as PartialCmp>::partial_cmp or something
        todo!("type qualified paths not yet supported")
    } else {
        PathType::Local
    };

    let mut components = Vec::new();

    loop {
        match tokens.next_token().to_parts() {
            (Token::Super, range) => components.push(Span::from_parts(PathComponent::Super, range)),
            (Token::Crate, range) => components.push(Span::from_parts(PathComponent::Crate, range)),
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

            (token, range) => {
                errors.push(ExpressionError::UnexpectedToken(Span::from_parts(
                    token, range,
                )));
                break Span::from_parts(Expression::Placeholder, start_pos..tokens.current_pos());
            }
        }

        if tokens.consume_matching_token(Token::PathSep) {
            // We've hit a path separator. Check for generic arguments
            if *tokens.peek_token().value() == Token::Lt {
                todo!("generic arguments not yet supported")
            }
        } else {
            break Span::from_parts(
                Expression::Path { ty, components },
                start_pos..tokens.current_pos(),
            );
        }
    }
}

fn literal(
    tokens: &mut impl DelimitedTokenReader,
    errors: &mut Vec<ExpressionError>,
) -> Span<Expression> {
    match tokens.next_token().to_parts() {
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
        (token, range) => {
            errors.push(ExpressionError::UnexpectedToken(Span::from_parts(
                token,
                range.clone(),
            )));
            Span::from_parts(Expression::Placeholder, range)
        }
    }
}

#[allow(dead_code)]
pub(crate) fn single_expression(
    tokens: impl IntoDelimitedTokenReader,
) -> Result<Span<Expression>, Vec<ExpressionError>> {
    let mut errors = Vec::new();
    let mut tokens = tokens.into_delimited_token_reader();
    let expression = expression(&mut tokens, &mut errors);

    let next_token = tokens.next_token();
    if *next_token.value() != Token::EndOfFile {
        errors.push(ExpressionError::UnexpectedToken(next_token));
    }

    if let Err(token_errors) = tokens.finish() {
        errors.extend(token_errors.into_iter().map(ExpressionError::TokenError));
    }

    if errors.is_empty() {
        Ok(expression)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::{tokenizer::Lexer, TokenError},
        LiteralType,
    };

    use super::*;

    fn test_failed_expression(code: &str, expected_errors: impl AsRef<[ExpressionError]>) {
        let mut actual_errors = single_expression(Lexer::from(code).into_delimited_token_reader())
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
        let actual = single_expression(Lexer::from(code).into_delimited_token_reader())
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
    fn test_token_error() {
        test_failed_expression(
            "\"boo",
            [
                ExpressionError::UnexpectedToken(Span::from_parts(Token::EndOfFile, 4..4)),
                ExpressionError::TokenError(Span::from_parts(
                    TokenError::ExpectedDelimiter('"'),
                    4..4,
                )),
            ],
        )
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
