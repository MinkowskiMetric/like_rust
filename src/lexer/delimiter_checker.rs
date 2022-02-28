use crate::{Span, Token, TokenError};

use super::{DelimiterMode, DelimiterType};

#[derive(Clone, Debug)]
pub struct DelimiterChecker<Iter: Iterator<Item = Result<Span<Token>, Span<TokenError>>>> {
    tokenizer: Iter,
    pending_token: Option<Span<Token>>,
    delimiter_stack: Vec<Span<DelimiterType>>,
}

impl<Iter: Iterator<Item = Result<Span<Token>, Span<TokenError>>>> Iterator
    for DelimiterChecker<Iter>
{
    type Item = Result<Span<Token>, Span<TokenError>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(pending_token) = self.pending_token.take() {
            Some(Ok(pending_token))
        } else {
            self.tokenizer.next().map(|token| {
                token.and_then(|token| {
                    if *token.value() == Token::EndOfFile {
                        if let Some(unmatched_open_delimiter) =
                            core::mem::take(&mut self.delimiter_stack).pop()
                        {
                            self.pending_token = Some(token);
                            Err(unmatched_open_delimiter.replace(TokenError::UnclosedDelimiter))
                        } else {
                            Ok(token)
                        }
                    } else {
                        match token.value().delimiter() {
                            None => Ok(token),
                            Some((DelimiterMode::Open, ty)) => {
                                self.delimiter_stack.push(token.clone().replace(ty));
                                Ok(token)
                            }
                            Some((DelimiterMode::Close, ty)) => {
                                if let Some(last_open) = self.delimiter_stack.pop() {
                                    if last_open.value() != &ty {
                                        // By trying to be a good citizen, if we have a mismatched
                                        // delimiter, we try to emit a correct delimiter and
                                        // see if it helps
                                        let expected_token = Token::to_delimiter(
                                            DelimiterMode::Close,
                                            last_open.value().clone(),
                                        );
                                        self.pending_token =
                                            Some(token.clone().replace(expected_token));
                                        Err(token.replace(TokenError::MismatchedDelimiter))
                                    } else {
                                        // The last open matches the current close, so we're good to go
                                        Ok(token)
                                    }
                                } else {
                                    // We have a close delimiter with no corresponding open delimiter
                                    Err(token.replace(TokenError::UnexpectedClosingDelimiter))
                                }
                            }
                        }
                    }
                })
            })
        }
    }
}

pub trait CheckDelimiters {
    type Result: Iterator<Item = Result<Span<Token>, Span<TokenError>>>;

    fn check_delimiters(self) -> Self::Result;
}

impl<IntoIter: IntoIterator<Item = Result<Span<Token>, Span<TokenError>>>> CheckDelimiters
    for IntoIter
{
    type Result = DelimiterChecker<IntoIter::IntoIter>;

    fn check_delimiters(self) -> Self::Result {
        DelimiterChecker {
            tokenizer: self.into_iter(),
            pending_token: None,
            delimiter_stack: Vec::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::Lexer;

    use super::{super::test_utils::*, *};

    #[test]
    fn test_delimiters() {
        check_tokens(
            Lexer::from(")").check_delimiters(),
            [
                Err(Span::from_parts(
                    TokenError::UnexpectedClosingDelimiter,
                    0..1,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 1..1)),
            ],
        );

        check_tokens(
            Lexer::from("(").check_delimiters(),
            [
                Ok(Span::from_parts(Token::OpenParen, 0..1)),
                Err(Span::from_parts(TokenError::UnclosedDelimiter, 0..1)),
                Ok(Span::from_parts(Token::EndOfFile, 1..1)),
            ],
        );

        check_tokens(
            Lexer::from("()").check_delimiters(),
            [
                Ok(Span::from_parts(Token::OpenParen, 0..1)),
                Ok(Span::from_parts(Token::CloseParen, 1..2)),
                Ok(Span::from_parts(Token::EndOfFile, 2..2)),
            ],
        );

        check_tokens(
            Lexer::from("(]").check_delimiters(),
            [
                Ok(Span::from_parts(Token::OpenParen, 0..1)),
                Err(Span::from_parts(TokenError::MismatchedDelimiter, 1..2)),
                Ok(Span::from_parts(Token::CloseParen, 1..2)),
                Ok(Span::from_parts(Token::EndOfFile, 2..2)),
            ],
        );
    }
}
