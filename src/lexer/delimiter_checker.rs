use crate::{Span, Token, TokenError};

use super::{DelimiterMode, DelimiterType, IntoTokenReader, TokenReader};

pub(crate) trait DelimitedTokenReader: TokenReader {
    fn skip_to_end_delimiter(&mut self, ty: DelimiterType);
}

pub(crate) trait IntoDelimitedTokenReader {
    type Reader: DelimitedTokenReader;

    fn into_delimited_token_reader(self) -> Self::Reader;
}

pub(crate) struct DelimiterChecker<Reader: TokenReader> {
    reader: Reader,
    delimiter_stack: Vec<Span<DelimiterType>>,
    errors: Vec<Span<TokenError>>,
    peek_entry: Option<Span<Token>>,
}

#[repr(transparent)]
struct NotNone<'a, T> {
    opt_ref: &'a mut Option<T>,
}

impl<'a, T> NotNone<'a, T> {
    pub fn get_or_fill<F: FnOnce() -> T>(opt_ref: &'a mut Option<T>, f: F) -> Self {
        if opt_ref.is_none() {
            *opt_ref = Some(f());
        }

        Self { opt_ref }
    }

    pub fn take(self) -> T {
        // Safety - We know that the option is not None because we initialized
        // it that way
        unsafe { self.opt_ref.take().unwrap_unchecked() }
    }
}

impl<T: Clone> NotNone<'_, T> {
    pub fn cloned(self) -> T {
        // Safety - We know that the option is not None because we initialized
        // it that way
        unsafe { self.opt_ref.as_ref().unwrap_unchecked().clone() }
    }
}

impl<Reader: TokenReader> DelimiterChecker<Reader> {
    fn fill_peek_entry(&mut self) -> NotNone<Span<Token>> {
        NotNone::get_or_fill(&mut self.peek_entry, || {
            loop {
                let token = self.reader.next_token();

                if *token.value() == Token::EndOfFile {
                    if let Some(unmatched_open_delimiter) =
                        std::mem::take(&mut self.delimiter_stack).pop()
                    {
                        self.errors
                            .push(unmatched_open_delimiter.replace(TokenError::UnclosedDelimiter));
                    }

                    break token;
                } else {
                    match token.value().delimiter() {
                        None => break token,

                        Some((DelimiterMode::Open, ty)) => {
                            self.delimiter_stack.push(token.clone().replace(ty));
                            break token;
                        }

                        Some((DelimiterMode::Close, ty)) => {
                            if let Some(last_open) = self.delimiter_stack.pop() {
                                break if last_open.value() != &ty {
                                    self.errors.push(
                                        token.clone().replace(TokenError::MismatchedDelimiter),
                                    );

                                    // Emit the token we *think* should be here. It might help. It might not.
                                    token.replace(Token::from_delimiter(
                                        DelimiterMode::Close,
                                        last_open.value().clone(),
                                    ))
                                } else {
                                    // The last open matches the current close, so we're good to go
                                    token
                                };
                            } else {
                                // We have a close delimiter with no corresponding open delimiter
                                self.errors.push(
                                    token
                                        .clone()
                                        .replace(TokenError::UnexpectedClosingDelimiter),
                                );
                                // Go around again and pretend it wasn't there
                            }
                        }
                    }
                }
            }
        })
    }
}

impl<Reader: TokenReader> TokenReader for DelimiterChecker<Reader> {
    fn peek_token(&mut self) -> Span<Token> {
        self.fill_peek_entry().cloned()
    }

    fn next_token(&mut self) -> Span<Token> {
        self.fill_peek_entry().take()
    }

    fn finish(mut self) -> Result<(), Vec<Span<TokenError>>> {
        let mut errors = self.reader.finish().err().unwrap_or_else(Vec::new);
        errors.append(&mut self.errors);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

impl<Reader: TokenReader> DelimitedTokenReader for DelimiterChecker<Reader> {
    fn skip_to_end_delimiter(&mut self, ty: DelimiterType) {
        let mut target_depth = self.delimiter_stack.len();
        loop {
            target_depth = target_depth
                .checked_sub(1)
                .expect("No matching open delimiter entry found to skip to");
            if *self.delimiter_stack[target_depth].value() == ty {
                break;
            }
        }

        while self.delimiter_stack.len() > target_depth {
            let (token, range) = self.next_token().to_parts();
            if token == Token::EndOfFile {
                break;
            } else {
                match token.delimiter() {
                    None => (), // Not a close delimiter, so skip

                    Some((DelimiterMode::Open, ty)) => {
                        self.delimiter_stack.push(Span::from_parts(ty, range))
                    }
                    Some((DelimiterMode::Close, _)) => {
                        self.delimiter_stack.pop();
                    }
                }
            }
        }
    }
}

impl<Reader: TokenReader> IntoDelimitedTokenReader for DelimiterChecker<Reader> {
    type Reader = Self;

    fn into_delimited_token_reader(self) -> Self {
        self
    }
}

impl<Reader: TokenReader + Clone> Clone for DelimiterChecker<Reader> {
    fn clone(&self) -> Self {
        Self {
            reader: self.reader.clone(),
            delimiter_stack: self.delimiter_stack.clone(),
            errors: self.errors.clone(),
            peek_entry: self.peek_entry.clone(),
        }
    }
}

impl<Reader: TokenReader + std::fmt::Debug> std::fmt::Debug for DelimiterChecker<Reader> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DelimiterChecker")
            .field("reader", &self.reader)
            .field("delimiter_stack", &self.delimiter_stack)
            .field("errors", &self.errors)
            .finish()
    }
}

impl<IntoReader: IntoTokenReader> IntoDelimitedTokenReader for IntoReader {
    type Reader = DelimiterChecker<IntoReader::Reader>;

    fn into_delimited_token_reader(self) -> Self::Reader {
        Self::Reader {
            reader: self.into_token_reader(),
            delimiter_stack: Vec::new(),
            errors: Vec::new(),
            peek_entry: None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::tokenizer::Lexer;
    use super::*;

    fn check_delimited_tokens(
        src: &str,
        expected_tokens: impl AsRef<[Span<Token>]>,
        expected_errors: impl AsRef<[Span<TokenError>]>,
    ) {
        let mut actual_tokens = Lexer::from(src).into_delimited_token_reader();
        let expected_errors: Vec<_> = expected_errors.as_ref().iter().cloned().collect();

        for (index, expected_token) in expected_tokens.as_ref().iter().enumerate() {
            let actual_token = actual_tokens.next_token();
            assert_eq!(actual_token, *expected_token, "Token mismatch at {}", index);
        }

        assert_eq!(
            *actual_tokens.next_token().value(),
            Token::EndOfFile,
            "Token stream not finished"
        );

        let actual_errors = actual_tokens.finish().err().unwrap_or_else(Vec::new);
        assert_eq!(actual_errors, expected_errors, "Errors mismatch");
    }

    #[test]
    fn test_delimiters() {
        check_delimited_tokens(
            ")",
            [Span::from_parts(Token::EndOfFile, 1..1)],
            [Span::from_parts(
                TokenError::UnexpectedClosingDelimiter,
                0..1,
            )],
        );

        check_delimited_tokens(
            "(",
            [
                Span::from_parts(Token::OpenParen, 0..1),
                Span::from_parts(Token::EndOfFile, 1..1),
            ],
            [Span::from_parts(TokenError::UnclosedDelimiter, 0..1)],
        );

        check_delimited_tokens(
            "()",
            [
                Span::from_parts(Token::OpenParen, 0..1),
                Span::from_parts(Token::CloseParen, 1..2),
                Span::from_parts(Token::EndOfFile, 2..2),
            ],
            [],
        );

        check_delimited_tokens(
            "(]",
            [
                Span::from_parts(Token::OpenParen, 0..1),
                Span::from_parts(Token::CloseParen, 1..2),
                Span::from_parts(Token::EndOfFile, 2..2),
            ],
            [Span::from_parts(TokenError::MismatchedDelimiter, 1..2)],
        );
    }

    #[test]
    fn test_delimiter_skipping() {
        let mut reader = Lexer::from("{((( this [is] + - hello) }?").into_delimited_token_reader();

        assert_eq!(
            reader.next_token(),
            Span::from_parts(Token::OpenBrace, 0..1)
        );
        assert_eq!(
            reader.next_token(),
            Span::from_parts(Token::OpenParen, 1..2)
        );
        assert_eq!(
            reader.next_token(),
            Span::from_parts(Token::OpenParen, 2..3)
        );
        assert_eq!(
            reader.next_token(),
            Span::from_parts(Token::OpenParen, 3..4)
        );
        reader.skip_to_end_delimiter(DelimiterType::Brace);

        assert_eq!(
            reader.next_token(),
            Span::from_parts(Token::Question, 27..28)
        );
        assert_eq!(
            reader.next_token(),
            Span::from_parts(Token::EndOfFile, 28..28)
        );
    }
}
