use std::iter::Peekable;

use crate::{Span, Token, TokenError};

pub(crate) trait IntoTokenReader {
    type Reader: TokenReader;

    fn into_token_reader(self) -> Self::Reader;
}

#[must_use]
pub(crate) trait TokenReader {
    fn peek_token(&mut self) -> Span<Token>;
    fn next_token(&mut self) -> Span<Token>;

    fn finish(self) -> Result<(), Vec<Span<TokenError>>>;

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

    /// Consumes tokens up to and including the next semicolon.
    /// This is intended to be used in error handling scenarios where there is no way
    /// to recover the current state, but you know there is a semicolon coming.
    fn consume_to_semicolon(&mut self) {
        loop {
            if matches!(
                self.next_token().to_parts().0,
                Token::Semi | Token::EndOfFile
            ) {
                break;
            }
        }
    }
}

type TokenResult = Result<Span<Token>, Span<TokenError>>;

#[must_use]
pub(crate) struct TokenIteratorReader<Iter: Iterator<Item = TokenResult>> {
    iter: Peekable<Iter>,
    errors: Vec<Span<TokenError>>,
    end_of_file: Option<Span<Token>>,
}

impl<Iter: Iterator<Item = TokenResult>> TokenReader for TokenIteratorReader<Iter> {
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

    fn finish(self) -> Result<(), Vec<Span<TokenError>>> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }
}

impl<Iter: Iterator<Item = TokenResult>> TokenIteratorReader<Iter> {
    fn handle_token_errors(&mut self) {
        while let Some(Err(_)) = self.iter.peek() {
            self.errors.push(self.iter.next().unwrap().unwrap_err())
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
}

impl<Iter: Iterator<Item = TokenResult> + Clone> Clone for TokenIteratorReader<Iter> {
    fn clone(&self) -> Self {
        Self {
            iter: self.iter.clone(),
            end_of_file: self.end_of_file.clone(),
            errors: self.errors.clone(),
        }
    }
}

impl<Iter: Iterator<Item = TokenResult> + std::fmt::Debug> std::fmt::Debug
    for TokenIteratorReader<Iter>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenIteratorReader")
            .field("iter", &self.iter)
            .field("end_of_file", &self.end_of_file)
            .finish()
    }
}

impl<Iter: Iterator<Item = TokenResult>> IntoTokenReader for TokenIteratorReader<Iter> {
    type Reader = Self;

    fn into_token_reader(self) -> Self {
        self
    }
}

impl<IntoIter: IntoIterator<Item = TokenResult>> IntoTokenReader for IntoIter {
    type Reader = TokenIteratorReader<IntoIter::IntoIter>;

    fn into_token_reader(self) -> Self::Reader {
        Self::Reader {
            iter: self.into_iter().peekable(),
            end_of_file: None,
            errors: Vec::new(),
        }
    }
}
