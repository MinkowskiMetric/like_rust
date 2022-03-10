use super::tokenizer::Lexer;
use crate::{Span, Token, TokenError};

pub fn check_tokens_chars<'a, S: Into<Lexer<'a>>>(
    chars: S,
    expected: impl AsRef<[Result<Span<Token>, Span<TokenError>>]>,
) {
    check_tokens(chars.into(), expected)
}

pub fn check_tokens<S: IntoIterator<Item = Result<Span<Token>, Span<TokenError>>>>(
    actual: S,
    expected: impl AsRef<[Result<Span<Token>, Span<TokenError>>]>,
) {
    let mut actual = actual.into_iter();
    let mut expected = expected.as_ref().into_iter().cloned().enumerate();

    loop {
        match (actual.next(), expected.next()) {
            (None, None) => break,

            (Some(unexpected_token), None) => {
                panic!("Unexpected trailing token: {:?}", unexpected_token)
            }
            (None, Some((index, expected_token))) => {
                panic!("Expected token {} missing: {:?}", index, expected_token)
            }
            (Some(actual_token), Some((index, expected_token))) => {
                assert_eq!(expected_token, actual_token, "Mismatch token {}", index)
            }
        }
    }
}

pub fn check_simple_positive_matches<'a>(matches: impl AsRef<[(&'a str, Token)]>) {
    for (chars, token) in matches.as_ref().iter().cloned() {
        check_tokens_chars(
            chars,
            [
                Ok(Span::from_parts(token, 0..chars.len())),
                Ok(Span::from_parts(Token::EndOfFile, chars.len()..chars.len())),
            ],
        )
    }
}
