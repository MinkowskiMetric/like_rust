use super::{CharLiteralType, CharReader, NumberLiteralType, Token, TokenError};
use crate::Span;

const fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        '\u{0009}' |                // Horizontal tab
        '\u{000A}' |                // Line feed
        '\u{000B}' |                // Vertical tab
        '\u{000C}' |                // Form feed
        '\u{000D}' |                // Carriage return
        '\u{0020}' |                // Space
        '\u{0085}' |                // Next line
        '\u{200E}' |                // Left to right mark
        '\u{200F}' |                // Right to left mark
        '\u{2028}' |                // Line separator
        '\u{2029}'
    ) // Paragraph separator
}

fn is_identifier_start(ch: char) -> bool {
    unic::ucd::ident::is_xid_start(ch)
}

fn is_identifier_continue(ch: char) -> bool {
    unic::ucd::ident::is_xid_continue(ch)
}

#[must_use]
#[derive(Clone)]
pub struct Lexer<'a> {
    chars: CharReader<'a>,
    end_of_file: bool,
}

impl Lexer<'_> {
    fn assert_next_char(&mut self, c: char) -> std::ops::Range<usize> {
        let start_pos = self.chars.current_pos();
        assert_eq!(self.chars.next(), Some(c), "Expected character '{}'", c);
        let end_pos = self.chars.current_pos();
        start_pos..end_pos
    }

    fn assert_next_str(&mut self, str: &str) -> std::ops::Range<usize> {
        let start_pos = self.chars.current_pos();

        for c in str.chars() {
            self.assert_next_char(c);
        }

        let end_pos = self.chars.current_pos();
        start_pos..end_pos
    }

    fn next_token(&mut self) -> Result<Span<Token>, Span<TokenError>> {
        loop {
            if let Some(literal_token) = match self.string_literal()? {
                Some(token) => Some(token),
                None => self.number_literal()?,
            } {
                break Ok(literal_token);
            }

            match self.chars.peek1() {
                None => {
                    break Ok(Span::from_parts(
                        Token::EndOfFile,
                        self.chars.current_pos()..self.chars.current_pos(),
                    ))
                }

                Some('/') => match self.chars.peek2() {
                    Some('/') => self.skip_to_end_of_line()?,
                    Some('*') => self.skip_block_comment()?,
                    _ => self.emit_unexpected_char('/')?,
                },

                Some('*') if self.chars.peek2() == Some('/') => {
                    let range = self.assert_next_str("*/");
                    break Err(Span::from_parts(TokenError::UnexpectedEndOfComment, range));
                }

                Some(c) => self.emit_unexpected_char(c)?,
            }
        }
    }

    fn emit_unexpected_char(&mut self, c: char) -> Result<(), Span<TokenError>> {
        let range = self.assert_next_char(c);
        self.skip_to_end_of_line()?;
        Err(Span::from_parts(TokenError::UnexpectedCharacter(c), range))
    }

    fn consume_char_if(&mut self, f: impl FnOnce(char) -> bool) -> Option<char> {
        match self.chars.peek1() {
            Some(c) if f(c) => {
                self.chars.next();
                Some(c)
            }
            _ => None,
        }
    }

    fn skip_if(&mut self, f: impl (Fn(char) -> bool) + Clone) {
        while self.consume_char_if(f.clone()).is_some() {
            // Skip it
        }
    }

    fn skip_whitespace(&mut self) {
        self.skip_if(is_whitespace)
    }

    fn skip_to_end_of_line(&mut self) -> Result<(), Span<TokenError>> {
        while let Some(c) = self.chars.next() {
            if c == '\u{000D}' {
                break;
            }
        }

        Ok(())
    }

    fn consume_identifier(&mut self) -> Option<Span<()>> {
        let start_pos = self.chars.current_pos();

        if self.consume_char_if(is_identifier_start).is_some() {
            self.skip_if(is_identifier_continue);

            Some(Span::from_parts((), start_pos..self.chars.current_pos()))
        } else {
            None
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), Span<TokenError>> {
        let mut start_stack = vec![self.assert_next_str("/*")];

        loop {
            match self.chars.peek1() {
                Some('/') if self.chars.peek2() == Some('*') => {
                    start_stack.push(self.assert_next_str("/*"))
                }
                Some('*') if self.chars.peek2() == Some('/') => {
                    self.assert_next_str("*/");
                    start_stack.pop().unwrap();
                    if start_stack.is_empty() {
                        break Ok(());
                    }
                }

                Some(_) => {
                    self.chars.next();
                }
                None => {
                    break Err(Span::from_parts(
                        TokenError::UnterminatedComment,
                        start_stack.last().unwrap().clone(),
                    ))
                }
            }
        }
    }

    fn consume_single_digit(&mut self, digits: &str) -> Option<char> {
        if let Some(c) = self.chars.peek1().filter(|c| digits.contains(*c)) {
            self.chars.next();
            Some(c)
        } else {
            None
        }
    }
    fn expect_single_digit(&mut self, digits: &str) -> Result<char, TokenError> {
        self.consume_single_digit(digits)
            .ok_or(TokenError::ExpectedDigit)
    }

    fn consume_integer_literal(&mut self, digits: &str) -> Result<(), Span<TokenError>> {
        let start_pos = self.chars.current_pos();

        // A number literal always has at least one digit
        self.expect_single_digit(digits)
            .map_err(|error| Span::from_parts(error, start_pos..start_pos))?;

        // Now keep running until we have all the digits
        while self.consume_single_digit(digits).is_some() {
            // Just keep going
        }

        Ok(())
    }

    fn number_literal(&mut self) -> Result<Option<Span<Token>>, Span<TokenError>> {
        let start_pos = self.chars.current_pos();

        let start = match self.chars.peek1() {
            Some('0') => match self.chars.peek2() {
                Some('b') => Some((Some("0b"), NumberLiteralType::Binary, "01_")),
                Some('o') => Some((Some("0o"), NumberLiteralType::Octal, "01234567_")),
                Some('x') => Some((
                    Some("0x"),
                    NumberLiteralType::Hexadecimal,
                    "0123456789aAbBcCdDeEfF_",
                )),
                _ => Some((None, NumberLiteralType::Decimal, "0123456789_")),
            },
            Some('1'..='9') => Some((None, NumberLiteralType::Decimal, "0123456789_")),
            _ => None,
        };

        start
            .map(|(start_delimiter, mut ty, digits)| -> Result<_, _> {
                let digits_start_pos = if let Some(start_delimiter) = start_delimiter {
                    self.assert_next_str(start_delimiter).end
                } else {
                    start_pos
                };

                let can_be_float = ty == NumberLiteralType::Decimal;

                self.consume_integer_literal(digits)?;

                if can_be_float && matches!(self.chars.peek1(), Some('.')) {
                    self.chars.next();

                    // There is always a sequence of numbers after the decimal point "0." for example, is not allowed
                    self.consume_integer_literal(digits)?;

                    ty = NumberLiteralType::Float;
                }

                // Look for the exponent indicator
                if can_be_float && matches!(self.chars.peek1(), Some('e' | 'E')) {
                    self.chars.next();

                    // Skip over a + or - sign
                    if matches!(self.chars.peek1(), Some('+' | '-')) {
                        self.chars.next();
                    }

                    // There is always a sequence of numbers after the E
                    self.consume_integer_literal(digits)?;

                    ty = NumberLiteralType::Float;
                }

                let digits_end_pos = self.chars.current_pos();
                let digits = digits_start_pos..digits_end_pos;

                let suffix = self.consume_identifier().map(|span| span.to_parts().1);

                let end_pos = self.chars.current_pos();

                Ok(Span::from_parts(
                    Token::Number { digits, ty, suffix },
                    start_pos..end_pos,
                ))
            })
            .transpose()
    }

    fn string_literal(&mut self) -> Result<Option<Span<Token>>, Span<TokenError>> {
        fn raw_string_start(
            mut chars: CharReader,
            start_delimiter: &str,
            ty: CharLiteralType,
        ) -> Option<(CharLiteralType, String, char, usize)> {
            // This should already have been checked
            for ch in start_delimiter.chars() {
                assert_eq!(chars.next(), Some(ch));
            }

            let mut pound_count = 0_usize;
            while chars.peek1() == Some('#') {
                chars.next();
                pound_count += 1;
            }

            if chars.next() == Some('\"') {
                let start_delimiter =
                    start_delimiter.to_string() + &("#".repeat(pound_count)) + "\"";
                Some((ty, start_delimiter, '\"', pound_count))
            } else {
                None
            }
        }

        let start = match self.chars.peek1() {
            Some('\'') => Some((CharLiteralType::Char, "\'".to_string(), '\'', 0_usize)),
            Some('\"') => Some((CharLiteralType::String, "\"".to_string(), '\"', 0_usize)),

            Some('b') => match self.chars.peek2() {
                Some('\'') => Some((CharLiteralType::ByteChar, "b\'".to_string(), '\'', 0_usize)),
                Some('\"') => Some((
                    CharLiteralType::ByteString,
                    "b\"".to_string(),
                    '\"',
                    0_usize,
                )),
                Some('r') => {
                    raw_string_start(self.chars.clone(), "br", CharLiteralType::RawByteString)
                }

                _ => None,
            },

            Some('r') => raw_string_start(self.chars.clone(), "r", CharLiteralType::RawString),

            _ => None,
        };

        start
            .and_then(|(ty, start_delimiter, end_delimiter, pound_count)| {
                let start_delimiter_range = self.assert_next_str(&start_delimiter);
                let start_pos = start_delimiter_range.start;
                let content_start_pos = start_delimiter_range.end;

                loop {
                    let content_end_pos = self.chars.current_pos();

                    match self.chars.next() {
                        Some(c) if c == end_delimiter => {
                            let mut terminal_pound_count = 0_usize;
                            while terminal_pound_count < pound_count
                                && self.chars.peek1() == Some('#')
                            {
                                self.chars.next();
                                terminal_pound_count += 1;
                            }

                            if terminal_pound_count == pound_count {
                                break Some(Ok(Span::from_parts(
                                    Token::CharLiteral {
                                        contents: content_start_pos..content_end_pos,
                                        ty,
                                    },
                                    start_pos..self.chars.current_pos(),
                                )));
                            }
                        }

                        Some('\\') if !ty.is_raw() => {
                            self.chars.next();
                        } // Don't look at the next character
                        None => {
                            break Some(Err(Span::from_parts(
                                TokenError::ExpectedDelimiter(end_delimiter),
                                content_end_pos..content_end_pos,
                            )))
                        }

                        // Ignore everything else
                        _ => {}
                    }
                }
            })
            .transpose()
    }
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(str: &'a str) -> Self {
        Self {
            chars: CharReader::new(str),
            end_of_file: false,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Span<Token>, Span<TokenError>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        match self.next_token() {
            Ok(next_token) => {
                if *next_token.value() == Token::EndOfFile
                    && core::mem::replace(&mut self.end_of_file, true)
                {
                    None
                } else {
                    Some(Ok(next_token))
                }
            }

            Err(e) => Some(Err(e)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn check_tokens(source: &str, expected: impl AsRef<[Result<Span<Token>, Span<TokenError>>]>) {
        let mut actual = Lexer::from(source);
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

    #[test]
    fn test_empty_source() {
        check_tokens("", [Ok(Span::from_parts(Token::EndOfFile, 0..0))]);
    }

    #[test]
    fn test_whitespace_source() {
        check_tokens("   ", [Ok(Span::from_parts(Token::EndOfFile, 3..3))]);
        check_tokens("\t", [Ok(Span::from_parts(Token::EndOfFile, 1..1))]);
        check_tokens("\r\n", [Ok(Span::from_parts(Token::EndOfFile, 2..2))]);
    }

    #[test]
    fn single_line_comment() {
        check_tokens(
            "// booyah!!",
            [Ok(Span::from_parts(Token::EndOfFile, 11..11))],
        );
    }

    #[test]
    fn block_comment() {
        check_tokens(
            "/* booyah!!",
            [
                Err(Span::from_parts(TokenError::UnterminatedComment, 0..2)),
                Ok(Span::from_parts(Token::EndOfFile, 11..11)),
            ],
        );
        check_tokens(
            "/* booyah!! */",
            [Ok(Span::from_parts(Token::EndOfFile, 14..14))],
        );
        check_tokens(
            "/* /* booyah!!",
            [
                Err(Span::from_parts(TokenError::UnterminatedComment, 3..5)),
                Ok(Span::from_parts(Token::EndOfFile, 14..14)),
            ],
        );
        check_tokens(
            "/* /* booyah!!\n\n */",
            [
                Err(Span::from_parts(TokenError::UnterminatedComment, 0..2)),
                Ok(Span::from_parts(Token::EndOfFile, 19..19)),
            ],
        );
        check_tokens(
            "\n\n */",
            [
                Err(Span::from_parts(TokenError::UnexpectedEndOfComment, 3..5)),
                Ok(Span::from_parts(Token::EndOfFile, 5..5)),
            ],
        );
    }

    #[test]
    fn test_binary_literals() {
        check_tokens(
            "0b0",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..3,
                        ty: NumberLiteralType::Binary,
                        suffix: None,
                    },
                    0..3,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 3..3)),
            ],
        );
        check_tokens(
            "0b0_1_____0__1___hello",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..17,
                        ty: NumberLiteralType::Binary,
                        suffix: Some(17..22),
                    },
                    0..22,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 22..22)),
            ],
        );
        check_tokens(
            "0b",
            [
                Err(Span::from_parts(TokenError::ExpectedDigit, 2..2)),
                Ok(Span::from_parts(Token::EndOfFile, 2..2)),
            ],
        );
        check_tokens(
            "0b013",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..4,
                        ty: NumberLiteralType::Binary,
                        suffix: None,
                    },
                    0..4,
                )),
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 4..5,
                        ty: NumberLiteralType::Decimal,
                        suffix: None,
                    },
                    4..5,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 5..5)),
            ],
        );
    }

    #[test]
    fn test_octal_literals() {
        check_tokens(
            "0o0",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..3,
                        ty: NumberLiteralType::Octal,
                        suffix: None,
                    },
                    0..3,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 3..3)),
            ],
        );
        check_tokens(
            "0o0_1_____2__7___hello",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..17,
                        ty: NumberLiteralType::Octal,
                        suffix: Some(17..22),
                    },
                    0..22,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 22..22)),
            ],
        );
        check_tokens(
            "0o",
            [
                Err(Span::from_parts(TokenError::ExpectedDigit, 2..2)),
                Ok(Span::from_parts(Token::EndOfFile, 2..2)),
            ],
        );
        check_tokens(
            "0o018",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..4,
                        ty: NumberLiteralType::Octal,
                        suffix: None,
                    },
                    0..4,
                )),
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 4..5,
                        ty: NumberLiteralType::Decimal,
                        suffix: None,
                    },
                    4..5,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 5..5)),
            ],
        );
    }

    #[test]
    fn test_hexadecimal_literals() {
        check_tokens(
            "0x0",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..3,
                        ty: NumberLiteralType::Hexadecimal,
                        suffix: None,
                    },
                    0..3,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 3..3)),
            ],
        );
        check_tokens(
            "0x0_1_____a__E___hello",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..17,
                        ty: NumberLiteralType::Hexadecimal,
                        suffix: Some(17..22),
                    },
                    0..22,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 22..22)),
            ],
        );
        check_tokens(
            "0x",
            [
                Err(Span::from_parts(TokenError::ExpectedDigit, 2..2)),
                Ok(Span::from_parts(Token::EndOfFile, 2..2)),
            ],
        );
        check_tokens(
            "0x01G",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 2..4,
                        ty: NumberLiteralType::Hexadecimal,
                        suffix: Some(4..5),
                    },
                    0..5,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 5..5)),
            ],
        )
    }

    #[test]
    fn test_decimal_literals() {
        check_tokens(
            "0",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 0..1,
                        ty: NumberLiteralType::Decimal,
                        suffix: None,
                    },
                    0..1,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 1..1)),
            ],
        );
        check_tokens(
            "0_1_____2__3___hello",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 0..15,
                        ty: NumberLiteralType::Decimal,
                        suffix: Some(15..20),
                    },
                    0..20,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 20..20)),
            ],
        );
        check_tokens(
            "0_1_e-17___hello",
            [
                Ok(Span::from_parts(
                    Token::Number {
                        digits: 0..11,
                        ty: NumberLiteralType::Float,
                        suffix: Some(11..16),
                    },
                    0..16,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 16..16)),
            ],
        );
    }

    #[test]
    fn test_string_literals() {
        // This one is interesting. This is an empty char literal, which isn't really a thing, but the lexer doesn't care
        // about that - we just treat it the same as a string literal, but with quotes
        check_tokens(
            "\'\'",
            [
                Ok(Span::from_parts(
                    Token::CharLiteral {
                        contents: 1..1,
                        ty: CharLiteralType::Char,
                    },
                    0..2,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 2..2)),
            ],
        );
        check_tokens(
            "\'",
            [
                Err(Span::from_parts(TokenError::ExpectedDelimiter('\''), 1..1)),
                Ok(Span::from_parts(Token::EndOfFile, 1..1)),
            ],
        );
        check_tokens(
            r"b'abcde\''",
            [
                Ok(Span::from_parts(
                    Token::CharLiteral {
                        contents: 2..9,
                        ty: CharLiteralType::ByteChar,
                    },
                    0..10,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 10..10)),
            ],
        );
        check_tokens(
            "b\"hello\n   \"",
            [
                Ok(Span::from_parts(
                    Token::CharLiteral {
                        contents: 2..11,
                        ty: CharLiteralType::ByteString,
                    },
                    0..12,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 12..12)),
            ],
        );
        check_tokens(
            "r\"hello\\n   \"",
            [
                Ok(Span::from_parts(
                    Token::CharLiteral {
                        contents: 2..12,
                        ty: CharLiteralType::RawString,
                    },
                    0..13,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 13..13)),
            ],
        );
        check_tokens(
            "r##\"hello\\n   ",
            [
                Err(Span::from_parts(
                    TokenError::ExpectedDelimiter('\"'),
                    14..14,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 14..14)),
            ],
        );
        check_tokens(
            "r##\"hello\\n   \"",
            [
                Err(Span::from_parts(
                    TokenError::ExpectedDelimiter('\"'),
                    15..15,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 15..15)),
            ],
        );
        check_tokens(
            "r##\"hello\\n   \"#",
            [
                Err(Span::from_parts(
                    TokenError::ExpectedDelimiter('\"'),
                    16..16,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 16..16)),
            ],
        );
        check_tokens(
            "r##\"hello\\n   \"##",
            [
                Ok(Span::from_parts(
                    Token::CharLiteral {
                        contents: 4..14,
                        ty: CharLiteralType::RawString,
                    },
                    0..17,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 17..17)),
            ],
        );
        check_tokens(
            "br##\"hello\\n   \"###",
            [
                Ok(Span::from_parts(
                    Token::CharLiteral {
                        contents: 5..15,
                        ty: CharLiteralType::RawByteString,
                    },
                    0..18,
                )),
                Err(Span::from_parts(
                    TokenError::UnexpectedCharacter('#'),
                    18..19,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 19..19)),
            ],
        );
        check_tokens(
            "r\"hello\\n   \"",
            [
                Ok(Span::from_parts(
                    Token::CharLiteral {
                        contents: 2..12,
                        ty: CharLiteralType::RawString,
                    },
                    0..13,
                )),
                Ok(Span::from_parts(Token::EndOfFile, 13..13)),
            ],
        );
    }
}
