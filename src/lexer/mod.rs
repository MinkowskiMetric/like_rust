mod char_reader;
mod delimiter_checker;
mod token;
mod tokenizer;

#[cfg(test)]
pub(self) mod test_utils;

pub(self) use char_reader::CharReader;

pub use token::{DelimiterMode, DelimiterType, LiteralType, Token, TokenError};
pub use tokenizer::Lexer;
