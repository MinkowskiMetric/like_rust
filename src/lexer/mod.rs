mod char_reader;
mod token;
mod tokenizer;

pub(self) use char_reader::CharReader;

pub use token::{CharLiteralType, NumberLiteralType, Token, TokenError};
pub use tokenizer::Lexer;
