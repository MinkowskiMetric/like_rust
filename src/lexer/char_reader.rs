use std::str::Chars;

#[derive(Clone)]
pub(super) struct CharReader<'a> {
    chars: Chars<'a>,
    original_length: usize,
}

impl<'a> CharReader<'a> {
    pub fn new(chars: &'a str) -> Self {
        let chars = chars.chars();
        let original_length = chars.as_str().len();

        Self {
            chars,
            original_length,
        }
    }

    pub fn current_pos(&self) -> usize {
        self.original_length - self.chars.as_str().len()
    }

    pub fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub fn peek1(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn peek2(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }
}
