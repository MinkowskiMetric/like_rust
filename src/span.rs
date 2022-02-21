use std::{fmt, hash::Hash, ops::Range};

pub struct Span<T> {
    t: T,
    range: Range<usize>,
}

impl<T> Span<T> {
    pub fn from_parts(t: T, range: Range<usize>) -> Self {
        Self { t, range }
    }

    pub fn to_parts(self) -> (T, Range<usize>) {
        (self.t, self.range)
    }

    pub fn parts(&self) -> (&T, &Range<usize>) {
        (&self.t, &self.range)
    }

    pub fn value(&self) -> &T {
        self.parts().0
    }

    pub fn range(&self) -> &Range<usize> {
        self.parts().1
    }
}

impl<T: Clone> Clone for Span<T> {
    fn clone(&self) -> Self {
        Self::from_parts(self.t.clone(), self.range.clone())
    }
}

impl<T: fmt::Debug> fmt::Debug for Span<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Span")
            .field(&self.t)
            .field(&self.range)
            .finish()
    }
}

impl<T: fmt::Display> fmt::Display for Span<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.t.fmt(f)
    }
}

impl<T: PartialEq> PartialEq for Span<T> {
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t && self.range == other.range
    }
}

impl<T: Hash> Hash for Span<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.t.hash(state);
        self.range.hash(state);
    }
}
