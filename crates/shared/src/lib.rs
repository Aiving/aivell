pub mod util;

use nom_locate::LocatedSpan;
use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Debug, Eq, Hash, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: usize,
}

impl Span {
    #[must_use]
    pub const fn between(&self, to: Self) -> Self {
        Self {
            start: self.start,
            end: to.end,
            line: self.line,
            column: self.column,
        }
    }

    pub const fn wrap<A>(self, value: A) -> Positioned<A> {
        Positioned { value, span: self }
    }
}

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl From<BytesSpan<'_>> for Span {
    fn from(value: BytesSpan) -> Self {
        Self {
            start: value.location_offset(),
            end: value.location_offset() + value.len(),
            line: value.location_line(),
            column: value.naive_get_utf8_column(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Positioned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Positioned<T> {
    pub const fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub const fn between<U>(&self, value: &Positioned<U>) -> Span {
        self.span.between(value.span)
    }

    pub const fn wrap<U>(&self, value: U) -> Positioned<U> {
        self.span.wrap(value)
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Positioned<U> {
        self.span.wrap(f(self.value))
    }

    pub fn into_span(self) -> Span {
        self.span
    }

    pub fn unpack(self) -> (Span, T) {
        (self.span, self.value)
    }
}

impl<T: Debug> Debug for Positioned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.value)
    }
}

pub type BytesSpan<'a> = LocatedSpan<&'a [u8]>;

// pub type RuntimeResult<T> = Result<T, RuntimeError>;
