use core::ops::Range;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::parser::SourceId;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Default)]
#[repr(C)]
pub struct Span {
    pub start: u32,
    pub end: u32,
    pub source_id: Option<SourceId>,
}

impl Span {
    #[inline]
    pub const fn new(start: u32, end: u32, source_id: Option<SourceId>) -> Self {
        Self {
            start,
            end,
            source_id,
        }
    }

    #[inline]
    pub const fn double(span: u32, source_id: Option<SourceId>) -> Self {
        Self {
            start: span,
            end: span,
            source_id,
        }
    }

    #[inline]
    pub const fn start(&self) -> u32 {
        self.start
    }

    #[inline]
    pub const fn end(&self) -> u32 {
        self.end
    }

    #[inline]
    pub const fn range(&self) -> Range<u32> {
        self.start..self.end
    }

    #[inline]
    pub const fn usize_range(&self) -> Range<usize> {
        self.start as _..self.end as _
    }

    #[inline]
    pub const fn source_id(&self) -> Option<SourceId> {
        self.source_id
    }

    #[inline]
    pub const fn merge(start: Self, end: Self) -> Span {
        // TODO: If this doesn't seem to make sense with macros, we can revisit
        Self::new(start.start, end.end, start.source_id)
    }

    #[inline]
    pub const fn width(&self) -> u32 {
        self.end - self.start
    }

    pub fn coalesce_span(spans: &[Span]) -> Span {
        let span = spans.first();
        if let Some(span) = span {
            let mut span = *span;
            for s in spans {
                if s.start() < span.start() {
                    span = Span::new(s.start(), span.end(), s.source_id);
                }
                if s.end() > span.end() {
                    span = Span::new(span.start(), s.end(), s.source_id);
                }
            }
            span
        } else {
            Span::new(0, 0, None)
        }
    }
}

impl fmt::Debug for Span {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Span> for Range<u32> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Span> for (u32, u32) {
    fn from(span: Span) -> Self {
        (span.start, span.end)
    }
}

impl From<Span> for [u32; 2] {
    fn from(span: Span) -> Self {
        [span.start, span.end]
    }
}
