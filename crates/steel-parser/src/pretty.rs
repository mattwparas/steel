use alloc::string::String;
use alloc::vec::Vec;
use core::{fmt, marker::PhantomData};

pub struct RcDoc<'a, A> {
    buffer: String,
    _marker: PhantomData<&'a A>,
}

impl<'a, A> Clone for RcDoc<'a, A> {
    fn clone(&self) -> Self {
        Self { buffer: self.buffer.clone(), _marker: PhantomData }
    }
}

impl<'a, A> fmt::Debug for RcDoc<'a, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RcDoc").field("buffer", &self.buffer).finish()
    }
}

impl<'a, A> RcDoc<'a, A> {
    pub fn text<T: Into<String>>(text: T) -> Self {
        Self {
            buffer: text.into(),
            _marker: PhantomData,
        }
    }

    pub fn space() -> Self {
        Self::text(" ")
    }

    pub fn line() -> Self {
        Self::text("\n")
    }

    pub fn append(mut self, other: RcDoc<'a, A>) -> Self {
        self.buffer.push_str(&other.buffer);
        self
    }

    pub fn nest(self, _indent: usize) -> Self {
        self
    }

    pub fn group(self) -> Self {
        self
    }

    pub fn intersperse<I>(iter: I, sep: RcDoc<'a, A>) -> RcDoc<'a, A>
    where
        I: IntoIterator<Item = RcDoc<'a, A>>,
    {
        let mut iter = iter.into_iter();
        let mut doc = if let Some(first) = iter.next() {
            first
        } else {
            return RcDoc::text("");
        };

        for item in iter {
            doc = doc.append(sep.clone()).append(item);
        }

        doc
    }

    pub fn to_string(&self) -> String {
        self.buffer.clone()
    }

    pub fn into_string(self) -> String {
        self.buffer
    }
}

impl<'a, A> RcDoc<'a, A> {
    pub fn render(&self, _width: usize, out: &mut Vec<u8>) -> Result<(), ()> {
        out.extend_from_slice(self.buffer.as_bytes());
        Ok(())
    }
}
