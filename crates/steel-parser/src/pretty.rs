use alloc::{borrow::Cow, boxed::Box, string::String, vec, vec::Vec};
use core::fmt;
use unicode_width::UnicodeWidthStr;

#[derive(Clone, Debug)]
pub enum Doc<'a> {
    Nil,
    Text(Cow<'a, str>),
    Line,
    Concat(Vec<Doc<'a>>),
    Nest(usize, Box<Doc<'a>>),
    Group(Box<Doc<'a>>),
}

type Command<'d, 'a> = (usize, bool, &'d Doc<'a>);

impl<'a> Doc<'a> {
    pub fn nil() -> Self {
        Doc::Nil
    }

    pub fn text(text: impl Into<Cow<'a, str>>) -> Self {
        Doc::Text(text.into())
    }

    pub fn space() -> Self {
        Doc::Text(Cow::Borrowed(" "))
    }

    /// A newline, or a space when the enclosing group fits on one line.
    pub fn line() -> Self {
        Doc::Line
    }

    pub fn append(self, that: Doc<'a>) -> Self {
        match (self, that) {
            (Doc::Nil, that) => that,
            (this, Doc::Nil) => this,
            (Doc::Concat(mut docs), that) => {
                docs.push(that);
                Doc::Concat(docs)
            }
            (this, that) => Doc::Concat(vec![this, that]),
        }
    }

    pub fn nest(self, by: usize) -> Self {
        Doc::Nest(by, Box::new(self))
    }

    /// Lays the document out on one line when it fits in the width.
    pub fn group(self) -> Self {
        Doc::Group(Box::new(self))
    }

    pub fn intersperse(docs: impl IntoIterator<Item = Doc<'a>>, separator: Doc<'a>) -> Self {
        let mut out = Vec::new();
        for doc in docs {
            if !out.is_empty() {
                out.push(separator.clone());
            }
            out.push(doc);
        }
        Doc::Concat(out)
    }

    pub fn pretty(&self, width: usize) -> String {
        let mut out = String::new();
        self.render(width, &mut out)
            .expect("writing to a string cannot fail");
        out
    }

    pub fn render<W: fmt::Write>(&self, width: usize, out: &mut W) -> fmt::Result {
        let mut column = 0;
        let mut pending = Vec::new();
        let mut commands: Vec<Command<'_, 'a>> = vec![(0, false, self)];

        while let Some((indent, flat, doc)) = commands.pop() {
            match doc {
                Doc::Nil => {}
                Doc::Text(text) => {
                    out.write_str(text)?;
                    column += text_width(text);
                }
                Doc::Line if flat => {
                    out.write_char(' ')?;
                    column += 1;
                }
                Doc::Line => {
                    // What follows the newline may sit at a different depth,
                    // and that is the depth to indent to
                    let indent = commands.last().map_or(indent, |next| next.0);
                    out.write_char('\n')?;
                    for _ in 0..indent {
                        out.write_char(' ')?;
                    }
                    column = indent;
                }
                Doc::Concat(docs) => {
                    commands.extend(docs.iter().rev().map(|doc| (indent, flat, doc)));
                }
                Doc::Nest(by, doc) => commands.push((indent.saturating_add(*by), flat, doc)),
                Doc::Group(doc) => {
                    let flat = flat || fits(doc, &commands, &mut pending, column, width);
                    commands.push((indent, flat, doc));
                }
            }
        }

        Ok(())
    }
}

// Whether `doc` laid out flat, and then whatever follows it up to the next
// newline, stays within the width.
fn fits<'d, 'a>(
    doc: &'d Doc<'a>,
    rest: &[Command<'d, 'a>],
    pending: &mut Vec<&'d Doc<'a>>,
    mut column: usize,
    width: usize,
) -> bool {
    let mut rest = rest.iter().rev();
    let mut flat = true;
    pending.clear();
    pending.push(doc);

    loop {
        let doc = match pending.pop() {
            Some(doc) => doc,
            None => match rest.next() {
                Some(command) => {
                    flat = false;
                    command.2
                }
                None => return true,
            },
        };

        match doc {
            Doc::Nil => {}
            Doc::Text(text) => {
                column += text_width(text);
                if column > width {
                    return false;
                }
            }
            Doc::Line if flat => {
                column += 1;
                if column > width {
                    return false;
                }
            }
            Doc::Line => return true,
            Doc::Concat(docs) => pending.extend(docs.iter().rev()),
            Doc::Nest(_, doc) | Doc::Group(doc) => pending.push(doc),
        }
    }
}

fn text_width(text: &str) -> usize {
    if text.is_ascii() {
        text.len()
    } else {
        UnicodeWidthStr::width(text)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn call<'a>(name: &'a str, args: &[&'a str]) -> Doc<'a> {
        let parts = core::iter::once(name)
            .chain(args.iter().copied())
            .map(Doc::text);

        Doc::text("(")
            .append(Doc::intersperse(parts, Doc::line()).nest(1).group())
            .append(Doc::text(")"))
    }

    #[test]
    fn a_group_that_fits_stays_on_one_line() {
        assert_eq!(call("list", &["a", "b", "c"]).pretty(12), "(list a b c)");
    }

    #[test]
    fn a_group_that_does_not_fit_breaks_every_line_in_it() {
        assert_eq!(
            call("list", &["a", "b", "c"]).pretty(11),
            "(list\n a\n b\n c)"
        );
    }

    #[test]
    fn what_follows_a_group_counts_toward_the_fit() {
        let doc = call("list", &["a"]).append(Doc::text("))))"));

        assert_eq!(doc.pretty(12), "(list a)))))");
        assert_eq!(doc.pretty(11), "(list\n a)))))");
    }

    #[test]
    fn an_inner_group_can_stay_flat_inside_a_broken_one() {
        let inner = call("f", &["x", "y"]);
        let doc = Doc::text("(define")
            .append(Doc::line())
            .append(Doc::text("name"))
            .append(Doc::line())
            .append(inner)
            .append(Doc::text(")"))
            .nest(2)
            .group();

        assert_eq!(doc.pretty(12), "(define\n  name\n  (f x y))");
    }

    #[test]
    fn a_newline_indents_to_the_depth_of_what_follows_it() {
        let doc = Doc::text("(")
            .append(Doc::text("a").append(Doc::line()).nest(4))
            .append(Doc::text("b)"));

        assert_eq!(doc.pretty(1), "(a\nb)");
    }

    #[test]
    fn width_counts_columns_and_not_bytes() {
        assert_eq!(call("λ", &["é", "ü"]).pretty(7), "(λ é ü)");
        assert_eq!(call("f", &["✅"]).pretty(6), "(f ✅)");
        assert_eq!(call("f", &["✅"]).pretty(5), "(f\n ✅)");
    }
}
