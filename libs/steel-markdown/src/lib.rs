use std::rc::Rc;

use abi_stable::std_types::RBoxError;
use steel::{
    rvals::{Custom, SerializableSteelVal},
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
};

use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag};

use syntect::highlighting::{Color, ThemeSet};
use syntect::{html::highlighted_html_for_string, parsing::SyntaxSet};

// fn main() {
// let ss = SyntaxSet::load_defaults_newlines();
// let ts = ThemeSet::load_defaults();

// let args: Vec<String> = std::env::args().collect();
// if args.len() < 2 {
//     println!("Please pass in a file to highlight");
//     return;
// }

// let style = "
//     pre {
//         font-size:13px;
//         font-family: Consolas, \"Liberation Mono\", Menlo, Courier, monospace;
//     }";
// println!("<head><title>{}</title><style>{}</style></head>", &args[1], style);
// let theme = &ts.themes["base16-ocean.dark"];
// let c = theme.settings.background.unwrap_or(Color::WHITE);
// println!("<body style=\"background-color:#{:02x}{:02x}{:02x};\">\n", c.r, c.g, c.b);
// let html = highlighted_html_for_file(&args[1], &ss, theme).unwrap();
// println!("{}", html);
// println!("</body>");
// }

struct SyntaxHighlighter {
    ss: SyntaxSet,
    ts: ThemeSet,
}

impl Custom for SyntaxHighlighter {}

#[derive(Debug)]
struct SyntectError(syntect::Error);
impl Custom for SyntectError {}

impl SyntaxHighlighter {
    pub fn new() -> Self {
        Self {
            ss: SyntaxSet::load_defaults_newlines(),
            ts: ThemeSet::load_defaults(),
        }
    }

    pub fn highlight_text(&self, language: String, input: String) -> Result<String, SyntectError> {
        // dbg!(self.ss.syntaxes());

        // for s in self.ss.syntaxes() {
        //     println!("{}", s.name);
        // }

        let reference = self.ss.find_syntax_by_name(&language).unwrap();

        highlighted_html_for_string(
            &input,
            &self.ss,
            reference,
            &self.ts.themes["base16-ocean.dark"],
        )
        .map_err(SyntectError)
    }
}

pub struct MarkdownEvent {
    event: Event<'static>,
    source: Rc<str>,
}

pub struct MarkdownTag {
    tag: Tag<'static>,
    source: Rc<str>,
}

impl Custom for MarkdownTag {}
impl Custom for MarkdownEvent {}

impl MarkdownEvent {
    // Convert this one event to html
    fn to_html(&self) -> String {
        let mut output = String::new();

        pulldown_cmark::html::push_html(&mut output, std::iter::once(self.event.clone()));

        output
    }

    fn is_text(&self) -> bool {
        matches!(self.event, Event::Text(_))
    }

    fn is_code(&self) -> bool {
        matches!(self.event, Event::Code(_))
    }

    fn set_code(&mut self, input: String) -> bool {
        if let Event::Code(ref mut text) = &mut self.event {
            *text = input.into();
            true
        } else {
            false
        }
    }

    fn is_html(&self) -> bool {
        matches!(self.event, Event::Html(_))
    }

    fn is_start(&self) -> bool {
        matches!(self.event, Event::Start(_))
    }

    fn is_end(&self) -> bool {
        matches!(self.event, Event::End(_))
    }

    fn set_text(&mut self, text: String) -> bool {
        if let Event::Text(t) = &mut self.event {
            *t = CowStr::from(text);
            true
        } else {
            false
        }
    }

    fn set_event_as_html(&mut self, html_text: String) {
        self.event = Event::Html(html_text.into());
    }

    fn as_text(&self) -> Option<String> {
        if let Event::Text(t) = &self.event {
            Some(t.to_string())
        } else {
            None
        }
    }

    fn as_code(&self) -> Option<String> {
        if let Event::Code(t) = &self.event {
            Some(t.to_string())
        } else {
            None
        }
    }

    fn as_html(&self) -> Option<String> {
        if let Event::Code(t) = &self.event {
            Some(t.to_string())
        } else {
            None
        }
    }

    fn as_start_tag(&self) -> Option<FFIValue> {
        if let Event::Start(tag) = self.event.clone() {
            Some(
                MarkdownTag {
                    tag,
                    source: self.source.clone(),
                }
                .into_ffi_val()
                .unwrap(),
            )
        } else {
            None
        }
    }

    fn as_end_tag(&self) -> Option<FFIValue> {
        if let Event::End(tag) = self.event.clone() {
            Some(
                MarkdownTag {
                    tag,
                    source: self.source.clone(),
                }
                .into_ffi_val()
                .unwrap(),
            )
        } else {
            None
        }
    }
}

struct MarkdownCodeBlockKind {
    _source: Rc<str>,
    kind: CodeBlockKind<'static>,
}

impl MarkdownCodeBlockKind {
    fn is_indented(&self) -> bool {
        matches!(self.kind, CodeBlockKind::Indented)
    }

    fn is_fenced(&self) -> bool {
        matches!(self.kind, CodeBlockKind::Fenced(..))
    }

    fn as_fenced(&self) -> Option<String> {
        if let CodeBlockKind::Fenced(label) = &self.kind {
            Some(label.to_string())
        } else {
            None
        }
    }
}

impl Custom for MarkdownCodeBlockKind {}

impl MarkdownTag {
    fn is_paragraph(&self) -> bool {
        matches!(self.tag, Tag::Paragraph)
    }

    fn is_heading(&self) -> bool {
        matches!(self.tag, Tag::Heading(..))
    }

    fn is_block_quote(&self) -> bool {
        matches!(self.tag, Tag::BlockQuote)
    }

    fn is_code_block(&self) -> bool {
        matches!(self.tag, Tag::CodeBlock(..))
    }

    fn as_heading(&self) -> Option<FFIValue> {
        if let Tag::Heading(level, fragment_id, classes) = &self.tag {
            Some(
                vec![
                    (*level as isize).into_ffi_val().unwrap(),
                    fragment_id.map(|x| x.to_string()).into_ffi_val().unwrap(),
                    classes
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .into_ffi_val()
                        .unwrap(),
                ]
                .into_ffi_val()
                .unwrap(),
            )
        } else {
            None
        }
    }

    fn as_code_block(&self) -> Option<FFIValue> {
        if let Tag::CodeBlock(kind) = &self.tag {
            Some(
                MarkdownCodeBlockKind {
                    _source: self.source.clone(),
                    kind: kind.clone(),
                }
                .into_ffi_val()
                .unwrap(),
            )
        } else {
            None
        }
    }

    fn is_list(&self) -> bool {
        matches!(self.tag, Tag::List(..))
    }

    fn is_foot_note_definition(&self) -> bool {
        matches!(self.tag, Tag::FootnoteDefinition(_))
    }

    fn is_table(&self) -> bool {
        matches!(self.tag, Tag::Table(..))
    }

    fn is_table_head(&self) -> bool {
        matches!(self.tag, Tag::TableHead)
    }

    fn is_table_row(&self) -> bool {
        matches!(self.tag, Tag::TableRow)
    }

    fn is_table_cell(&self) -> bool {
        matches!(self.tag, Tag::TableCell)
    }

    fn is_emphasis(&self) -> bool {
        matches!(self.tag, Tag::Emphasis)
    }

    fn is_strong(&self) -> bool {
        matches!(self.tag, Tag::Strong)
    }

    fn is_strike_through(&self) -> bool {
        matches!(self.tag, Tag::Strikethrough)
    }

    fn is_link(&self) -> bool {
        matches!(self.tag, Tag::Link(..))
    }

    fn is_image(&self) -> bool {
        matches!(self.tag, Tag::Image(..))
    }
}

struct MarkdownParser {
    source: Rc<str>,
    parser: Parser<'static, 'static>,
}

impl MarkdownParser {
    fn new(source: String) -> Self {
        let source: Rc<str> = Rc::from(source);

        Self {
            source: source.clone(),
            // SAFETY: We're going to lift this to a static lifetime
            // by calling to owned on each of the nodes, and keeping
            // the source location that we're referencing around for
            // the same lifetime as this object.
            parser: unsafe {
                std::mem::transmute::<Parser<'_, '_>, Parser<'static, 'static>>(Parser::new_ext(
                    &source,
                    Options::all(),
                ))
            },
        }
    }

    fn next(&mut self) -> Option<FFIValue> {
        self.parser.next().map(|event| {
            MarkdownEvent {
                event: event.to_owned(),
                source: self.source.clone(),
            }
            .into_ffi_val()
            .unwrap()
        })
    }
}

impl Custom for MarkdownParser {}

pub fn parse(input: String) -> Vec<FFIValue> {
    let input = Rc::from(input);

    Parser::new(&input)
        .map(|x| {
            // SAFETY: We're going to lift this to a static lifetime
            // by calling to owned on each of the nodes, and keeping
            // the source location that we're referencing around for
            // the same lifetime as this object.
            MarkdownEvent {
                event: unsafe { std::mem::transmute::<Event<'_>, Event<'static>>(x.to_owned()) },
                source: input.clone(),
            }
            .into_ffi_val()
            .unwrap()
        })
        .collect()
}

steel::declare_module!(build_module);

pub fn build_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/steel/markdown");

    module
        .register_fn("parse", parse)
        .register_fn("parser", MarkdownParser::new)
        .register_fn("parser-next", MarkdownParser::next)
        .register_fn("event->html-string", MarkdownEvent::to_html)
        .register_fn("event-text?", MarkdownEvent::is_text)
        .register_fn("event-code?", MarkdownEvent::is_code)
        .register_fn("set-event-code!", MarkdownEvent::set_code)
        .register_fn("event-html?", MarkdownEvent::is_html)
        .register_fn("event-start?", MarkdownEvent::is_start)
        .register_fn("event-end?", MarkdownEvent::is_end)
        .register_fn("event->text", MarkdownEvent::as_text)
        .register_fn("set-event-text!", MarkdownEvent::set_text)
        .register_fn("set-event-as-html!", MarkdownEvent::set_event_as_html)
        .register_fn("event->code", MarkdownEvent::as_code)
        .register_fn("event->html", MarkdownEvent::as_html)
        .register_fn("event->start-tag", MarkdownEvent::as_start_tag)
        .register_fn("event->end-tag", MarkdownEvent::as_end_tag)
        .register_fn("tag-paragraph?", MarkdownTag::is_paragraph)
        .register_fn("tag-heading?", MarkdownTag::is_heading)
        .register_fn("tag-block-quote?", MarkdownTag::is_block_quote)
        .register_fn("tag-code-block?", MarkdownTag::is_code_block)
        .register_fn("tag-list?", MarkdownTag::is_list)
        .register_fn(
            "tag-foot-note-definition?",
            MarkdownTag::is_foot_note_definition,
        )
        .register_fn("tag-table?", MarkdownTag::is_table)
        .register_fn("tag-table-head?", MarkdownTag::is_table_head)
        .register_fn("tag-table-row?", MarkdownTag::is_table_row)
        .register_fn("tag-table-cell?", MarkdownTag::is_table_cell)
        .register_fn("tag-emphasis?", MarkdownTag::is_emphasis)
        .register_fn("tag-strong?", MarkdownTag::is_strong)
        .register_fn("tag-strike-through?", MarkdownTag::is_strike_through)
        .register_fn("tag-link?", MarkdownTag::is_link)
        .register_fn("tag-image?", MarkdownTag::is_image)
        .register_fn("tag->heading", MarkdownTag::as_heading)
        .register_fn("tag->code-block", MarkdownTag::as_code_block)
        .register_fn("code-block-indented?", MarkdownCodeBlockKind::is_indented)
        .register_fn("code-block-fenced?", MarkdownCodeBlockKind::is_fenced)
        .register_fn("code-block->fenced", MarkdownCodeBlockKind::as_fenced)
        .register_fn("event->string", |event: &MarkdownEvent| -> String {
            format!("{:?}", event.event)
        })
        .register_fn("syntax-highlighter", SyntaxHighlighter::new)
        .register_fn(
            "syntax-highlighter/text->highlighted-html",
            SyntaxHighlighter::highlight_text,
        );
    module
}
