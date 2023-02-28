use std::{borrow::Cow, rc::Rc};

use crate::{
    parser::{ast::ExprKind, parser::SyntaxObject, tokens::TokenType},
    rvals::{Custom, FromSteelVal, FunctionSignature, IntoSteelVal, Result, SteelVal},
    values::functions::BoxedDynFunction,
};
use im_rc::{HashMap, OrdMap};

/// A module to be consumed by the Steel Engine for later on demand access by scripts
/// to refresh the primitives that are being used. For instance, the VM should have support
/// for a primitive like so, where "kernel" has functions like `list`, `cons`, `car`, and `cdr`:
///
/// ```scheme
/// (require-builtin "kernel")
/// ```
///
/// This would then expand into a series of function calls like so:
/// ```scheme
/// (begin
///     (define list (hash-get *--unreadable-module-name--kernel*))
///     (define cons (hash-get *--unreadable-module-name--kernel*))
///     (define car (hash-get *--unreadable-module-name--kernel*))
///     (define cdr (hash-get *--unreadable-module-name--kernel*)))
/// ```
/// So for now, its just a hashmap that will get embedded smartly, accompanied with a macro that will assist
/// with reserving the proper slots in the interner
///
/// TODO: @Matt - We run the risk of running into memory leaks here when exposing external mutable
/// structs.
#[derive(Clone, Debug)]
pub struct BuiltInModule {
    pub(crate) name: Rc<str>,
    values: OrdMap<String, SteelVal>,
    docs: InternalDocumentation,
    version: &'static str,
    // Add the metadata separate from the pointer, keeps the pointer slim
    fn_ptr_table: HashMap<*const FunctionSignature, FunctionSignatureMetadata>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// Probably need something more interesting than just an integer for the arity
struct FunctionSignatureMetadata {
    name: &'static str,
    arity: usize,
}

impl Custom for BuiltInModule {}

impl BuiltInModule {
    pub fn new(name: String) -> Self {
        Self {
            name: name.into(),
            values: OrdMap::new(),
            docs: InternalDocumentation::new(),
            version: env!("CARGO_PKG_VERSION"),
            fn_ptr_table: HashMap::new(),
        }
    }

    pub fn check_compatibility(self: &BuiltInModule) -> bool {
        self.version == env!("CARGO_PKG_VERSION")
    }

    pub fn contains(&self, ident: &str) -> bool {
        self.values.contains_key(ident)
    }

    pub fn bound_identifiers(&self) -> im_lists::list::List<SteelVal> {
        self.values
            .keys()
            .map(|x| SteelVal::StringV(x.into()))
            .collect()
    }

    pub fn with_module(mut self, module: BuiltInModule) -> Self {
        self.values = self.values.union(module.values);
        self.docs.definitions.extend(module.docs.definitions);
        self
    }

    pub fn register_type<T: FromSteelVal + IntoSteelVal>(
        &mut self,
        predicate_name: &'static str,
    ) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected 1 argument, got {}", predicate_name, args.len()));
            }

            assert!(args.len() == 1);

            Ok(SteelVal::BoolV(T::from_steelval(&args[0]).is_ok()))
        };

        self.register_value(
            predicate_name,
            SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
                Box::new(f),
                Some(predicate_name),
                Some(1),
            ))),
        )
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<Cow<'static, str>>,
        description: impl Into<Documentation<'static>>,
    ) {
        self.docs.register_doc(definition, description.into());
    }

    pub fn get_doc(&self, definition: String) {
        if let Some(value) = self.docs.get(&definition) {
            println!("{value}")
        }
    }

    pub(crate) fn unreadable_name(&self) -> String {
        "%-builtin-module-".to_string() + &self.name
    }

    /// Add a value to the module namespace. This value can be any legal SteelVal, or if you're explicitly attempting
    /// to compile an program for later use and don't currently have access to the functions in memory, use `SteelVal::Void`
    pub fn register_value(&mut self, name: &str, value: SteelVal) -> &mut Self {
        self.values.insert(name.to_string(), value);
        self
    }

    pub fn register_value_with_doc(
        &mut self,
        name: &'static str,
        value: SteelVal,
        doc: DocTemplate<'static>,
    ) -> &mut Self {
        self.values.insert(name.to_string(), value);
        self.register_doc(Cow::from(name), doc);
        self
    }

    // This _will_ panic given an incorrect value. This will be tied together by macros only allowing legal entries
    pub fn get(&self, name: String) -> SteelVal {
        self.values.get(&name).unwrap().clone()
    }

    /// This does the boot strapping for bundling modules
    /// Rather than expose a native hash-get, the built in module above should expose a raw
    /// function to fetch a dependency. It will be a packaged #<BuiltInModule> with only a function to
    /// fetch a function given its registered name. For instance:
    ///
    /// (##module-get## ##unreadable-module-name-core-lists## 'list)
    ///
    /// This puts the onus on the expansion of a primitive on the compiler, but now the language
    /// is bootstrapped via syntax-rules and the kernel macro expansion, and other dependencies
    /// are included via the usual macro expansion.
    ///
    /// In this way its always possible to refresh the native functions (and they don't disappear),
    /// and bundles of functions from third parties don't get included immediately into the global namespace.
    /// Scripts can choose to include these modules directly, or opt to not, and are not as risk of clobbering their
    /// global namespace.
    pub fn to_syntax(&self, prefix: Option<&str>) -> ExprKind {
        let module_name = self.unreadable_name();

        let mut defines = self
            .values
            .keys()
            .map(|x| {
                // TODO: Consider a custom delimeter as well
                // If we have a prefix, put the prefix at the front and append x
                // Otherwise, just default to using the provided name
                let name = prefix
                    .map(|pre| pre.to_string() + x)
                    .unwrap_or_else(|| x.to_owned());

                ExprKind::Define(Box::new(crate::parser::ast::Define::new(
                    // TODO: Add the custom prefix here
                    // Handling a more complex case of qualifying imports
                    ExprKind::atom(name),
                    ExprKind::List(crate::parser::ast::List::new(vec![
                        ExprKind::atom("%module-get%".to_string()),
                        ExprKind::atom(module_name.clone()),
                        ExprKind::Quote(Box::new(crate::parser::ast::Quote::new(
                            ExprKind::atom(x.to_string()),
                            SyntaxObject::default(TokenType::Quote),
                        ))),
                    ])),
                    SyntaxObject::default(TokenType::Define),
                )))
            })
            .collect::<Vec<_>>();

        defines.push(ExprKind::List(crate::parser::ast::List::new(vec![
            ExprKind::atom("%module-get%".to_string()),
            ExprKind::atom("%-builtin-module-".to_string() + "steel/constants"),
            ExprKind::Quote(Box::new(crate::parser::ast::Quote::new(
                ExprKind::atom("void".to_string()),
                SyntaxObject::default(TokenType::Quote),
            ))),
        ])));

        ExprKind::Begin(crate::parser::ast::Begin::new(
            defines,
            SyntaxObject::default(TokenType::Begin),
        ))
    }
}

/// Documentation representation
#[derive(Clone, Debug)]
pub struct InternalDocumentation {
    definitions: im_rc::HashMap<Cow<'static, str>, Documentation<'static>>,
}

impl InternalDocumentation {
    pub fn new() -> Self {
        Self {
            definitions: im_rc::HashMap::new(),
        }
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<Cow<'static, str>>,
        description: Documentation<'static>,
    ) {
        self.definitions.insert(definition.into(), description);
    }

    pub fn get(&self, definition: &str) -> Option<&Documentation<'static>> {
        self.definitions.get(definition)
    }
}

// pub(crate) const LAST_DOC: &'static str = r#"

// (last l) -> any/c

//     l : list?

// Returns the last element in the list.
// Takes time proportional to the length of the list.

// Example:
//     > (last (list 1 2 3 4))
//     4

// "#;

#[derive(Debug, Clone, PartialEq)]
pub enum Documentation<'a> {
    Function(DocTemplate<'a>),
    Module(ModuleDoc<'a>),
    Value(ValueDoc<'a>),
    Markdown(MarkdownDoc<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DocTemplate<'a> {
    pub signature: &'a str,
    pub params: &'a [&'a str],
    pub description: &'a str,
    pub examples: &'a [(&'a str, &'a str)],
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDoc<'a> {
    pub name: &'a str,
    pub description: &'a str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDoc<'a> {
    pub name: &'a str,
    pub description: &'a str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MarkdownDoc<'a>(pub &'a str);

impl<'a> std::fmt::Display for MarkdownDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", termimad::text(self.0))
    }
}

impl<'a> std::fmt::Display for Documentation<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Documentation::Function(d) => write!(f, "{d}"),
            Documentation::Module(d) => write!(f, "{d}"),
            Documentation::Value(d) => write!(f, "{d}"),
            Documentation::Markdown(d) => write!(f, "{d}"),
        }
    }
}

impl<'a> std::fmt::Display for DocTemplate<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        writeln!(f, "{}", self.signature)?;
        writeln!(f)?;

        for param in self.params {
            writeln!(f, "   {param}")?
        }

        writeln!(f)?;
        writeln!(f, "{}", self.description)?;
        writeln!(f)?;
        if !self.examples.is_empty() {
            writeln!(f, "Examples:")?;
            for (example, output) in self.examples {
                writeln!(f, "   {example}")?;
                writeln!(f, "   {output}")?;
            }
        }

        Ok(())
    }
}

impl<'a> std::fmt::Display for ValueDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        writeln!(f, "{}", self.name)?;
        writeln!(f)?;
        writeln!(f, "{}", self.description)
    }
}

impl<'a> std::fmt::Display for ModuleDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        writeln!(f, "{}", self.name)?;
        writeln!(f)?;
        writeln!(f, "{}", self.description)
    }
}

impl<'a> From<DocTemplate<'a>> for Documentation<'a> {
    fn from(val: DocTemplate<'a>) -> Self {
        Documentation::Function(val)
    }
}

impl<'a> From<ModuleDoc<'a>> for Documentation<'a> {
    fn from(val: ModuleDoc<'a>) -> Self {
        Documentation::Module(val)
    }
}

impl<'a> From<ValueDoc<'a>> for Documentation<'a> {
    fn from(val: ValueDoc<'a>) -> Self {
        Documentation::Value(val)
    }
}

impl<'a> From<MarkdownDoc<'a>> for Documentation<'a> {
    fn from(val: MarkdownDoc<'a>) -> Self {
        Documentation::Markdown(val)
    }
}

// #[test]
// fn check_output() {
//     println!("{}", LAST_DOC);
// }
