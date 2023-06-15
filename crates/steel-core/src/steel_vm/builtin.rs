use std::{borrow::Cow, rc::Rc, sync::Arc};

use crate::{
    containers::RegisterValue,
    parser::{ast::ExprKind, interner::InternedString, parser::SyntaxObject, tokens::TokenType},
    rvals::{Custom, FromSteelVal, FunctionSignature, IntoSteelVal, Result, SteelVal},
    values::functions::BoxedDynFunction,
};
use im_rc::HashMap;

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
/// structs. This should be more properly documented.
#[derive(Clone, Debug)]
#[repr(C)]
pub struct BuiltInModule {
    pub(crate) name: Rc<str>,
    values: HashMap<Arc<str>, SteelVal>,
    docs: Box<InternalDocumentation>,
    version: &'static str,
    // Add the metadata separate from the pointer, keeps the pointer slim
    fn_ptr_table: HashMap<*const FunctionSignature, FunctionSignatureMetadata>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// Probably need something more interesting than just an integer for the arity
pub struct FunctionSignatureMetadata {
    name: &'static str,
    arity: Arity,
}

impl FunctionSignatureMetadata {
    pub fn new(name: &'static str, arity: Arity) -> Self {
        Self { name, arity }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arity {
    Exact(usize),
    AtLeast(usize),
    AtMost(usize),
    Range(usize),
}

impl Custom for FunctionSignatureMetadata {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("{:?}", self)))
    }
}

impl Custom for BuiltInModule {}

impl RegisterValue for BuiltInModule {
    fn register_value_inner(&mut self, name: &str, value: SteelVal) -> &mut Self {
        self.values.insert(name.into(), value);
        self
    }
}

lazy_static::lazy_static! {
    pub static ref MODULE_GET: InternedString = "%module-get%".into();
    pub static ref VOID: InternedString = "void".into();
}

impl BuiltInModule {
    pub fn new<T: Into<Rc<str>>>(name: T) -> Self {
        Self {
            name: name.into(),
            values: HashMap::new(),
            docs: Box::new(InternalDocumentation::new()),
            version: env!("CARGO_PKG_VERSION"),
            fn_ptr_table: HashMap::new(),
        }
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name.into();
    }

    pub fn register_native_fn(
        &mut self,
        name: &'static str,
        func: fn(&[SteelVal]) -> Result<SteelVal>,
        arity: Arity,
    ) -> &mut Self {
        // Just automatically add it to the function pointer table to help out with searching
        self.add_to_fn_ptr_table(func, FunctionSignatureMetadata::new(name, arity));
        self.register_value(name, SteelVal::FuncV(func))
    }

    pub fn register_native_fn_with_doc(
        &mut self,
        name: &'static str,
        func: fn(&[SteelVal]) -> Result<SteelVal>,
        arity: Arity,
        doc: impl Into<Documentation<'static>>,
    ) -> &mut Self {
        self.register_native_fn(name, func, arity)
            .register_doc(name, doc)
    }

    pub fn check_compatibility(self: &BuiltInModule) -> bool {
        // self.version == env!("CARGO_PKG_VERSION")
        true
    }

    pub fn contains(&self, ident: &str) -> bool {
        self.values.contains_key(ident)
    }

    pub(crate) fn add_to_fn_ptr_table(
        &mut self,
        value: FunctionSignature,
        data: FunctionSignatureMetadata,
    ) -> &mut Self {
        self.fn_ptr_table
            .insert(value as *const FunctionSignature, data);

        self
    }

    pub fn search(&self, value: SteelVal) -> Option<FunctionSignatureMetadata> {
        // println!("{:?}", (*value as *const FunctionSignature) as usize);

        // println!("{:#?}", self.fn_ptr_table);

        if let SteelVal::FuncV(f) = value {
            self.fn_ptr_table
                .get(&(f as *const FunctionSignature))
                .cloned()
            // None
        } else {
            None
        }
    }

    pub fn bound_identifiers(&self) -> im_lists::list::List<SteelVal> {
        self.values
            .keys()
            .map(|x| SteelVal::StringV(x.to_string().into()))
            .collect()
    }

    pub fn with_module(mut self, module: BuiltInModule) -> Self {
        // self.values = self.values.union(module.values);

        self.values.extend(module.values.into_iter());

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
                Arc::new(f),
                Some(predicate_name),
                Some(1),
            ))),
        )
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<Cow<'static, str>>,
        description: impl Into<Documentation<'static>>,
    ) -> &mut Self {
        self.docs.register_doc(definition, description.into());
        self
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
        self.register_value_inner(name, value)
    }

    pub fn register_value_with_doc(
        &mut self,
        name: &'static str,
        value: SteelVal,
        doc: DocTemplate<'static>,
    ) -> &mut Self {
        self.values.insert(name.into(), value);
        self.register_doc(Cow::from(name), doc);
        self
    }

    // This _will_ panic given an incorrect value. This will be tied together by macros only allowing legal entries
    pub fn get(&self, name: String) -> SteelVal {
        self.values.get(name.as_str()).unwrap().clone()
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
                    .unwrap_or_else(|| x.to_string());

                ExprKind::Define(Box::new(crate::parser::ast::Define::new(
                    // TODO: Add the custom prefix here
                    // Handling a more complex case of qualifying imports
                    ExprKind::atom(name),
                    ExprKind::List(crate::parser::ast::List::new(vec![
                        ExprKind::atom(*MODULE_GET),
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
            ExprKind::atom(*MODULE_GET),
            ExprKind::atom("%-builtin-module-".to_string() + "steel/constants"),
            ExprKind::Quote(Box::new(crate::parser::ast::Quote::new(
                ExprKind::atom(*VOID),
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

// TODO: We're gonna need an FFI safe variant of this as well
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
