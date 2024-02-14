use std::{borrow::Cow, cell::RefCell, rc::Rc, sync::Arc};

use crate::{
    containers::RegisterValue,
    parser::{ast::ExprKind, interner::InternedString, parser::SyntaxObject, tokens::TokenType},
    rvals::{
        Custom, FromSteelVal, FunctionSignature, IntoSteelVal, MutFunctionSignature, Result,
        SteelVal, TypeKind,
    },
    values::functions::BoxedDynFunction,
};
use fxhash::FxBuildHasher;
use im_rc::HashMap;
use once_cell::sync::Lazy;

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
pub struct BuiltInModule {
    module: Rc<RefCell<BuiltInModuleRepr>>,
}

#[derive(Clone, Debug)]
struct BuiltInModuleRepr {
    pub(crate) name: Rc<str>,
    values: HashMap<Arc<str>, SteelVal, FxBuildHasher>,
    docs: Box<InternalDocumentation>,
    // Add the metadata separate from the pointer, keeps the pointer slim
    fn_ptr_table: HashMap<BuiltInFunctionTypePointer, FunctionSignatureMetadata>,
    // We don't need to generate this every time, just need to
    // clone it?
    generated_expression: RefCell<Option<ExprKind>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// Probably need something more interesting than just an integer for the arity
pub struct FunctionSignatureMetadata {
    pub name: &'static str,
    pub arity: Arity,
    pub is_const: bool,
    pub doc: Option<MarkdownDoc<'static>>,
}

impl FunctionSignatureMetadata {
    pub fn new(
        name: &'static str,
        arity: Arity,
        is_const: bool,
        doc: Option<MarkdownDoc<'static>>,
    ) -> Self {
        Self {
            name,
            arity,
            is_const,
            doc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arity {
    Exact(usize),
    AtLeast(usize),
    AtMost(usize),
    Range(usize),
}

impl Custom for Arity {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(match self {
            Arity::Exact(a) => format!("(Arity::Exact {a})"),
            Arity::AtLeast(a) => format!("(Arity::AtLeast {a})"),
            Arity::AtMost(a) => format!("(Arity::AtMost {a})"),
            Arity::Range(a) => format!("(Arity::Range {a})"),
        }))
    }
}

impl Custom for FunctionSignatureMetadata {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!(
            "(FunctionSignatureMetadata #:name {} #:arity {:?} #:const? {})",
            self.name, self.arity, self.is_const
        )))
    }
}

impl Custom for BuiltInModule {}

impl RegisterValue for BuiltInModule {
    #[track_caller]
    fn register_value_inner(&mut self, name: &str, value: SteelVal) -> &mut Self {
        let previous = self.module.borrow_mut().values.insert(name.into(), value);
        debug_assert!(previous.is_none(), "{name} already registered");
        self
    }
}

pub static MODULE_GET: Lazy<InternedString> = Lazy::new(|| "%module-get%".into());
pub static VOID: Lazy<InternedString> = Lazy::new(|| "void".into());
pub static GET_DYLIB: Lazy<InternedString> = Lazy::new(|| "#%get-dylib".into());
pub static VOID_MODULE: Lazy<InternedString> =
    Lazy::new(|| "%-builtin-module-steel/constants".into());

// Global function table
thread_local! {
    pub static FUNCTION_TABLE: RefCell<HashMap<BuiltInFunctionTypePointer, FunctionSignatureMetadata>> = RefCell::new(HashMap::new());
}

pub fn get_function_name(function: FunctionSignature) -> Option<FunctionSignatureMetadata> {
    FUNCTION_TABLE.with(|x| {
        x.borrow()
            .get(&BuiltInFunctionTypePointer::Reference(
                function as *const FunctionSignature,
            ))
            .cloned()
    })
}

pub fn get_function_metadata(
    function: BuiltInFunctionTypePointer,
) -> Option<FunctionSignatureMetadata> {
    FUNCTION_TABLE.with(|x| x.borrow().get(&function).cloned())
}

#[derive(Copy, Clone)]
pub enum BuiltInFunctionType {
    Reference(FunctionSignature),
    Mutable(MutFunctionSignature),
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum BuiltInFunctionTypePointer {
    Reference(*const FunctionSignature),
    Mutable(*const MutFunctionSignature),
}

pub struct NativeFunctionDefinition {
    pub name: &'static str,
    pub func: BuiltInFunctionType,
    pub arity: Arity,
    pub doc: Option<MarkdownDoc<'static>>,
    pub is_const: bool,
    pub signature: Option<(&'static [TypeKind], TypeKind)>,
}

impl BuiltInModuleRepr {
    pub fn new<T: Into<Rc<str>>>(name: T) -> Self {
        Self {
            name: name.into(),
            values: HashMap::default(),
            docs: Box::new(InternalDocumentation::new()),
            fn_ptr_table: HashMap::new(),
            generated_expression: RefCell::new(None),
        }
    }

    pub fn contains(&self, ident: &str) -> bool {
        self.values.contains_key(ident)
    }

    pub(crate) fn add_to_fn_ptr_table(
        &mut self,
        value: BuiltInFunctionType,
        data: FunctionSignatureMetadata,
    ) -> &mut Self {
        match value {
            BuiltInFunctionType::Reference(value) => {
                // Store this in a globally accessible place for printing
                FUNCTION_TABLE.with(|table| {
                    table.borrow_mut().insert(
                        BuiltInFunctionTypePointer::Reference(value as *const FunctionSignature),
                        data.clone(),
                    )
                });

                // Probably don't need to store it in both places?
                self.fn_ptr_table.insert(
                    BuiltInFunctionTypePointer::Reference(value as *const FunctionSignature),
                    data,
                );
            }

            BuiltInFunctionType::Mutable(value) => {
                FUNCTION_TABLE.with(|table| {
                    table.borrow_mut().insert(
                        BuiltInFunctionTypePointer::Mutable(value as *const MutFunctionSignature),
                        data.clone(),
                    )
                });

                self.fn_ptr_table.insert(
                    BuiltInFunctionTypePointer::Mutable(value as *const MutFunctionSignature),
                    data,
                );
            }
        }

        self
    }

    pub fn search_by_name(&self, name: &str) -> Option<FunctionSignatureMetadata> {
        self.values.get(name).and_then(|x| self.search(x.clone()))
    }

    pub fn search(&self, value: SteelVal) -> Option<FunctionSignatureMetadata> {
        match value {
            SteelVal::FuncV(f) => self
                .fn_ptr_table
                .get(&BuiltInFunctionTypePointer::Reference(
                    f as *const FunctionSignature,
                ))
                .cloned(),
            SteelVal::MutFunc(f) => self
                .fn_ptr_table
                .get(&BuiltInFunctionTypePointer::Mutable(
                    f as *const MutFunctionSignature,
                ))
                .cloned(),
            _ => None,
        }
    }

    pub fn names(&self) -> Vec<String> {
        self.values.keys().map(|x| x.to_string()).collect()
    }

    pub fn bound_identifiers(&self) -> crate::values::lists::List<SteelVal> {
        self.values
            .keys()
            .map(|x| SteelVal::StringV(x.to_string().into()))
            .collect()
    }

    pub fn with_module(&mut self, module: BuiltInModule) {
        self.values = std::mem::take(&mut self.values).union(module.module.borrow().values.clone());

        self.docs
            .definitions
            .extend(module.module.borrow().docs.definitions.clone());
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

    pub fn get_documentation(&self, definition: &str) -> Option<String> {
        self.docs.get(definition).map(|x| x.to_string())
    }

    pub(crate) fn unreadable_name(&self) -> String {
        "%-builtin-module-".to_string() + &self.name
    }

    /// Add a value to the module namespace. This value can be any legal SteelVal, or if you're explicitly attempting
    /// to compile an program for later use and don't currently have access to the functions in memory, use `SteelVal::Void`
    pub fn register_value(&mut self, name: &str, value: SteelVal) -> &mut Self {
        self.values.insert(name.into(), value);
        self
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
        match self.values.get(name.as_str()) {
            Some(v) => v.clone(),
            None => {
                let known_symbols = self.values.keys().map(Arc::as_ref);
                match find_closest_match(&name, known_symbols) {
                    Some(closest_match) => panic!(
                        "symbol {name:?} not found in module {module_name}, did you mean {closest_match:?}?",
                        module_name = self.name
                    ),
                    None => panic!(
                        "symbol {name:?} not found in module {module_name}",
                        module_name = self.name
                    ),
                }
            }
        }
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
        // log::debug!(target: "engine-creation", "{:p}, Creating module: {} - Prefix: {:?} - cached: {}", self, self.name, prefix, self.generated_expression.borrow().is_some());

        // No need to generate this module multiple times -
        if prefix.is_none() && self.generated_expression.borrow().is_some() {
            return self.generated_expression.borrow().as_ref().unwrap().clone();
        }

        let module_name = self.unreadable_name();
        let module_name_expr = ExprKind::atom(module_name.clone());

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
                        module_name_expr.clone(),
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
            ExprKind::atom(*VOID_MODULE),
            ExprKind::Quote(Box::new(crate::parser::ast::Quote::new(
                ExprKind::atom(*VOID),
                SyntaxObject::default(TokenType::Quote),
            ))),
        ])));

        let res = ExprKind::Begin(crate::parser::ast::Begin::new(
            defines,
            SyntaxObject::default(TokenType::Begin),
        ));

        // Cache the generated expression
        if prefix.is_none() && self.generated_expression.borrow().is_none() {
            *self.generated_expression.borrow_mut() = Some(res.clone());
        }

        // log::debug!(target: "engine-creation", "Generating expression for: {} took: {:?}", self.name, now.elapsed());

        res
    }
}

impl BuiltInModule {
    pub fn new<T: Into<Rc<str>>>(name: T) -> Self {
        Self {
            module: Rc::new(RefCell::new(BuiltInModuleRepr::new(name))),
        }
    }

    pub fn names(&self) -> Vec<String> {
        self.module.borrow().names()
    }

    pub fn name(&self) -> Rc<str> {
        Rc::clone(&self.module.borrow().name)
    }

    pub fn documentation(&self) -> std::cell::Ref<'_, InternalDocumentation> {
        std::cell::Ref::map(self.module.borrow(), |x| x.docs.as_ref())
    }

    // pub fn set_name(&mut self, name: String) {
    //     self.name = name.into();
    // }

    pub fn register_native_fn(
        &mut self,
        name: &'static str,
        func: fn(&[SteelVal]) -> Result<SteelVal>,
        arity: Arity,
    ) -> &mut Self {
        // Just automatically add it to the function pointer table to help out with searching
        self.add_to_fn_ptr_table(
            BuiltInFunctionType::Reference(func),
            FunctionSignatureMetadata::new(name, arity, false, None),
        );
        self.register_value(name, SteelVal::FuncV(func))
    }

    pub fn register_native_fn_definition(
        &mut self,
        definition: NativeFunctionDefinition,
    ) -> &mut Self {
        self.add_to_fn_ptr_table(
            definition.func,
            FunctionSignatureMetadata::new(
                definition.name,
                definition.arity,
                definition.is_const,
                definition.doc.clone(),
            ),
        );

        if let Some(doc) = definition.doc {
            self.register_doc(definition.name, doc);
        }

        match definition.func {
            BuiltInFunctionType::Reference(value) => {
                self.register_value(definition.name, SteelVal::FuncV(value));
            }
            BuiltInFunctionType::Mutable(value) => {
                self.register_value(definition.name, SteelVal::MutFunc(value));
            }
        }

        self
    }

    pub fn check_compatibility(self: &BuiltInModule) -> bool {
        // self.version == env!("CARGO_PKG_VERSION")
        true
    }

    pub fn contains(&self, ident: &str) -> bool {
        // self.values.contains_key(ident)
        self.module.borrow().contains(ident)
    }

    pub(crate) fn add_to_fn_ptr_table(
        &mut self,
        value: BuiltInFunctionType,
        data: FunctionSignatureMetadata,
    ) -> &mut Self {
        self.module.borrow_mut().add_to_fn_ptr_table(value, data);

        self
    }

    pub fn search(&self, value: SteelVal) -> Option<FunctionSignatureMetadata> {
        self.module.borrow().search(value)
    }

    pub fn search_by_name(&self, name: &str) -> Option<FunctionSignatureMetadata> {
        self.module.borrow().search_by_name(name)
    }

    pub fn bound_identifiers(&self) -> crate::values::lists::List<SteelVal> {
        // self.values
        //     .keys()
        //     .map(|x| SteelVal::StringV(x.to_string().into()))
        //     .collect()

        self.module.borrow().bound_identifiers()
    }

    pub fn with_module(self, module: BuiltInModule) -> Self {
        // self.values.extend(module.values.into_iter());

        // self.docs.definitions.extend(module.docs.definitions);
        // self

        self.module.borrow_mut().with_module(module);
        self
    }

    pub fn register_type<T: FromSteelVal + IntoSteelVal>(
        &mut self,
        predicate_name: &'static str,
    ) -> &mut Self {
        self.module.borrow_mut().register_type::<T>(predicate_name);
        self
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<Cow<'static, str>>,
        description: impl Into<Documentation<'static>>,
    ) -> &mut Self {
        // self.docs.register_doc(definition, description.into());
        // self

        self.module
            .borrow_mut()
            .register_doc(definition, description);
        self
    }

    // pub fn docs(&self) ->

    pub fn get_doc(&self, definition: String) {
        // if let Some(value) = self.docs.get(&definition) {
        //     println!("{value}")
        // }

        self.module.borrow().get_doc(definition);
    }

    pub fn get_documentation(&self, definition: &str) -> Option<String> {
        self.module.borrow().get_documentation(definition)
    }

    pub(crate) fn unreadable_name(&self) -> String {
        "%-builtin-module-".to_string() + &self.module.borrow().name
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
        // self.values.insert(name.into(), value);
        // self.register_doc(Cow::from(name), doc);
        // self

        self.module
            .borrow_mut()
            .register_value_with_doc(name, value, doc);
        self
    }

    // This _will_ panic given an incorrect value. This will be tied together by macros only allowing legal entries
    pub fn get(&self, name: String) -> SteelVal {
        self.module.borrow().get(name)
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
        self.module.borrow().to_syntax(prefix)
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

    pub fn definitions(&self) -> &im_rc::HashMap<Cow<'static, str>, Documentation<'static>> {
        &self.definitions
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MarkdownDoc<'a>(pub &'a str);

impl<'a> std::fmt::Display for MarkdownDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(feature = "markdown")]
        return write!(f, "{}", termimad::text(self.0));

        #[cfg(not(feature = "markdown"))]
        return write!(f, "{}", self.0);
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

pub(crate) fn find_closest_match<'a>(
    target: &str,
    candidates: impl IntoIterator<Item = &'a str>,
) -> Option<&'a str> {
    candidates
        .into_iter()
        .map(|candidate| (strsim::normalized_levenshtein(target, candidate), candidate))
        .filter(|(sim, _)| *sim > 0.8)
        // The key must be converted to a type that implements std::cmp::Ord.
        .max_by_key(|(sim, _)| (*sim * 100.0) as usize)
        .map(|(_, s)| s)
}
