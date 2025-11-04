use std::{borrow::Cow, cell::RefCell, sync::Arc};

use crate::gc::shared::{MappedScopedReadContainer, MutContainer, ScopedReadContainer};

// #[cfg(not(feature = "triomphe"))]
// use crate::gc::shared::ShareableMut;

#[cfg(not(feature = "sync"))]
use crate::gc::shared::ShareableMut;

use crate::gc::{Shared, SharedMut};
use crate::values::HashMap;
use crate::{
    containers::RegisterValue,
    gc::Gc,
    parser::{ast::ExprKind, interner::InternedString, parser::SyntaxObject, tokens::TokenType},
    rvals::{
        Custom, FromSteelVal, FunctionSignature, IntoSteelVal, MutFunctionSignature, Result,
        SteelVal, TypeKind,
    },
    values::functions::BoxedDynFunction,
};
use compact_str::CompactString;
use fxhash::FxBuildHasher;
use once_cell::sync::Lazy;

#[cfg(feature = "sync")]
use parking_lot::RwLock;

use super::vm::BuiltInSignature;

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
#[derive(Clone)]
pub struct BuiltInModule {
    pub(crate) module: SharedMut<BuiltInModuleRepr>,
}

#[derive(Clone)]
pub(crate) struct BuiltInModuleRepr {
    pub(crate) name: Shared<str>,
    pub(crate) values: std::collections::HashMap<Arc<str>, SteelVal, FxBuildHasher>,
    docs: Box<InternalDocumentation>,
    // Add the metadata separate from the pointer, keeps the pointer slim
    fn_ptr_table: std::collections::HashMap<BuiltInFunctionType, FunctionSignatureMetadata>,
    // We don't need to generate this every time, just need to
    // clone it?
    generated_expression: SharedMut<Option<ExprKind>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// Probably need something more interesting than just an integer for the arity
pub struct FunctionSignatureMetadata {
    pub name: Cow<'static, str>,
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
            name: Cow::Borrowed(name),
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
    Range(usize, usize),
}

impl Custom for Arity {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(match self {
            Arity::Exact(a) => format!("(Arity::Exact {a})"),
            Arity::AtLeast(a) => format!("(Arity::AtLeast {a})"),
            Arity::AtMost(a) => format!("(Arity::AtMost {a})"),
            Arity::Range(a, b) => format!("(Arity::Range [{a},{b}])"),
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

impl Custom for BuiltInModule {
    fn into_serializable_steelval(&mut self) -> Option<crate::rvals::SerializableSteelVal> {
        Some(crate::rvals::SerializableSteelVal::Custom(Box::new(
            self.clone(),
        )))
    }
}

impl RegisterValue for BuiltInModule {
    fn register_value_inner(&mut self, name: &str, value: SteelVal) -> &mut Self {
        self.module.write().values.insert(name.into(), value);
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
    pub static FUNCTION_TABLE: RefCell<HashMap<BuiltInFunctionType, FunctionSignatureMetadata>> = RefCell::new(HashMap::new());
}

#[cfg(feature = "sync")]
pub static STATIC_FUNCTION_TABLE: Lazy<
    RwLock<HashMap<BuiltInFunctionType, FunctionSignatureMetadata>>,
> = Lazy::new(|| RwLock::new(HashMap::new()));

pub fn get_function_name(function: FunctionSignature) -> Option<FunctionSignatureMetadata> {
    #[cfg(feature = "sync")]
    {
        STATIC_FUNCTION_TABLE
            .read()
            .get(&BuiltInFunctionType::Reference(function))
            .cloned()
    }
    #[cfg(not(feature = "sync"))]
    {
        FUNCTION_TABLE.with(|x| {
            x.borrow()
                .get(&BuiltInFunctionType::Reference(function))
                .cloned()
        })
    }
}

pub fn get_function_metadata(function: BuiltInFunctionType) -> Option<FunctionSignatureMetadata> {
    #[cfg(feature = "sync")]
    {
        STATIC_FUNCTION_TABLE.read().get(&function).cloned()
    }

    #[cfg(not(feature = "sync"))]
    {
        FUNCTION_TABLE.with(|x| x.borrow().get(&function).cloned())
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
#[allow(unpredictable_function_pointer_comparisons)]
pub enum BuiltInFunctionType {
    Reference(FunctionSignature),
    Mutable(MutFunctionSignature),
    Context(BuiltInSignature),
}

pub struct NativeFunctionDefinition {
    pub name: &'static str,
    pub aliases: &'static [&'static str],
    pub func: BuiltInFunctionType,
    pub arity: Arity,
    pub doc: Option<MarkdownDoc<'static>>,
    pub is_const: bool,
    pub signature: Option<(&'static [TypeKind], TypeKind)>,
}

impl BuiltInModuleRepr {
    pub fn new<T: Into<Shared<str>>>(name: T) -> Self {
        Self {
            name: name.into(),
            values: std::collections::HashMap::default(),
            docs: Box::new(InternalDocumentation::new()),
            fn_ptr_table: std::collections::HashMap::new(),
            generated_expression: Shared::new(MutContainer::new(None)),
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
                #[cfg(feature = "sync")]
                {
                    STATIC_FUNCTION_TABLE
                        .write()
                        .insert(BuiltInFunctionType::Reference(value), data.clone());
                }
                #[cfg(not(feature = "sync"))]
                {
                    // Store this in a globally accessible place for printing
                    FUNCTION_TABLE.with(|table| {
                        table
                            .borrow_mut()
                            .insert(BuiltInFunctionType::Reference(value), data.clone())
                    });
                }

                // Probably don't need to store it in both places?
                self.fn_ptr_table
                    .insert(BuiltInFunctionType::Reference(value), data);
            }
            BuiltInFunctionType::Mutable(value) => {
                #[cfg(feature = "sync")]
                {
                    STATIC_FUNCTION_TABLE
                        .write()
                        .insert(BuiltInFunctionType::Mutable(value), data.clone());
                }
                #[cfg(not(feature = "sync"))]
                {
                    FUNCTION_TABLE.with(|table| {
                        table
                            .borrow_mut()
                            .insert(BuiltInFunctionType::Mutable(value), data.clone())
                    });
                }

                self.fn_ptr_table
                    .insert(BuiltInFunctionType::Mutable(value), data);
            }
            BuiltInFunctionType::Context(value) => {
                #[cfg(feature = "sync")]
                {
                    STATIC_FUNCTION_TABLE
                        .write()
                        .insert(BuiltInFunctionType::Context(value), data.clone());
                }
                #[cfg(not(feature = "sync"))]
                {
                    FUNCTION_TABLE.with(|table| {
                        table
                            .borrow_mut()
                            .insert(BuiltInFunctionType::Context(value), data.clone())
                    });
                }

                self.fn_ptr_table
                    .insert(BuiltInFunctionType::Context(value), data);
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
                .get(&BuiltInFunctionType::Reference(f))
                .cloned(),
            SteelVal::MutFunc(f) => self
                .fn_ptr_table
                .get(&BuiltInFunctionType::Mutable(f))
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
        // self.values = std::mem::take(&mut self.values).union(module.module.read().values.clone());

        self.values.extend(
            module
                .module
                .read()
                .values
                .iter()
                .map(|x| (x.0.clone(), x.1.clone())),
        );

        // TODO: This almost assuredly, is not necessary, right? We could instead just use
        // the global metadata table and get rid of the vast majority of this information.
        // self.fn_ptr_table =
        //     std::mem::take(&mut self.fn_ptr_table).union(module.module.read().fn_ptr_table.clone());

        self.fn_ptr_table.extend(
            module
                .module
                .read()
                .fn_ptr_table
                .iter()
                .map(|x| (*x.0, x.1.clone())),
        );

        self.docs.definitions.extend(
            module
                .module
                .read()
                .docs
                .definitions
                .iter()
                .map(|x| (x.0.clone(), x.1.clone())),
        );
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
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
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
        self.docs
            .get(definition)
            .and_then(|x| x.as_inner_str().map(|x| x.to_owned()))
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

    pub fn try_get(&self, name: String) -> Option<SteelVal> {
        self.values.get(name.as_str()).cloned()
    }

    pub fn try_get_ref(&self, name: &str) -> Option<SteelVal> {
        self.values.get(name).cloned()
    }

    pub(crate) fn cached_expression(&self) -> SharedMut<Option<ExprKind>> {
        self.generated_expression.clone()
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
        if prefix.is_none() && self.generated_expression.read().is_some() {
            return self.generated_expression.read().as_ref().unwrap().clone();
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

        let res = ExprKind::Begin(Box::new(crate::parser::ast::Begin::new(
            defines,
            SyntaxObject::default(TokenType::Begin),
        )));

        // Cache the generated expression
        if prefix.is_none() && self.generated_expression.read().is_none() {
            *self.generated_expression.write() = Some(res.clone());
        }

        // log::debug!(target: "engine-creation", "Generating expression for: {} took: {:?}", self.name, now.elapsed());

        res
    }
}

impl BuiltInModule {
    pub fn new<T: Into<Shared<str>>>(name: T) -> Self {
        Self {
            module: Shared::new(MutContainer::new(BuiltInModuleRepr::new(name))),
        }
    }

    pub(crate) fn cached_expression(&self) -> SharedMut<Option<ExprKind>> {
        self.module.read().cached_expression()
    }

    pub(crate) fn constant_funcs(
        &self,
    ) -> crate::values::HashMap<InternedString, SteelVal, FxBuildHasher> {
        self.module
            .read()
            .fn_ptr_table
            .iter()
            .filter_map(|(key, value)| match key {
                BuiltInFunctionType::Reference(func) if value.is_const => Some((
                    (CompactString::new("#%prim.") + value.name.as_ref()).into(),
                    SteelVal::FuncV(*func),
                )),
                BuiltInFunctionType::Mutable(func) if value.is_const => Some((
                    (CompactString::new("#%prim.") + value.name.as_ref()).into(),
                    SteelVal::MutFunc(*func),
                )),
                _ => None,
            })
            .collect()
    }

    pub fn names(&self) -> Vec<String> {
        self.module.read().names()
    }

    pub(crate) fn inner_map(
        &self,
    ) -> MappedScopedReadContainer<'_, std::collections::HashMap<Arc<str>, SteelVal, FxBuildHasher>>
    {
        ScopedReadContainer::map(self.module.read(), |x| &x.values)
    }

    pub fn name(&self) -> Shared<str> {
        Shared::clone(&self.module.read().name)
    }

    // pub fn documentation(&self) -> std::cell::Ref<'_, InternalDocumentation> {
    //     std::cell::Ref::map(self.module.read(), |x| x.docs.as_ref())
    // }

    pub fn documentation(&self) -> MappedScopedReadContainer<'_, InternalDocumentation> {
        ScopedReadContainer::map(self.module.read(), |x| x.docs.as_ref())
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

            for alias in definition.aliases {
                self.register_doc(
                    *alias,
                    MarkdownDoc(format!("Alias of `{}`.", definition.name).into()),
                );
            }
        }

        let steel_val = match definition.func {
            BuiltInFunctionType::Reference(value) => SteelVal::FuncV(value),
            BuiltInFunctionType::Mutable(value) => SteelVal::MutFunc(value),
            BuiltInFunctionType::Context(value) => SteelVal::BuiltIn(value),
        };

        let names = std::iter::once(definition.name).chain(definition.aliases.iter().cloned());

        for name in names {
            self.register_value(name, steel_val.clone());
        }

        self
    }

    pub fn check_compatibility(self: &BuiltInModule) -> bool {
        // self.version == env!("CARGO_PKG_VERSION")
        true
    }

    pub fn contains(&self, ident: &str) -> bool {
        // self.values.contains_key(ident)
        self.module.read().contains(ident)
    }

    pub(crate) fn add_to_fn_ptr_table(
        &mut self,
        value: BuiltInFunctionType,
        data: FunctionSignatureMetadata,
    ) -> &mut Self {
        self.module.write().add_to_fn_ptr_table(value, data);

        self
    }

    pub fn search(&self, value: SteelVal) -> Option<FunctionSignatureMetadata> {
        self.module.read().search(value)
    }

    pub fn search_by_name(&self, name: &str) -> Option<FunctionSignatureMetadata> {
        self.module.read().search_by_name(name)
    }

    pub fn bound_identifiers(&self) -> crate::values::lists::List<SteelVal> {
        self.module.read().bound_identifiers()
    }

    pub fn with_module(self, module: BuiltInModule) -> Self {
        self.module.write().with_module(module);
        self
    }

    pub fn register_type<T: FromSteelVal + IntoSteelVal>(
        &mut self,
        predicate_name: &'static str,
    ) -> &mut Self {
        self.module.write().register_type::<T>(predicate_name);
        self
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<Cow<'static, str>>,
        description: impl Into<Documentation<'static>>,
    ) -> &mut Self {
        self.module.write().register_doc(definition, description);
        self
    }

    pub fn get_doc(&self, definition: String) {
        self.module.read().get_doc(definition);
    }

    pub fn get_documentation(&self, definition: &str) -> Option<String> {
        self.module.read().get_documentation(definition)
    }

    pub(crate) fn unreadable_name(&self) -> String {
        "%-builtin-module-".to_string() + &self.module.read().name
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
        self.module
            .write()
            .register_value_with_doc(name, value, doc);

        self
    }

    // This _will_ panic given an incorrect value. This will be tied together by macros only allowing legal entries
    pub fn get(&self, name: String) -> SteelVal {
        self.module.read().get(name)
    }

    pub fn try_get(&self, name: String) -> Option<SteelVal> {
        self.module.read().try_get(name)
    }

    pub fn try_get_ref(&self, name: &str) -> Option<SteelVal> {
        self.module.read().try_get_ref(name)
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
        self.module.read().to_syntax(prefix)
    }
}

/// Documentation representation
#[derive(Clone, Debug)]
pub struct InternalDocumentation {
    definitions: HashMap<Cow<'static, str>, Documentation<'static>>,
}

impl InternalDocumentation {
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new(),
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

    pub fn definitions(&self) -> &HashMap<Cow<'static, str>, Documentation<'static>> {
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
pub struct MarkdownDoc<'a>(pub Cow<'a, str>);

impl<'a> MarkdownDoc<'a> {
    pub const fn from_str(s: &'a str) -> Self {
        Self(Cow::Borrowed(s))
    }
}

impl<'a> std::fmt::Display for MarkdownDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(feature = "markdown")]
        return write!(f, "{}", termimad::text(&self.0));

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

impl<'a> Documentation<'a> {
    pub fn as_inner_str(&self) -> Option<&str> {
        match self {
            Documentation::Markdown(markdown_doc) => Some(markdown_doc.0.as_ref()),
            _ => None,
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
