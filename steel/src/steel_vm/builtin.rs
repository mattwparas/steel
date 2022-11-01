use std::rc::Rc;

use crate::{
    parser::{ast::ExprKind, parser::SyntaxObject, tokens::TokenType},
    rvals::{Custom, FromSteelVal, IntoSteelVal, Result, SteelVal},
};
use im_rc::OrdMap;

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
}

impl Custom for BuiltInModule {}

impl BuiltInModule {
    pub fn new(name: String) -> Self {
        Self {
            name: name.into(),
            values: OrdMap::new(),
        }
    }

    pub fn contains(&self, ident: &str) -> bool {
        self.values.contains_key(ident)
    }

    pub fn with_module<'a>(mut self, module: BuiltInModule) -> Self {
        self.values = self.values.union(module.values);
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

        self.register_value(predicate_name, SteelVal::BoxedFunction(Rc::new(f)))
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
                    .unwrap_or(x.to_owned());

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
