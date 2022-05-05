use crate::{
    parser::{ast::ExprKind, parser::SyntaxObject, tokens::TokenType},
    rvals::SteelVal,
};
use im_rc::HashMap;

/// A module to be consumed by the Steel Engine for later on demand access by scripts
/// to refresh the primitives that are being used. For instance, the VM should have support
/// for a primitive like so, where "kernel" has functions like `list`, `cons`, `car`, and `cdr`:
///
/// (require-builtin "kernel")
///
/// This would then expand into a series of function calls like so:
/// (begin
///     (define list (hash-get *--unreadable-module-name--kernel*))
///     (define cons (hash-get *--unreadable-module-name--kernel*))
///     (define car (hash-get *--unreadable-module-name--kernel*))
///     (define cdr (hash-get *--unreadable-module-name--kernel*)))
///
/// So for now, its just a hashmap that will get embedded smartly, accompanied with a macro that will assist
/// with reserving the proper slots in the interner
pub struct BuiltInModule {
    name: String,
    values: HashMap<String, SteelVal>,
}

impl BuiltInModule {
    pub fn new(name: String) -> Self {
        Self {
            name,
            values: HashMap::new(),
        }
    }

    fn to_syntax(&self) -> ExprKind {
        let module_name = "###-module".to_string() + self.name.as_str();

        let defines = self
            .values
            .keys()
            .map(|x| {
                ExprKind::Define(Box::new(crate::parser::ast::Define::new(
                    ExprKind::atom(x.to_string()),
                    ExprKind::List(crate::parser::ast::List::new(vec![
                        ExprKind::atom("hash-get".to_string()),
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

        ExprKind::Begin(crate::parser::ast::Begin::new(
            defines,
            SyntaxObject::default(TokenType::Begin),
        ))
    }
}
