use crate::parser::ast::ExprKind;
use crate::rvals::Result;

pub struct Module {
    name: Option<String>,
    main: bool,
    source_ast: Vec<ExprKind>,
    provided_macros: Vec<String>,
    dependencies: Vec<Module>,
    expanded: bool,
}

impl Module {
    pub fn new(main: bool, name: Option<String>, source_ast: Vec<ExprKind>) -> Self {
        Module {
            name,
            main,
            source_ast,
            provided_macros: Vec::new(),
            dependencies: Vec::new(),
            expanded: true,
        }
    }

    pub fn expand(&mut self) -> Result<()> {
        unimplemented!();
    }
}
