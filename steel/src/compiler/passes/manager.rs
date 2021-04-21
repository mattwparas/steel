use crate::parser::ast::ExprKind;

use super::Folder;

pub struct PassManager {
    passes: Vec<Box<fn() -> Box<dyn Folder>>>,
}

impl PassManager {
    pub fn new() -> Self {
        PassManager { passes: Vec::new() }
    }

    pub fn register_pass(&mut self, pass: fn() -> Box<dyn Folder>) -> &mut Self {
        self.passes.push(Box::new(pass));
        self
    }

    pub fn run(&self, mut ast: Vec<ExprKind>) -> Vec<ExprKind> {
        let mut changed = false;
        while changed {
            for thunk in &self.passes {
                let mut pass = thunk();
                ast = pass.fold(ast);
                changed = pass.modified();
            }
        }
        ast
    }
}
