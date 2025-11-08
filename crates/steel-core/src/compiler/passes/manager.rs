use crate::parser::ast::ExprKind;
use alloc::{boxed::Box, vec::Vec};

use super::Folder;

type PassFn = fn() -> Box<dyn Folder>;

pub struct PassManager {
    passes: Vec<PassFn>,
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

impl PassManager {
    pub fn new() -> Self {
        PassManager { passes: Vec::new() }
    }

    pub fn register_pass(&mut self, pass: PassFn) -> &mut Self {
        self.passes.push(pass);
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
