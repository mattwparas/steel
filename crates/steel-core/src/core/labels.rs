use serde::{Deserialize, Serialize};
use steel_parser::ast::ExprKind;

use super::instructions::Instruction;
use super::opcode::OpCode;
use crate::parser::parser::SyntaxObject;

use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

pub(crate) static LABEL_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub struct Label(usize);

pub fn fresh() -> Label {
    Label(LABEL_ID.fetch_add(1, Ordering::Relaxed))
}

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub enum Expr {
    Atom(SyntaxObject),
    List(ExprKind),
}

#[derive(Clone, Debug)]
pub struct LabeledInstruction {
    pub op_code: OpCode,
    pub payload_size: usize,
    pub contents: Option<Expr>,
    pub tag: Option<Label>,
    pub goto: Option<Label>,
    pub constant: bool,
}

impl LabeledInstruction {
    pub fn builder(op_code: OpCode) -> Self {
        Self {
            op_code,
            payload_size: 0,
            contents: None,
            tag: None,
            goto: None,
            constant: false,
        }
    }

    pub fn payload(mut self, payload_size: usize) -> Self {
        self.payload_size = payload_size;
        self
    }

    pub fn contents(mut self, contents: SyntaxObject) -> Self {
        self.contents = Some(Expr::Atom(contents));
        self
    }

    pub fn list_contents(mut self, contents: ExprKind) -> Self {
        self.contents = Some(Expr::List(contents));
        self
    }

    pub fn tag(mut self, tag: Label) -> Self {
        self.tag = Some(tag);
        self
    }

    pub fn goto(mut self, goto: Label) -> Self {
        self.goto = Some(goto);
        self
    }

    pub fn constant(mut self, constant: bool) -> Self {
        self.constant = constant;
        self
    }

    pub fn set_goto(&mut self, goto: Label) {
        self.goto = Some(goto);
    }

    pub fn set_tag(&mut self, tag: Label) {
        self.tag = Some(tag);
    }
}

pub fn resolve_labels(instructions: Vec<LabeledInstruction>) -> Vec<Instruction> {
    // TODO: Come back to this
    // let label_map = instructions
    //     .iter()
    //     .enumerate()
    //     // We want a map of label -> index, flip the incoming
    //     .filter_map(|x| x.1.tag.map(|t| (t, x.0)))
    //     .collect::<HashMap<Label, usize>>();

    // dbg!(&label_map);

    instructions
        .into_iter()
        .map(|x| {
            // If this is an instruction with a jump of some kind to a label
            // Resolve that here
            // if let Some(label_goto) = x.goto {
            //     // println!("Instruction: {x:#?}");
            //     x.payload_size = label_map[&label_goto]
            // }

            Instruction {
                op_code: x.op_code,
                payload_size: x.payload_size,
                contents: x.contents,
            }
        })
        .collect()
}
