use serde::{Deserialize, Serialize};
use steel_parser::ast::ExprKind;

use super::instructions::{u24, Instruction};
use super::opcode::OpCode;
use crate::parser::parser::SyntaxObject;

use core::sync::atomic::{AtomicUsize, Ordering};

pub(crate) static LABEL_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub struct Label(usize);

pub fn fresh() -> Label {
    Label(LABEL_ID.fetch_add(1, Ordering::Relaxed))
}

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub enum Expr {
    Atom(SyntaxObject),
    List(Box<ExprKind>),
}

#[derive(Clone, Debug)]
pub struct LabeledInstruction {
    pub op_code: OpCode,
    pub payload_size: u24,
    pub contents: Option<Expr>,
    pub tag: Option<Label>,
    pub goto: Option<Label>,
}

impl LabeledInstruction {
    pub fn builder(op_code: OpCode) -> Self {
        Self {
            op_code,
            payload_size: u24::from_u32(0),
            contents: None,
            tag: None,
            goto: None,
        }
    }

    // TODO: Automatically split into two instructions
    // if the instructions don't fit into one u24.
    // There are only certain instructions where
    // we'll need to do this.
    pub fn payload(mut self, payload_size: usize) -> Self {
        self.payload_size = u24::from_usize(payload_size);
        self
    }

    pub fn payload_u24(mut self, payload: u24) -> Self {
        self.payload_size = payload;
        self
    }

    pub fn contents(mut self, contents: SyntaxObject) -> Self {
        self.contents = Some(Expr::Atom(contents));
        self
    }

    pub fn list_contents(mut self, contents: ExprKind) -> Self {
        self.contents = Some(Expr::List(Box::new(contents)));
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
