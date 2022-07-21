use super::instructions::Instruction;
use super::opcode::OpCode;
use crate::parser::parser::SyntaxObject;

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub struct Label(usize);

#[derive(Default, Debug)]
pub struct LabelGenerator {
    seed: usize,
}

impl LabelGenerator {
    pub fn fresh(&mut self) -> Label {
        let label = Label(self.seed);
        self.seed += 1;
        label
    }
}

#[derive(Clone, Debug)]
pub struct LabeledInstruction {
    pub op_code: OpCode,
    pub payload_size: usize,
    pub contents: Option<SyntaxObject>,
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
        self.contents = Some(contents);
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
    let label_map = instructions
        .iter()
        .enumerate()
        // We want a map of label -> index, flip the incoming
        .filter_map(|x| x.1.tag.map(|t| (t, x.0)))
        .collect::<HashMap<Label, usize>>();

    instructions
        .into_iter()
        .map(|mut x| {
            // If this is an instruction with a jump of some kind to a label
            // Resolve that here
            if let Some(label_goto) = x.goto {
                x.payload_size = label_map[&label_goto]
            }

            Instruction {
                op_code: x.op_code,
                payload_size: x.payload_size,
                contents: x.contents,
                constant: x.constant,
            }
        })
        .collect()
}
