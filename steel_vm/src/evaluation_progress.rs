use std::cell::Cell;

pub type Callback = fn(usize) -> bool;

#[derive(Clone)]
pub struct EvaluationProgress {
    instruction_count: Cell<usize>,
    callback: Option<Callback>,
}

impl EvaluationProgress {
    pub fn new() -> Self {
        EvaluationProgress {
            instruction_count: Cell::new(1),
            callback: None,
        }
    }

    pub fn with_callback(&mut self, callback: Callback) {
        self.callback.replace(callback);
    }

    pub fn callback(&self) -> Option<bool> {
        if let Some(callback) = &self.callback {
            return Some(callback(self.instruction_count.get()));
        }
        None
    }

    pub fn increment(&self) {
        self.instruction_count.set(self.instruction_count.get() + 1);
    }

    pub fn call_and_increment(&self) -> Option<bool> {
        let b = self.callback();
        self.increment();
        b
    }
}
