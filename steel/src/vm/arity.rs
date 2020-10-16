pub enum Arity {
    Exact(usize),
    AtLeast(usize),
}

impl Arity {
    pub fn check(&self, r: usize) -> bool {
        match &self {
            Self::Exact(l) => *l == r,
            Self::AtLeast(l) => *l <= r,
        }
    }
}

#[derive(Default)]
pub struct ArityMap(Vec<(String, Arity)>);

impl ArityMap {
    pub fn new() -> Self {
        ArityMap(Vec::new())
    }

    pub fn insert_exact(&mut self, name: &str, arity: usize) {
        self.0.push((name.to_string(), Arity::Exact(arity)))
    }

    pub fn insert(&mut self, name: &str, arity: Arity) {
        self.0.push((name.to_string(), arity))
    }

    // Should actually just result a Result<(), ArityError>
    // This way performing an arity check is simply
    // arity_map.check(name, arg_count)?
    // And it will return an error
    // TODO
    pub fn check(&self, name: &str, arg_count: usize) -> bool {
        let mut rev_iter = self.0.iter().rev();

        let result = rev_iter
            .find(|x| x.0.as_str() == name)
            .map(|x| x.1.check(arg_count));

        if let Some(b) = result {
            b
        } else {
            false
        }
    }

    pub fn roll_back(&mut self, idx: usize) {
        self.0.truncate(idx);
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}
