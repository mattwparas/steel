extern crate steel;
extern crate steel_derive;
extern crate steel_repl;

use steel_derive::Steel;

use steel::steel_vm::engine::Engine;
use steel_repl::run_repl;

use std::cell::RefCell;
use std::cmp::{max, min};
use std::collections::HashMap;
use std::process;

use steel::gc::{ShareableMut, Shared, SharedMut};
use steel::steel_vm::register_fn::RegisterFn;

use env_logger::Builder;
use log::LevelFilter;

/// An implementation of a spell check in steel, with the levenshtein distance calculated
/// via Rust, and a BKTree implementation in steel
/// For optimal performance, implementing the entire spellchecker in Rust and exposing
/// an interface to Steel would be ideal, however since trees go naturally with schemes
/// in general, I opted to include this as an example
/// Note: It is possible to implement the levenshtein distance in Steel, however at scale
/// it proves to be cumbersome and slows down the spell checking greatly.
///
///
/// This will launch a repl instance after building the spellchecker
/// see `spellcheck.rkt` for the exact script
fn main() {
    let mut builder = Builder::new();

    builder
        .filter(Some("steel::compiler::code_generator"), LevelFilter::Trace)
        .init();

    let mut vm = configure_engine();

    let contents = include_str!("scripts/spellcheck.scm");

    let res = vm.compile_and_run_raw_program(contents);

    if let Err(e) = res {
        e.emit_result("spellcheck.rkt", contents);
    }

    finish(run_repl(vm))
}

fn finish(result: Result<(), std::io::Error>) -> ! {
    let code = match result {
        Ok(()) => 0,
        Err(e) => {
            eprintln!(
                "{}: {}",
                std::env::args().next().unwrap_or_else(|| "steel".into()),
                e
            );
            1
        }
    };

    process::exit(code);
}

pub fn configure_engine() -> Engine {
    let mut vm = Engine::new();
    vm.register_type::<Levenshtein>("Levenshtein?")
        .register_fn("new-levenshtein", new_levenshtein)
        .register_fn("edit-distance", Levenshtein::edit_distance);
    vm
}

#[derive(Clone, Debug, Steel)]
pub struct Levenshtein(SharedMut<EditDistance>);

impl Levenshtein {
    pub fn edit_distance(self, one: String, two: String) -> usize {
        self.0.write().get_edit_distance(one.as_str(), two.as_str())
    }
}

pub fn new_levenshtein() -> Levenshtein {
    Levenshtein(Rc::new(RefCell::new(EditDistance::new(15))))
}

/// Represents edit distance with a preallocated array for distances
#[derive(Default, Debug, PartialEq)]
pub struct EditDistance {
    mat: Vec<Vec<usize>>,
}

impl EditDistance {
    /// Creates a new `EditDistance` with dimensions `n` x `n`
    ///
    /// # Examples
    ///
    /// ```
    /// # use correct::bktree::EditDistance;
    /// let mut ed1 = EditDistance::new(5);
    /// let mut ed2 = EditDistance::new(5);
    /// ed1.get_edit_distance("one", "ones");
    /// assert_ne!(ed1, ed2);
    /// ed1.clear_mat();
    /// assert_eq!(ed1, ed2);
    /// ```
    ///
    pub fn new(n: usize) -> Self {
        EditDistance {
            mat: vec![vec![0; n + 2]; n + 2],
        }
    }

    /// Zeros out the edit distance matrix
    ///
    /// # Examples
    ///
    /// ```
    /// # use correct::bktree::EditDistance;
    /// let mut ed1 = EditDistance::new(5);
    /// let mut ed2 = EditDistance::new(5);
    /// ed1.get_edit_distance("one", "ones");
    /// assert_ne!(ed1, ed2);
    /// ed1.clear_mat();
    /// assert_eq!(ed1, ed2);
    /// ```
    ///
    pub fn clear_mat(&mut self) {
        for i in 0..self.mat.len() {
            for j in 0..self.mat.len() {
                self.mat[i][j] = 0;
            }
        }
    }

    /// Calculates the Damerau-Levenshtein distance between two
    /// `&str`s, `s`, and `t`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use correct::bktree::EditDistance;
    /// let mut ed = EditDistance::new(6);
    /// assert_eq!(1, ed.get_edit_distance("matt", "mat"));
    /// assert_eq!(1, ed.get_edit_distance("mattt", "matt"));
    /// assert_eq!(1, ed.get_edit_distance("maat", "matt"));
    /// assert_eq!(1, ed.get_edit_distance("kevin", "kevi"));
    /// assert_eq!(1, ed.get_edit_distance("kevn", "kevin"));
    /// assert_eq!(2, ed.get_edit_distance("abcde", "badce"));
    /// assert_eq!(2, ed.get_edit_distance("m", "man"));
    /// assert_eq!(0, ed.get_edit_distance("", ""));
    /// assert_eq!(3, ed.get_edit_distance("", "one"));
    /// assert_eq!(3, ed.get_edit_distance("two", ""));
    /// assert_eq!(3, ed.get_edit_distance("1234", "21435"));
    ///
    /// ```
    ///
    pub fn get_edit_distance(&mut self, s: &str, t: &str) -> usize {
        // get length of unicode chars
        let len_s = s.chars().count();
        let len_t = t.chars().count();
        let max_distance = len_t + len_s;
        let longest = max(len_s, len_t);

        // TODO make this better
        // expand the matrix if its not big enough
        // self.mat = vec![vec![0; longest + 2]; longest + 2];
        if self.mat.len() < longest + 2 {
            self.mat = vec![vec![0; longest + 2]; longest + 2];
        }

        // initialize the matrix
        self.mat[0][0] = max_distance;
        for i in 0..=len_s {
            self.mat[i + 1][0] = max_distance;
            self.mat[i + 1][1] = i;
        }
        for i in 0..=len_t {
            self.mat[0][i + 1] = max_distance;
            self.mat[1][i + 1] = i;
        }

        let mut char_map: HashMap<char, usize> = HashMap::new();
        // apply edit operations
        for (i, s_char) in s.chars().enumerate() {
            let mut db = 0;
            let i = i + 1;
            for (j, t_char) in t.chars().enumerate() {
                let j = j + 1;
                let last = *char_map.get(&t_char).unwrap_or(&0);

                let cost = if s_char == t_char { 0 } else { 1 };
                self.mat[i + 1][j + 1] = min(
                    self.mat[i + 1][j] + 1, // deletion
                    min(
                        self.mat[i][j + 1] + 1, // insertion
                        min(
                            self.mat[i][j] + cost,                                  // substitution
                            self.mat[last][db] + (i - last - 1) + 1 + (j - db - 1), // transposition
                        ),
                    ),
                );
                if cost == 0 {
                    db = j;
                }
            }

            char_map.insert(s_char, i);
        }

        self.mat[len_s + 1][len_t + 1]
    }
}
