//! Shared values written once. While a value is written inside
//! [`writing_tables`], each interned string and scope set in it is written as
//! its number in a table, and the tables are returned to be written once. A
//! value written that way is read inside [`reading_tables`] with those tables:
//! each string and set in them is interned once, not once per use.
//!
//! Outside those, strings and sets are written by their contents.

use alloc::{string::String, vec::Vec};
use core::cell::RefCell;
use rustc_hash::FxHashMap;

use crate::interner::InternedString;
use crate::scopes::{Scope, ScopeSetId};

/// The strings and scope sets a value written by [`writing_tables`] refers to
#[derive(Default, serde::Serialize, serde::Deserialize)]
pub struct Tables {
    pub strings: Vec<String>,
    pub scope_sets: Vec<Vec<Scope>>,
    // Every scope written, in a set or not
    #[serde(default)]
    pub scopes: Vec<Scope>,
    // Every source id written
    #[serde(default)]
    pub sources: Vec<u32>,
}

#[derive(Default)]
struct Writing {
    strings: FxHashMap<InternedString, u32>,
    string_list: Vec<InternedString>,
    sets: FxHashMap<u64, u32>,
    set_list: Vec<ScopeSetId>,
    scopes: alloc::collections::BTreeSet<Scope>,
    sources: alloc::collections::BTreeSet<u32>,
}

struct Reading {
    strings: Vec<InternedString>,
    sets: Vec<ScopeSetId>,
}

/// How numbers saved by another process become this one's while a module's saved
/// code is read. Scopes and source ids are looked up (the caller matches module
/// scopes by name, and gives the others new numbers in the same order); a syntax
/// object id from before `syntax_base` (the boot image's) is the same in both, a
/// later one gets a fresh id, the same one each time it's read.
#[derive(Clone, Default)]
pub struct Relocation {
    pub scopes: FxHashMap<Scope, Scope>,
    pub syntax_base: u32,
    pub syntax: FxHashMap<u32, u32>,
    pub sources: FxHashMap<u32, u32>,
}

std::thread_local! {
    static RELOCATING: RefCell<Option<Relocation>> = const { RefCell::new(None) };
    static COMPARING: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };
    static WRITING: RefCell<Option<Writing>> = const { RefCell::new(None) };
    static READING: RefCell<Option<Reading>> = const { RefCell::new(None) };
}

/// Runs `write`, collecting the strings and scope sets it writes into tables
pub fn writing_tables<T>(write: impl FnOnce() -> T) -> (T, Tables) {
    let outer = WRITING.with(|w| w.replace(Some(Writing::default())));
    let result = write();
    let writing = WRITING.with(|w| w.replace(outer)).unwrap_or_default();
    let tables = Tables {
        strings: writing
            .string_list
            .iter()
            .map(|s| String::from(s.resolve()))
            .collect(),
        scope_sets: writing
            .set_list
            .iter()
            .map(|s| s.scopes().to_vec())
            .collect(),
        scopes: writing
            .set_list
            .iter()
            .flat_map(|s| s.scopes().iter().copied())
            .chain(writing.scopes.iter().copied())
            .collect::<alloc::collections::BTreeSet<_>>()
            .into_iter()
            .collect(),
        sources: writing.sources.into_iter().collect(),
    };
    (result, tables)
}

/// Runs `read`, which reads a value written by [`writing_tables`] with `tables`
pub fn reading_tables<T>(tables: &Tables, read: impl FnOnce() -> T) -> T {
    let reading = Reading {
        strings: tables
            .strings
            .iter()
            .map(|s| InternedString::from(s.as_str()))
            .collect(),
        sets: tables
            .scope_sets
            .iter()
            .map(|scopes| {
                // Moved scopes can change places
                let mut scopes: Vec<Scope> = scopes.iter().map(|s| relocate_scope(*s)).collect();
                scopes.sort();
                scopes.dedup();
                ScopeSetId::of_sorted(&scopes)
            })
            .collect(),
    };
    let outer = READING.with(|r| r.replace(Some(reading)));
    let result = read();
    READING.with(|r| r.replace(outer));
    result
}

// The string's number, if a value is being written with tables
pub(crate) fn write_string(string: InternedString) -> Option<u32> {
    WRITING.with(|w| {
        let mut w = w.borrow_mut();
        let w = w.as_mut()?;
        Some(*w.strings.entry(string).or_insert_with(|| {
            w.string_list.push(string);
            (w.string_list.len() - 1) as u32
        }))
    })
}

pub(crate) fn reading_strings() -> bool {
    READING.with(|r| r.borrow().is_some())
}

pub(crate) fn read_string(index: u32) -> Option<InternedString> {
    READING.with(|r| r.borrow().as_ref()?.strings.get(index as usize).copied())
}

// The set's number, if a value is being written with tables
pub(crate) fn write_set(set: &ScopeSetId) -> Option<u32> {
    WRITING.with(|w| {
        let mut w = w.borrow_mut();
        let w = w.as_mut()?;
        Some(*w.sets.entry(set.serial()).or_insert_with(|| {
            w.set_list.push(set.clone());
            (w.set_list.len() - 1) as u32
        }))
    })
}

pub(crate) fn read_set(index: u32) -> Option<ScopeSetId> {
    READING.with(|r| r.borrow().as_ref()?.sets.get(index as usize).cloned())
}

/// Runs `write` with syntax object ids written as 0, to compare values that are
/// copies of each other (a copy of syntax gets new ids)
pub fn comparing<T>(write: impl FnOnce() -> T) -> T {
    let outer = COMPARING.with(|c| c.replace(true));
    let result = write();
    COMPARING.with(|c| c.set(outer));
    result
}

pub(crate) fn is_comparing() -> bool {
    COMPARING.with(|c| c.get())
}

/// Runs `read` with numbers from another process made this one's (`Relocation`)
pub fn relocating<T>(relocation: Relocation, read: impl FnOnce() -> T) -> T {
    let outer = RELOCATING.with(|r| r.replace(Some(relocation)));
    let result = read();
    RELOCATING.with(|r| r.replace(outer));
    result
}

// Notes a scope written outside a set
pub(crate) fn write_scope(scope: Scope) {
    WRITING.with(|w| {
        if let Some(w) = w.borrow_mut().as_mut() {
            w.scopes.insert(scope);
        }
    })
}

// Notes a source id written
pub(crate) fn write_source(id: u32) {
    WRITING.with(|w| {
        if let Some(w) = w.borrow_mut().as_mut() {
            w.sources.insert(id);
        }
    })
}

pub(crate) fn relocate_scope(scope: Scope) -> Scope {
    RELOCATING.with(|r| match r.borrow().as_ref() {
        Some(r) => r.scopes.get(&scope).copied().unwrap_or(scope),
        None => scope,
    })
}

pub(crate) fn relocate_syntax_id(id: u32) -> u32 {
    RELOCATING.with(|r| match r.borrow_mut().as_mut() {
        // Id 0 marks a hole, not a syntax object
        Some(r) if id >= r.syntax_base && id != 0 => *r
            .syntax
            .entry(id)
            .or_insert_with(|| crate::parser::SyntaxObjectId::fresh().0),
        _ => id,
    })
}

pub(crate) fn relocate_source(id: u32) -> u32 {
    RELOCATING.with(|r| match r.borrow().as_ref() {
        Some(r) => r.sources.get(&id).copied().unwrap_or(id),
        None => id,
    })
}
