//! Compiled modules kept on disk (`STEEL_MODULE_CACHE`, with
//! `STEEL_SEPARATE_MODULES`). A module compiled on its own is saved with what a
//! later process needs in place of compiling it again:
//!
//! - its interface (`CompiledModule`: provides, macros, the code other modules
//!   inline from) and what its expansion recorded per module (bindings, struct
//!   info),
//! - the code generated for it before linking (`ModuleObject`), what it lets
//!   later modules fold (constants, globals never assigned) and the snapshot of
//!   its code they inline from.
//!
//! An entry is used when its source, the sources of everything it requires and
//! the build are what they were. The numbers in it that a process hands out are
//! made this process's when it's read: module prefixes and module scopes by the
//! module they belong to, other scopes and syntax ids made after the boot image
//! freshly, source ids by path. Modules with phase 1 code or procedural macros
//! aren't kept, nor what requires them.

use crate::compiler::modules::CompiledModule;
use crate::core::instructions::Instruction;
use crate::core::labels::Reloc;
use crate::parser::ast::ExprKind;
use crate::parser::expand_visitor::ThreadExpanderState;
use crate::parser::interner::InternedString;
use crate::parser::parser::Sources;
use crate::rvals::SteelVal;
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use steel_parser::scopes::Scope;

const MAGIC: &[u8; 8] = b"STLMODC1";

#[cfg(test)]
static KEPT: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);
#[cfg(test)]
static READ: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);

// The counters of the boot images this process started from: numbers below them
// are the same in every process that starts from the same images
static IMAGE_COUNTERS: std::sync::Mutex<Counters> = std::sync::Mutex::new(Counters {
    noted: None,
    fixed: false,
});

// The highest counters of the images made or read, until the cache first uses
// them. After that an image read later (a sandbox's) doesn't move them: numbers
// this process gave modules since would look like the image's.
struct Counters {
    noted: Option<ImageCounters>,
    fixed: bool,
}

impl Counters {
    fn note(&mut self, counters: ImageCounters) {
        if self.fixed {
            return;
        }
        let old = self.noted.unwrap_or(ImageCounters {
            scopes: 0,
            syntax: 0,
            sources: 0,
        });
        self.noted = Some(ImageCounters {
            scopes: old.scopes.max(counters.scopes),
            syntax: old.syntax.max(counters.syntax),
            sources: old.sources.max(counters.sources),
        });
    }

    fn used(&mut self) -> Option<ImageCounters> {
        if self.noted.is_some() {
            self.fixed = true;
        }
        self.noted
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
struct ImageCounters {
    scopes: u32,
    syntax: u32,
    sources: u32,
}

/// A boot image with these counters was made or read in this process
pub(crate) fn note_image_counters(scopes: u32, syntax: u32, sources: u32) {
    IMAGE_COUNTERS.lock().unwrap().note(ImageCounters {
        scopes,
        syntax,
        sources,
    });
}

fn image_counters() -> Option<ImageCounters> {
    IMAGE_COUNTERS.lock().unwrap().used()
}

std::thread_local! {
    static PAUSED: core::cell::Cell<usize> = const { core::cell::Cell::new(0) };
    static GENERATING: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };
}

/// Runs a compile that makes code: only its modules are read from the cache,
/// since a module read is only code once the compile links it. A compile that
/// stops after expanding (`emit_expanded_ast`, `expand`) compiles them.
pub(crate) fn generating_code<T>(f: impl FnOnce() -> T) -> T {
    let outer = GENERATING.with(|g| g.replace(true));
    let result = f();
    GENERATING.with(|g| g.set(outer));
    result
}

/// Off until it's dropped: while an image is made or replayed, whose contents
/// mustn't depend on what's in the cache
pub(crate) struct Paused(());

pub(crate) fn pause() -> Paused {
    PAUSED.with(|p| p.set(p.get() + 1));
    Paused(())
}

impl Drop for Paused {
    fn drop(&mut self) {
        PAUSED.with(|p| p.set(p.get() - 1));
    }
}

/// Whether modules are read from and kept in the cache
pub(crate) fn enabled() -> bool {
    let config = crate::config::config();
    config.module_cache
        && config.separate_modules
        && PAUSED.with(|p| p.get()) == 0
        && GENERATING.with(|g| g.get())
        && image_counters().is_some()
        && dir().is_some()
}

fn debug() -> bool {
    crate::config::config().module_cache_debug
}

// Next to the boot images
fn dir() -> Option<PathBuf> {
    Some(crate::steel_vm::engine::boot_image::cache_dir()?.join("modules"))
}

// The build and settings: an entry from another is never used
fn build_key() -> Option<u64> {
    use std::hash::{Hash, Hasher};
    static KEY: std::sync::OnceLock<Option<u64>> = std::sync::OnceLock::new();
    let key = (*KEY.get_or_init(|| {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        env!("CARGO_PKG_VERSION").hash(&mut hasher);
        let exe = std::fs::metadata(std::env::current_exe().ok()?).ok()?;
        exe.len().hash(&mut hasher);
        exe.modified().ok()?.hash(&mut hasher);
        let mut settings: Vec<(String, String)> = std::env::vars()
            .filter(|(name, _)| name.starts_with("STEEL_") && name != "STEEL_MODULE_CACHE_DEBUG")
            .collect();
        settings.sort();
        settings.hash(&mut hasher);
        Some(hasher.finish())
    }))?;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    (key, image_counters()?).hash(&mut hasher);
    Some(hasher.finish())
}

/// The key of a module with this source (and prelude), requiring modules with
/// these keys
pub(crate) fn module_key(source: &str, prelude: &str, requires: &[(PathBuf, u64)]) -> Option<u64> {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    (build_key()?, source, prelude, requires).hash(&mut hasher);
    Some(hasher.finish())
}

fn entry_file(path: &Path) -> Option<PathBuf> {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    path.hash(&mut hasher);
    let stem = path.file_stem().map(|s| s.to_string_lossy().into_owned()).unwrap_or_default();
    Some(dir()?.join(format!("{stem}-{:016x}.mod", hasher.finish())))
}

/// A constant of a kept module. Only plain data is kept.
#[derive(Clone, Serialize, Deserialize)]
enum PlainValue {
    Void,
    Bool(bool),
    Int(isize),
    Float(f64),
    Char(char),
    String(String),
    Symbol(String),
    Rational(i32, i32),
    BigNum(String),
    List(Vec<PlainValue>),
    Pair(Box<PlainValue>, Box<PlainValue>),
    Vector(Vec<PlainValue>),
}

impl PlainValue {
    fn of(value: &SteelVal) -> Option<Self> {
        Some(match value {
            SteelVal::Void => PlainValue::Void,
            SteelVal::BoolV(b) => PlainValue::Bool(*b),
            SteelVal::IntV(i) => PlainValue::Int(*i),
            SteelVal::NumV(f) => PlainValue::Float(*f),
            SteelVal::CharV(c) => PlainValue::Char(*c),
            SteelVal::StringV(s) => PlainValue::String(s.as_str().to_string()),
            SteelVal::SymbolV(s) => PlainValue::Symbol(s.as_str().to_string()),
            SteelVal::Rational(r) => PlainValue::Rational(*r.numer(), *r.denom()),
            SteelVal::BigNum(b) => PlainValue::BigNum(b.to_string()),
            SteelVal::ListV(l) => PlainValue::List(l.iter().map(Self::of).collect::<Option<_>>()?),
            SteelVal::Pair(p) => PlainValue::Pair(
                Box::new(Self::of(p.car_ref())?),
                Box::new(Self::of(p.cdr_ref())?),
            ),
            SteelVal::VectorV(v) => {
                PlainValue::Vector(v.iter().map(Self::of).collect::<Option<_>>()?)
            }
            _ => return None,
        })
    }

    fn value(&self) -> Option<SteelVal> {
        Some(match self {
            PlainValue::Void => SteelVal::Void,
            PlainValue::Bool(b) => SteelVal::BoolV(*b),
            PlainValue::Int(i) => SteelVal::IntV(*i),
            PlainValue::Float(f) => SteelVal::NumV(*f),
            PlainValue::Char(c) => SteelVal::CharV(*c),
            PlainValue::String(s) => SteelVal::StringV(s.as_str().into()),
            PlainValue::Symbol(s) => SteelVal::SymbolV(s.as_str().into()),
            PlainValue::Rational(n, d) => {
                SteelVal::Rational(num_rational::Rational32::new_raw(*n, *d))
            }
            PlainValue::BigNum(s) => {
                SteelVal::BigNum(crate::gc::Gc::new(s.parse::<num_bigint::BigInt>().ok()?))
            }
            PlainValue::List(l) => {
                SteelVal::ListV(l.iter().map(Self::value).collect::<Option<Vec<_>>>()?.into())
            }
            PlainValue::Pair(car, cdr) => SteelVal::Pair(crate::gc::Gc::new(
                crate::values::lists::Pair::cons(car.value()?, cdr.value()?),
            )),
            PlainValue::Vector(v) => SteelVal::VectorV(
                v.iter().map(Self::value).collect::<Option<Vec<_>>>()?.into_iter().collect(),
            ),
        })
    }
}

fn plain_pairs(pairs: &[(InternedString, SteelVal)]) -> Option<Vec<(InternedString, PlainValue)>> {
    pairs.iter().map(|(name, value)| Some((*name, PlainValue::of(value)?))).collect()
}

fn value_pairs(pairs: &[(InternedString, PlainValue)]) -> Option<Vec<(InternedString, SteelVal)>> {
    pairs.iter().map(|(name, value)| Some((*name, value.value()?))).collect()
}

/// A module's code before it's linked (`ModuleObject`), as kept
#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct SavedObject {
    pub(crate) instructions: Vec<Vec<Instruction>>,
    pub(crate) relocations: Vec<Vec<(usize, Reloc)>>,
    constants: Vec<PlainValue>,
    pub(crate) functions: usize,
}

impl SavedObject {
    /// The object, or the constant that can't be kept
    pub(crate) fn new(
        instructions: Vec<Vec<Instruction>>,
        relocations: Vec<Vec<(usize, Reloc)>>,
        constants: &[SteelVal],
        functions: usize,
    ) -> core::result::Result<Self, String> {
        let constants = constants
            .iter()
            .map(|c| PlainValue::of(c).ok_or_else(|| format!("{c:?}").chars().take(120).collect()))
            .collect::<core::result::Result<_, String>>()?;
        Ok(SavedObject {
            instructions,
            relocations,
            constants,
            functions,
        })
    }

    pub(crate) fn constants(&self) -> Option<Vec<SteelVal>> {
        self.constants.iter().map(PlainValue::value).collect()
    }
}

/// What a compile needs from a module read from the cache in place of
/// lowering it and generating its code
#[derive(Clone)]
pub(crate) struct CachedModule {
    pub(crate) snapshot: Option<ExprKind>,
    // Its names for builtins, and the reserved names lowering replaces them with
    pub(crate) builtins: Vec<(InternedString, InternedString)>,
    pub(crate) learned: Vec<(InternedString, SteelVal)>,
    pub(crate) non_mutable: Vec<InternedString>,
    pub(crate) object: SavedObject,
}

/// What the module compile recorded about a module compiled from source, for
/// keeping it once the rest of its compile is done
#[derive(Clone)]
pub(crate) struct Compiled {
    pub(crate) key: u64,
    pub(crate) source: u64,
    pub(crate) requires: Vec<PathBuf>,
    pub(crate) expander: ThreadExpanderState,
    pub(crate) structs: Vec<((InternedString, InternedString), (Vec<InternedString>, bool))>,
}

/// This process's cache state, kept with the module manager
#[derive(Clone, Default)]
pub(crate) struct State {
    // The key of each module compiled or read, None if it can't be kept
    pub(crate) keys: FxHashMap<PathBuf, Option<u64>>,
    // Read for the compile running now
    pub(crate) read: FxHashMap<PathBuf, CachedModule>,
    // Compiled from source by the compile running now, to keep
    pub(crate) compiled: FxHashMap<PathBuf, Compiled>,
    // Modules seen to have phase 1 code
    pub(crate) refused: FxHashSet<PathBuf>,
}

#[derive(Serialize, Deserialize)]
struct Header {
    path: PathBuf,
    key: u64,
    // Of the source alone: an entry for another version isn't used for anything
    source: u64,
    requires: Vec<PathBuf>,
}

/// A hash of a module's source
pub(crate) fn source_hash(source: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    source.hash(&mut hasher);
    hasher.finish()
}

#[derive(Serialize, Deserialize)]
struct Numbering {
    counters: ImageCounters,
    // Module prefix numbers in the strings, with the modules they're for
    prefixes: Vec<(u32, String)>,
    // Module scopes, with their modules
    module_scopes: Vec<(Scope, String)>,
    // Source ids, with their files
    sources: Vec<(u32, Option<PathBuf>)>,
}

#[derive(Serialize, Deserialize)]
struct Body {
    module: CompiledModule,
    snapshot: Option<ExprKind>,
    builtins: Vec<(InternedString, InternedString)>,
    learned: Vec<(InternedString, PlainValue)>,
    non_mutable: Vec<InternedString>,
    object: SavedObject,
    expander: ThreadExpanderState,
    structs: Vec<((InternedString, InternedString), (Vec<InternedString>, bool))>,
}

/// Everything kept for a module
pub(crate) struct Keep<'a> {
    pub(crate) path: &'a Path,
    pub(crate) compiled: &'a Compiled,
    pub(crate) module: &'a CompiledModule,
    pub(crate) snapshot: Option<&'a ExprKind>,
    pub(crate) builtins: Vec<(InternedString, InternedString)>,
    pub(crate) learned: &'a [(InternedString, SteelVal)],
    pub(crate) non_mutable: &'a [InternedString],
    pub(crate) object: SavedObject,
}

// The module prefixes `##mm<n>__%#__` in `s`, each number replaced by `f`'s.
// (The other names the compiler makes for a module, like template prefixes and
// lifted functions, name it by its prefix.)
fn numbered_names(s: &str, mut f: impl FnMut(u32) -> u32) -> Option<String> {
    const START: &str = "##mm";
    let end = crate::compiler::modules::MANGLER_SEPARATOR;
    if !s.contains(START) {
        return None;
    }
    let mut out = String::with_capacity(s.len());
    let mut rest = s;
    while !rest.is_empty() {
        if let Some(after) = rest.strip_prefix(START) {
            let digits = after.bytes().take_while(|b| b.is_ascii_digit()).count();
            if digits > 0 && after[digits..].starts_with(end) {
                let number: u32 = after[..digits].parse().ok()?;
                out.push_str(START);
                out.push_str(&f(number).to_string());
                rest = &after[digits..];
                continue;
            }
        }
        let next = rest.chars().next().unwrap();
        out.push(next);
        rest = &rest[next.len_utf8()..];
    }
    Some(out)
}

fn file_bytes(sections: &[&[u8]]) -> Vec<u8> {
    let mut body = Vec::new();
    for section in sections {
        body.extend_from_slice(&(section.len() as u64).to_le_bytes());
        body.extend_from_slice(section);
    }
    let mut out = Vec::with_capacity(body.len() + 16);
    out.extend_from_slice(MAGIC);
    out.extend_from_slice(&checksum(&body).to_le_bytes());
    out.extend_from_slice(&body);
    out
}

// The sections of an entry whose checksum matches
fn sections(bytes: &[u8]) -> Option<Vec<&[u8]>> {
    let rest = bytes.strip_prefix(MAGIC.as_slice())?;
    let (sum, rest) = rest.split_at_checked(8)?;
    if u64::from_le_bytes(sum.try_into().ok()?) != checksum(rest) {
        return None;
    }
    unchecked_sections(bytes)
}

// The sections of an entry already checked (`find` checks what it gives)
fn unchecked_sections(bytes: &[u8]) -> Option<Vec<&[u8]>> {
    let rest = bytes.strip_prefix(MAGIC.as_slice())?;
    let (_, mut rest) = rest.split_at_checked(8)?;
    let mut out = Vec::new();
    while !rest.is_empty() {
        let (len, after) = rest.split_at_checked(8)?;
        let len = u64::from_le_bytes(len.try_into().ok()?) as usize;
        let (section, after) = after.split_at_checked(len)?;
        out.push(section);
        rest = after;
    }
    Some(out)
}

fn checksum(bytes: &[u8]) -> u64 {
    use std::hash::Hasher;
    let mut hasher = rustc_hash::FxHasher::default();
    for chunk in bytes.chunks(8) {
        let mut word = [0u8; 8];
        word[..chunk.len()].copy_from_slice(chunk);
        hasher.write_u64(u64::from_le_bytes(word));
    }
    hasher.write_usize(bytes.len());
    hasher.finish()
}

/// Keeps a module. A module that can't be kept, or a failure to write, leaves
/// the cache as it was.
pub(crate) fn keep(entry: Keep<'_>, sources: &Sources) {
    let path = entry.path;
    let result = (|| -> core::result::Result<(), String> {
        let counters = image_counters().ok_or("no boot image")?;
        let learned = plain_pairs(entry.learned).ok_or("a learned constant isn't plain data")?;
        let header = Header {
            path: path.to_path_buf(),
            key: entry.compiled.key,
            source: entry.compiled.source,
            requires: entry.compiled.requires.clone(),
        };
        let body = Body {
            module: entry.module.clone(),
            snapshot: entry.snapshot.cloned(),
            builtins: entry.builtins,
            learned,
            non_mutable: entry.non_mutable.to_vec(),
            object: entry.object,
            expander: entry.compiled.expander.clone(),
            structs: entry.compiled.structs.clone(),
        };
        let (body, tables) = steel_parser::tables::writing_tables(|| bincode::serialize(&body));
        let body = body.map_err(|e| e.to_string())?;

        let mut prefixes = FxHashMap::default();
        let mut unknown = false;
        for s in &tables.strings {
            numbered_names(s, |n| {
                match crate::compiler::modules::prefix_path_of(n) {
                    Some(p) => {
                        prefixes.insert(n, p);
                    }
                    None => unknown = true,
                }
                n
            });
        }
        if unknown {
            return Err("a module prefix with no module".into());
        }
        let mut module_scopes = Vec::new();
        for scope in &tables.scopes {
            if scope.is_module() && scope.time() >= counters.scopes {
                module_scopes.push((*scope, scope.module_name().ok_or("a module scope with no module")?));
            }
        }
        let numbering = Numbering {
            counters,
            prefixes: prefixes.into_iter().collect(),
            module_scopes,
            sources: tables
                .sources
                .iter()
                .filter(|id| **id >= counters.sources)
                .map(|id| (*id, sources.get_path(&steel_parser::parser::SourceId(*id))))
                .collect(),
        };
        let header = bincode::serialize(&header).map_err(|e| e.to_string())?;
        let numbering = bincode::serialize(&numbering).map_err(|e| e.to_string())?;
        let tables = bincode::serialize(&tables).map_err(|e| e.to_string())?;
        let bytes = file_bytes(&[&header, &numbering, &tables, &body]);

        let file = entry_file(path).ok_or("no cache directory")?;
        let dir = file.parent().ok_or("no cache directory")?;
        std::fs::create_dir_all(dir).map_err(|e| e.to_string())?;
        // Written whole, then renamed, so a reader never sees part of one. The
        // name is this write's alone, so two writers don't share one.
        static WRITES: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);
        let temporary = file.with_extension(format!(
            "{}.{}.tmp",
            std::process::id(),
            WRITES.fetch_add(1, core::sync::atomic::Ordering::Relaxed)
        ));
        std::fs::write(&temporary, bytes).map_err(|e| e.to_string())?;
        std::fs::rename(&temporary, &file).map_err(|e| {
            let _ = std::fs::remove_file(&temporary);
            e.to_string()
        })?;
        Ok(())
    })();
    #[cfg(test)]
    if result.is_ok() {
        KEPT.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
    }
    if debug() {
        match result {
            Ok(()) => eprintln!("module cache: kept {}", path.display()),
            Err(e) => eprintln!("module cache: not keeping {}: {e}", path.display()),
        }
    }
}

/// The entry kept for `path` with this source, if there is one: its key, the
/// modules it requires, and the bytes to read it from
pub(crate) fn find(path: &Path, source: &str) -> Option<(u64, Vec<PathBuf>, Vec<u8>)> {
    let bytes = std::fs::read(entry_file(path)?).ok()?;
    let header: Header = bincode::deserialize(sections(&bytes)?.first()?).ok()?;
    if header.path != path {
        return None;
    }
    if header.source != source_hash(source) {
        if debug() {
            eprintln!("module cache: {} changed", path.display());
        }
        return None;
    }
    Some((header.key, header.requires, bytes))
}

/// An entry read is used in place of compiling its module
pub(crate) fn used(path: &Path) {
    #[cfg(test)]
    READ.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
    if debug() {
        eprintln!("module cache: read {}", path.display());
    }
}

/// What was read from an entry
pub(crate) struct Read {
    pub(crate) module: CompiledModule,
    pub(crate) cached: CachedModule,
    pub(crate) expander: ThreadExpanderState,
    pub(crate) structs: Vec<((InternedString, InternedString), (Vec<InternedString>, bool))>,
}

// This process's id for a source an entry refers to: the file's (read now if
// nothing has yet; a registered module's is replaced when it's loaded), or one
// empty source for all that had no file
fn source_here(sources: &mut Sources, file: Option<&PathBuf>) -> steel_parser::parser::SourceId {
    let path = file.cloned().unwrap_or_else(|| PathBuf::from("#%no-source"));
    if let Some(here) = sources.get_source_id(&path) {
        return here;
    }
    let text = file.and_then(|f| std::fs::read_to_string(f).ok()).unwrap_or_default();
    sources.add_source(text, Some(path))
}

/// Reads an entry's bytes (from `find`), its numbers made this process's
pub(crate) fn read(path: &Path, bytes: &[u8], sources: &mut Sources) -> Option<Read> {
    let result = (|| -> core::result::Result<Read, &'static str> {
        let sections = unchecked_sections(bytes).ok_or("damaged")?;
        let [_, numbering, tables, body] = sections.as_slice() else {
            return Err("damaged");
        };
        let numbering: Numbering = bincode::deserialize(numbering).map_err(|_| "damaged")?;
        let counters = image_counters().ok_or("no boot image")?;
        if numbering.counters != counters {
            return Err("made from another boot image");
        }
        let mut tables: steel_parser::tables::Tables =
            bincode::deserialize(tables).map_err(|_| "damaged")?;

        // Module prefixes by module
        let prefixes: FxHashMap<u32, u32> = numbering
            .prefixes
            .iter()
            .map(|(n, p)| (*n, crate::compiler::modules::prefix_number(p)))
            .collect();
        let mut missing = false;
        let mut rename = |s: &str| {
            numbered_names(s, |n| {
                *prefixes.get(&n).unwrap_or_else(|| {
                    missing = true;
                    &0
                })
            })
        };
        for s in tables.strings.iter_mut() {
            if let Some(renamed) = rename(s) {
                *s = renamed;
            }
        }
        let mut relocation = steel_parser::tables::Relocation {
            syntax_base: counters.syntax,
            ..Default::default()
        };
        for (scope, name) in &numbering.module_scopes {
            relocation.scopes.insert(*scope, Scope::module(name));
        }
        // The others are given new times in the same order
        let fresh: Vec<Scope> = tables
            .scopes
            .iter()
            .copied()
            .filter(|s| !s.is_module() && s.time() >= counters.scopes)
            .collect();
        let mut fresh = fresh;
        fresh.sort_by_key(|s| s.time());
        fresh.dedup();
        if !fresh.is_empty() {
            let start = Scope::reserve(fresh.len() as u32);
            for (i, scope) in fresh.iter().enumerate() {
                relocation.scopes.insert(*scope, scope.with_time(start + i as u32));
            }
        }
        for (id, file) in &numbering.sources {
            relocation.sources.insert(*id, source_here(sources, file.as_ref()).0);
        }

        let mut body: Body = steel_parser::tables::relocating(relocation, || {
            steel_parser::tables::reading_tables(&tables, || bincode::deserialize(body))
        })
        .map_err(|_| "damaged")?;
        // The strings kept outside the tables
        body.module.renumber(&mut rename);
        drop(rename);
        if missing {
            return Err("damaged");
        }
        Ok(Read {
            module: body.module,
            cached: CachedModule {
                snapshot: body.snapshot,
                builtins: body.builtins,
                learned: value_pairs(&body.learned).ok_or("damaged")?,
                non_mutable: body.non_mutable,
                object: body.object,
            },
            expander: body.expander,
            structs: body.structs,
        })
    })();
    match result {
        Ok(read) => Some(read),
        Err(e) => {
            if debug() {
                eprintln!("module cache: not reading {}: {e}", path.display());
            }
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // A damaged entry isn't read
    #[test]
    fn damaged_entries_are_refused() {
        let bytes = file_bytes(&[b"header", b"body"]);
        assert_eq!(sections(&bytes), Some(vec![&b"header"[..], &b"body"[..]]));
        let mut damaged = bytes.clone();
        let last = damaged.len() - 1;
        damaged[last] ^= 1;
        assert_eq!(sections(&damaged), None);
        assert!(sections(&bytes[..bytes.len() - 2]).is_none());
    }

    // Sources an entry refers to are found again, not added again
    #[test]
    fn sources_are_found_by_file() {
        let mut sources = Sources::new();
        let a = source_here(&mut sources, None);
        let b = source_here(&mut sources, None);
        assert_eq!(a, b, "one source for those without a file");
        let file = PathBuf::from("registered/module");
        let known = sources.add_source("(define x 1)".to_string(), Some(file.clone()));
        assert_eq!(source_here(&mut sources, Some(&file)), known);
        assert_eq!(sources.size_in_bytes(), "(define x 1)".len());
    }

    // An image read after the cache has used the counters doesn't move them
    #[test]
    fn image_counters_stay_once_used() {
        let at = |n| ImageCounters {
            scopes: n,
            syntax: n,
            sources: n,
        };
        let mut counters = Counters {
            noted: None,
            fixed: false,
        };
        assert_eq!(counters.used(), None);
        counters.note(at(10));
        counters.note(at(5));
        assert_eq!(counters.used(), Some(at(10)));
        counters.note(at(20));
        assert_eq!(counters.used(), Some(at(10)));
    }

    #[test]
    fn numbered_names_are_found() {
        let renamed = numbered_names("##mm12__%#__foo@##mm3__%#__ #%template7@##mm4__%#__:x ##mm5x", |n| n + 100);
        assert_eq!(
            renamed.as_deref(),
            Some("##mm112__%#__foo@##mm103__%#__ #%template7@##mm104__%#__:x ##mm5x")
        );
        assert_eq!(numbered_names("plain", |n| n), None);
    }

    const CONSTANTS: &str = r#"
(provide SCALE scale shout sum-to twice (struct-out point) (for-syntax swap!) (for-syntax call-helper)
         (for-syntax with-helper))
;; A require its expansion writes
(define-syntax with-helper
  (syntax-rules ()
    [(_ name) (begin (require "cache/helper") (define (name y) (helper (+ y 1))))]))
(define SCALE 3)
(define (scale x) (* x SCALE))
(define (shout s) (string-append s "!"))
(struct point (x y) #:transparent)
(define (sum-to n)
  (let loop ([i 0] [acc 0])
    (if (> i n) acc (loop (+ i 1) (+ acc i)))))
(define-syntax swap!
  (syntax-rules ()
    [(_ a b) (let ([t a]) (set! a b) (set! b t))]))
(define (twice f) (lambda (x) (f (f x))))
;; Not provided: the macro's expansion refers to it where it's defined
(define (private-helper x) (* x 10))
(define-syntax call-helper
  (syntax-rules ()
    [(_ x) (let ([private-helper 0]) (+ private-helper (helper-of x)))]))
(define (helper-of x) (private-helper x))
"#;

    const USES: &str = r#"
(require "cache/constants")
(require "cache/gone")
(provide report broken (for-syntax scaled) (for-syntax swap-scaled!))
;; Refers to what this module imports
(define-syntax scaled
  (syntax-rules ()
    [(_ x) (list (scale x) SCALE (shout "s"))]))
(define-syntax swap-scaled!
  (syntax-rules ()
    [(_ a b) (begin (swap! a b) (set! a (scale a)))]))
(define (report)
  (list (scale 7) (shout "hi") (point-x (point 1 2)) (sum-to 10)
        (let ([a 1] [b 2]) (swap! a b) (list a b))
        ((twice scale) 2)
        (map (lambda (p) (point-y p)) (list (point 5 6)))
        (equal? (point 1 2) (point 1 2))
        'sym "str" #\c 1.5 '(1 (2 3)) #(1 2) '(1 . 2) '(1 2 . 3)))
(define (broken x)
  (+ x "not a number"))
(provide helped)
(with-helper helped)
"#;

    // Inlines what `cache/uses` made with a template's require
    const THIRD: &str = r#"
(require "cache/uses")
(provide third)
(define (third) (list (helped 1) (helped 2)))
"#;

    const PROGRAM: &str = r#"
(require "cache/uses")
(require "cache/constants")
(require "cache/third")
(list (third) (helped 0) (report) (point? (point 0 0)) (scale SCALE)
      (let ([a 1] [b 2]) (swap! a b) (list a b))
      (call-helper 4)
      (scaled 5)
      (let ([a 1] [b 2]) (swap-scaled! a b) (list a b)))
"#;

    // In a child process: runs the program over modules registered in memory,
    // `MODULE_CHECK_VARIANT=changed` with another constant
    fn run_program() -> String {
        let mut out = String::new();
        let mut shifter = crate::steel_vm::engine::Engine::new();
        // `MODULE_CHECK_SHIFT`: a module compiled first, so this process numbers
        // what it makes after it differently
        if std::env::var_os("MODULE_CHECK_SHIFT").is_some() {
            shifter.register_steel_module(
                "cache/first".to_string(),
                r#"
(provide first-fn (for-syntax first-macro))
(struct first-thing (a b c))
;; The same names as the kept modules'
(define (scale x) 'wrong)
(define SCALE 'wrong)
(define (shout x) 'wrong)
(define (helper-of x) 'wrong)
(define (private-helper x) 'wrong)
(define (first-fn x)
  (let loop ([i 0]) (if (< i x) (loop (+ i 1)) (first-thing-a (first-thing i i i)))))
(define-syntax first-macro (syntax-rules () [(_ x) (let ([y x]) (+ y y))]))
"#
                .to_string(),
            );
            shifter.run(r#"(require "cache/first") (first-macro (first-fn 3))"#).unwrap();
        }

        // A compile that only expands has the modules' code, read or not
        {
            let mut engine = crate::steel_vm::engine::Engine::new();
            register_modules(&mut engine);
            let expanded = engine.emit_expanded_ast(PROGRAM, None).unwrap();
            let text: String = expanded.iter().map(|e| e.to_string()).collect();
            out.push_str(&format!("expanded has the module code: {}\n", text.contains("not a number")));
        }
        let mut engine = crate::steel_vm::engine::Engine::new();
        register_modules(&mut engine);
        run_in(&mut engine, out)
    }

    // `cache/lib` is compiled while the cache is off (so it has no key) before
    // what requires it is
    fn run_lib(version: &str) -> String {
        let mut engine = crate::steel_vm::engine::Engine::new();
        let value = if version == "lib-1" { 1 } else { 2 };
        engine.register_steel_module(
            "cache/lib".to_string(),
            format!("(provide lib-value LIB) (define (lib-value) {value}) (define LIB {value})"),
        );
        engine.register_steel_module(
            "cache/libuser".to_string(),
            "(require \"cache/lib\") (provide libuser) (define (libuser) (+ (lib-value) LIB 100))"
                .to_string(),
        );
        {
            let _paused = pause();
            engine.run("(require \"cache/lib\")").unwrap();
        }
        match engine.run("(require \"cache/libuser\") (libuser)") {
            Ok(values) => format!("{:?}\n", values.last()),
            Err(e) => format!("error: {e}\n"),
        }
    }

    // Requires `a.scm` from the directory `MODULE_CHECK_FILES`
    fn run_files() -> String {
        let dir = PathBuf::from(std::env::var_os("MODULE_CHECK_FILES").unwrap());
        let mut engine = crate::steel_vm::engine::Engine::new();
        let program = format!("(require \"{}\") (a)", dir.join("project/a.scm").display());
        match engine.run(program) {
            Ok(values) => format!("{:?}\n", values.last()),
            Err(e) => format!("error: {e}\n"),
        }
    }

    const SHAPE: &str = "(provide (struct-out shape)) (struct shape (a b c))";
    const USE_SHAPE: &str = r#"
(require "cache/shape")
(match (shape 1 2 3) [(shape a b c) (list a b c)])
"#;

    // `keep`: keeps the version of `cache/shape` with three fields. Otherwise
    // compiles one with two first, without the cache, then reads the kept one
    fn run_shapes(older_first: bool) -> String {
        if older_first {
            let _paused = pause();
            let mut engine = crate::steel_vm::engine::Engine::new();
            engine.register_steel_module(
                "cache/shape".to_string(),
                "(provide (struct-out shape)) (struct shape (a b))".to_string(),
            );
            engine
                .run("(require \"cache/shape\") (match (shape 1 2) [(shape a b) (list a b)])")
                .unwrap();
        }
        let mut engine = crate::steel_vm::engine::Engine::new();
        engine.register_steel_module("cache/shape".to_string(), SHAPE.to_string());
        match engine.run(USE_SHAPE) {
            Ok(values) => format!("{:?}\n", values.last()),
            Err(e) => format!("error: {e}\n"),
        }
    }

    fn register_modules(engine: &mut crate::steel_vm::engine::Engine) {
        let constants = match std::env::var("MODULE_CHECK_VARIANT").as_deref() {
            Ok("changed") => CONSTANTS.replace("(define SCALE 3)", "(define SCALE 4)"),
            _ => CONSTANTS.to_string(),
        };
        engine.register_steel_module("cache/constants".to_string(), constants);
        engine.register_steel_module("cache/third".to_string(), THIRD.to_string());
        engine.register_steel_module(
            "cache/helper".to_string(),
            "(provide helper) (define (helper x) (* x 100))".to_string(),
        );
        // `dropped`: `cache/uses` no longer requires `cache/gone`, which is gone
        if std::env::var("MODULE_CHECK_VARIANT").as_deref() == Ok("dropped") {
            engine.register_steel_module(
                "cache/uses".to_string(),
                USES.replace("(require \"cache/gone\")", ""),
            );
        } else {
            engine.register_steel_module("cache/uses".to_string(), USES.to_string());
            engine.register_steel_module(
                "cache/gone".to_string(),
                "(provide gone) (define (gone) 'gone)".to_string(),
            );
        }
    }

    fn run_in(engine: &mut crate::steel_vm::engine::Engine, mut out: String) -> String {
        match engine.run(PROGRAM) {
            Ok(values) => {
                for value in values {
                    out.push_str(&format!("{value}\n"));
                }
            }
            Err(e) => out.push_str(&format!("error: {e}\n")),
        }
        // An error inside a module, with its source
        match engine.run("(broken 1)") {
            Ok(_) => out.push_str("no error\n"),
            Err(e) => out.push_str(&engine.raise_error_to_string(e).unwrap_or_default()),
        }
        // Every module prefix anywhere in the modules, by the module it names:
        // the same whether a module was read or compiled
        let compiler = engine.virtual_machine.compiler.read();
        for name in ["cache/constants", "cache/uses", "cache/third", "cache/helper"] {
            let module = &compiler.module_manager.modules()[&PathBuf::from(name)];
            let text = format!("{module:?}");
            let mut named: Vec<String> = Vec::new();
            numbered_names(&text, |n| {
                named.push(crate::compiler::modules::prefix_path_of(n).unwrap_or_default());
                n
            });
            named.sort();
            let mut counts: Vec<(String, usize)> = Vec::new();
            for path in named {
                match counts.last_mut() {
                    Some((p, c)) if *p == path => *c += 1,
                    _ => counts.push((path, 1)),
                }
            }
            out.push_str(&format!("{name} names {counts:?}\n"));
        }
        out
    }

    // Modules read from the cache run as they did compiled: three child
    // processes share a cache. One compiles without it, one compiles and keeps
    // the modules, one reads them, after compiling another module first so the
    // numbers it hands out (module prefixes, scopes, source ids) aren't the
    // ones the modules were kept with. Then a changed module is compiled
    // again, and so is the module that requires it.
    #[test]
    fn modules_read_from_the_cache_run_as_compiled() {
        const TEST: &str =
            "compiler::module_cache::tests::modules_read_from_the_cache_run_as_compiled";
        if let Some(out) = std::env::var_os("MODULE_CHECK_OUT") {
            // The directory the program asks for
            if let Some(dir) = std::env::var_os("MODULE_CHECK_HOST_DIR") {
                crate::steel_vm::engine::Engine::enable_boot_image_cache(Some(dir.into()));
            }
            let text = match std::env::var("MODULE_CHECK_VARIANT").as_deref() {
                Ok(v @ ("shape-keep" | "shape")) => run_shapes(v == "shape"),
                Ok(v @ ("lib-1" | "lib-2")) => run_lib(v),
                Ok("files") => run_files(),
                _ => run_program(),
            };
            let kept = KEPT.load(core::sync::atomic::Ordering::Relaxed);
            let read = READ.load(core::sync::atomic::Ordering::Relaxed);
            std::fs::write(&out, format!("{kept} {read}\n{text}")).unwrap();
            return;
        }

        let dir = std::env::temp_dir().join(format!("steel-module-check-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let run = |role: &str, cache: bool, variant: &str| -> (usize, usize, String) {
            let shift = role == "warm";
            let out = dir.join(format!("{role}.txt"));
            let mut command = std::process::Command::new(std::env::current_exe().unwrap());
            command
                .args(["--exact", TEST, "--nocapture", "--test-threads=1"])
                .env("MODULE_CHECK_OUT", &out)
                .env("MODULE_CHECK_VARIANT", variant)
                .env("STEEL_BOOT_IMAGE_CACHE", "1")
                .env("STEEL_BOOT_IMAGE_DIR", dir.join("cache"))
                .env("STEEL_SEPARATE_MODULES", "1")
                .env("STEEL_MODULE_CACHE", if cache { "1" } else { "0" });
            if shift {
                command.env("MODULE_CHECK_SHIFT", "1");
            }
            let status = command.status().unwrap();
            assert!(status.success(), "the {role} process failed");
            let text = std::fs::read_to_string(&out).unwrap();
            let (counts, rest) = text.split_once('\n').unwrap();
            let (kept, read) = counts.split_once(' ').unwrap();
            (kept.parse().unwrap(), read.parse().unwrap(), rest.to_string())
        };

        let (_, _, compiled) = run("compiled", false, "");
        let (kept, read, cold) = run("cold", true, "");
        assert_eq!((kept, read), (5, 0), "the first process keeps the modules");
        let (kept, read, warm) = run("warm", true, "");
        assert_eq!((kept, read), (1, 5), "the second reads them (and keeps the one it compiles first)");
        assert!(compiled.contains("TypeMismatch"), "{compiled}");
        assert!(compiled.contains("not a number"), "the error shows the module's source:\n{compiled}");
        assert_eq!(compiled, cold);
        assert_eq!(compiled, warm);

        // An entry for an older version isn't used, nor what it required
        let (_, _, dropped_compiled) = run("dropped-compiled", false, "dropped");
        let (kept, read, dropped) = run("dropped", true, "dropped");
        assert_eq!((kept, read), (2, 2), "the changed module and what requires it are compiled again");
        assert_eq!(dropped, dropped_compiled);

        let (_, _, changed_compiled) = run("changed-compiled", false, "changed");
        let (kept, read, changed) = run("changed", true, "changed");
        assert_eq!((kept, read), (3, 2), "a changed module and what requires it are compiled again");
        assert_eq!(changed, changed_compiled);
        assert_ne!(changed, compiled);

        // Modules are kept where the program asks for boot images to be
        {
            let out = dir.join("host-dir.txt");
            let status = std::process::Command::new(std::env::current_exe().unwrap())
                .args(["--exact", TEST, "--nocapture", "--test-threads=1"])
                .env("MODULE_CHECK_OUT", &out)
                .env("MODULE_CHECK_HOST_DIR", dir.join("host"))
                .env("HOME", dir.join("home"))
                .env_remove("XDG_CACHE_HOME")
                .env_remove("STEEL_BOOT_IMAGE_DIR")
                .env_remove("STEEL_BOOT_IMAGE_CACHE")
                .env("STEEL_SEPARATE_MODULES", "1")
                .env("STEEL_MODULE_CACHE", "1")
                .status()
                .unwrap();
            assert!(status.success(), "the host-dir process failed");
            let kept = std::fs::read_dir(dir.join("host").join("modules")).map(|d| d.count()).unwrap_or(0);
            assert!(kept >= 5, "modules kept in the asked-for directory: {kept}");
            assert!(!dir.join("home").exists(), "nothing goes to the user's cache directory");
        }

        // What requires a module the cache knows nothing about isn't kept
        let (kept, _, first) = run("lib-1", true, "lib-1");
        assert!(first.contains("102"), "{first}");
        assert_eq!(kept, 0, "what requires a module with no key isn't kept");
        let (_, _, second) = run("lib-2", true, "lib-2");
        assert!(second.contains("104"), "a changed module is seen: {second}");

        // A module's requires resolve as they would now: a file that would be
        // found first is used (files, since how files are found is the point)
        {
            let files = dir.join("files");
            std::fs::create_dir_all(files.join("project")).unwrap();
            std::fs::create_dir_all(files.join("search")).unwrap();
            std::fs::write(
                files.join("project/a.scm"),
                "(require \"b.scm\") (provide a) (define (a) (b))",
            )
            .unwrap();
            std::fs::write(files.join("search/b.scm"), "(provide b) (define (b) 'from-search)").unwrap();
            let run_files = |role: &str| -> (usize, String) {
                let out = dir.join(format!("{role}.txt"));
                let status = std::process::Command::new(std::env::current_exe().unwrap())
                    .args(["--exact", TEST, "--nocapture", "--test-threads=1"])
                    .env("MODULE_CHECK_OUT", &out)
                    .env("MODULE_CHECK_VARIANT", "files")
                    .env("MODULE_CHECK_FILES", &files)
                    .env("STEEL_SEARCH_PATHS", files.join("search"))
                    .env("STEEL_BOOT_IMAGE_CACHE", "1")
                    .env("STEEL_BOOT_IMAGE_DIR", dir.join("cache"))
                    .env("STEEL_SEPARATE_MODULES", "1")
                    .env("STEEL_MODULE_CACHE", "1")
                    .status()
                    .unwrap();
                assert!(status.success(), "the {role} process failed");
                let text = std::fs::read_to_string(&out).unwrap();
                let (counts, rest) = text.split_once('\n').unwrap();
                (counts.split_once(' ').unwrap().1.parse().unwrap(), rest.to_string())
            };
            let (_, found) = run_files("files-search");
            assert!(found.contains("from-search"), "{found}");
            let (read, found) = run_files("files-search-again");
            assert_eq!(read, 2, "both are read");
            assert!(found.contains("from-search"), "{found}");
            std::fs::write(files.join("project/b.scm"), "(provide b) (define (b) 'from-project)")
                .unwrap();
            let (_, found) = run_files("files-project");
            assert!(found.contains("from-project"), "the file found first now is used: {found}");
        }

        // A newer version read replaces what an older one recorded here
        let (kept, _, shape) = run("shape-keep", true, "shape-keep");
        assert_eq!(kept, 1);
        assert!(shape.contains("(1 2 3)"), "{shape}");
        let (_, read, shape) = run("shape", true, "shape");
        assert_eq!(read, 1);
        assert!(shape.contains("(1 2 3)"), "{shape}");

        let _ = std::fs::remove_dir_all(&dir);
    }
}
