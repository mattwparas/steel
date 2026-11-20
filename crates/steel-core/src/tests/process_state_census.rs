// Every process-wide value (statics with interior mutability, thread-locals) in
// steel-core and steel-parser, and what a boot image does about it. An image
// made in one process is read in another, so state that compiling the stdlib
// changes has to be saved with it; anything else needs a reason it doesn't.
// A new static fails this test until it is listed here.

use self::Kind::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    /// Written into the image and restored when it's read (`SavedProcess`, the
    /// expander state, the source table)
    Saved,
    /// Interns what's saved: the image holds the values, interned again on load
    ByValue,
    /// Reset by each compile, or only set while one runs
    PerCompile,
    /// Remembers results of a function of its key; a fresh process recomputes them
    Cache,
    /// Made again in every process or engine: images, builtin modules, struct
    /// types (made by running code, which a replay runs again)
    Remade,
    /// The same in every process once made
    Constant,
    /// Settings, the host's registrations, debugging and tests
    Host,
    /// Values, the heap, threads, ports, the jit: not changed by compiling
    Runtime,
}

const CENSUS: &[(&str, &str, Kind)] = &[
    // Only set while an image is written, read or compared
    ("steel-parser/tables.rs", "COMPARING", PerCompile),
    ("steel-parser/tables.rs", "READING", PerCompile),
    ("steel-parser/tables.rs", "RELOCATING", PerCompile),
    ("steel-parser/tables.rs", "WRITING", PerCompile),
    ("config.rs", "CONFIG", Host),
    ("env.rs", "UNDEFINED_GLOBAL", Constant),
    ("env.rs", "NEXT", Runtime),
    ("env.rs", "DEFAULT_ENV", Constant),
    ("gc.rs", "NURSERY", Runtime),
    ("gc.rs", "OBJECT_COUNT", Runtime),
    ("rvals.rs", "ITERATOR_FINISHED", Constant),
    ("rvals.rs", "HASH_VISITS", Runtime),
    ("rvals.rs", "HASH_DEPTH", Runtime),
    ("rvals.rs", "SHARED_HASHES", Runtime),
    ("time.rs", "TIME_PROVIDER", Host),
    ("value_type.rs", "CUSTOM_TYPES", Cache),
    ("value_type.rs", "NOMINAL_SETS", Cache),
    ("rvals/cycles.rs", "DROP_BUFFER", Runtime),
    ("rvals/cycles.rs", "PENDING", Runtime),
    ("rvals/cycles.rs", "FORMAT_BUFFER", Runtime),
    ("rvals/cycles.rs", "LEFT_QUEUE", Runtime),
    ("rvals/cycles.rs", "RIGHT_QUEUE", Runtime),
    ("rvals/cycles.rs", "VISITED_SET", Runtime),
    ("rvals/cycles.rs", "VISITED_NESTED", Runtime),
    ("rvals/cycles.rs", "EQ_DEPTH", Runtime),
    ("rvals/cycles.rs", "DROP_THREAD", Runtime),
    ("rvals/serialization.rs", "ORIGIN", Host),
    ("rvals/serialization.rs", "APART", Runtime),
    ("core/labels.rs", "LABEL_ID", PerCompile),
    ("primitives/git.rs", "INTERRUPT", Runtime),
    ("primitives/polling.rs", "EVENTS", Runtime),
    ("primitives/polling.rs", "KEY_ID", Runtime),
    ("primitives/ports.rs", "EOF_OBJECT", Constant),
    ("primitives/ports.rs", "EXISTS_FLAG", Constant),
    ("primitives/strings.rs", "COMPILED", Cache),
    ("compiler/code_gen.rs", "FUNCTION_ID", Saved),
    // Only while a module's code is put together on its own
    ("compiler/code_gen.rs", "LOCAL_FUNCTION_IDS", PerCompile),
    // The module cache's: counters of the images read or made (set again by each),
    // the build key, a pause while an image is made, test counters
    ("compiler/module_cache.rs", "IMAGE_COUNTERS", Host),
    ("compiler/module_cache.rs", "KEY", Constant),
    ("compiler/module_cache.rs", "PAUSED", PerCompile),
    ("compiler/module_cache.rs", "GENERATING", PerCompile),
    ("compiler/module_cache.rs", "WRITES", Host),
    // The experimental compiled code cache's directory and temp file counter
    ("jit2/code_cache.rs", "RUNNING", PerCompile),
    ("jit2/code_cache.rs", "WRITES", Host),
    ("compiler/module_cache.rs", "KEPT", Host),
    ("compiler/module_cache.rs", "READ", Host),
    ("compiler/passes/analysis.rs", "GENERATED_NAME_SUFFIX", PerCompile),
    ("compiler/modules.rs", "REQUIRE_CONTEXTS", PerCompile),
    ("compiler/modules.rs", "STEEL_SEARCH_PATHS", Host),
    ("compiler/modules.rs", "STEEL_HOME", Host),
    ("compiler/modules.rs", "TEMPLATE_PREFIXES", Saved),
    ("compiler/modules.rs", "MODULE_TEMPLATE_PREFIXES", PerCompile),
    ("compiler/modules.rs", "MODULE_PREFIXES", Saved),
    ("compiler/modules.rs", "BACKGROUND_DESERIALIZING", Cache),
    ("compiler/modules.rs", "PHASE1_INSTANCES", Saved),
    ("compiler/passes/analysis.rs", "BUILTIN", Cache),
    ("compiler/passes/analysis.rs", "SET_BOX_VALUE_COUNTER", Saved),
    ("compiler/passes/analysis.rs", "CAR_HOIST_COUNTER", Saved),
    ("compiler/passes/analysis.rs", "FRESHEN_COUNTER", Saved),
    ("compiler/passes/pass_queries.rs", "FACTS", PerCompile),
    ("compiler/passes/scopes_check.rs", "COMPARING", Host),
    ("compiler/passes/steel_passes.rs", "IN_PASS_ENGINE", PerCompile),
    ("compiler/passes/steel_passes.rs", "RUST_PASSES_ONLY", PerCompile),
    ("compiler/passes/steel_passes.rs", "SHARED_ENGINE", Cache),
    ("compiler/passes/types.rs", "CHECKED", Cache),
    ("compiler/passes/types.rs", "N", PerCompile),
    ("compiler/passes/types.rs", "STRUCT_NAMES", Host),
    ("compiler/passes/types.rs", "VIOLATIONS", Host),
    ("compiler/passes/types.rs", "CHECKS_RUN", Host),
    ("tests/history.rs", "TICKS", Host),
    ("parser/expand_visitor.rs", "COMPILES", PerCompile),
    ("parser/expand_visitor.rs", "FOREIGN_EXPANSIONS", Saved),
    ("parser/expand_visitor.rs", "PROCEDURAL_HEADS", PerCompile),
    ("parser/expand_visitor.rs", "FORM_SCOPES", PerCompile),
    ("parser/expand_visitor.rs", "LAST_USE_SITE", PerCompile),
    ("parser/expand_visitor.rs", "BODY_USE_SITES", PerCompile),
    ("parser/expand_visitor.rs", "BINDER_TABLES", PerCompile),
    ("parser/expand_visitor.rs", "MODULE_BINDINGS", Saved),
    ("parser/expand_visitor.rs", "CURRENT_MODULE", PerCompile),
    ("parser/expand_visitor.rs", "LITERAL_MODULE", PerCompile),
    ("parser/expand_visitor.rs", "USE_CHAIN", PerCompile),
    ("parser/expand_visitor.rs", "LITERAL_SCOPES", PerCompile),
    ("parser/expand_visitor.rs", "DEFINED_NAMES", Saved),
    ("parser/expand_visitor.rs", "BINDING_FORMS", Constant),
    ("parser/expand_visitor.rs", "IDENTIFIER_SYNTAX", Constant),
    ("parser/expand_visitor.rs", "PATHS", PerCompile),
    ("parser/expand_visitor.rs", "SUFFIXES", PerCompile),
    ("parser/expand_visitor.rs", "ALIASES", Cache),
    ("parser/expand_visitor.rs", "LOCAL_MACRO_NAMES", Saved),
    ("parser/expand_visitor.rs", "STRUCT_INFO", Saved),
    ("parser/expand_visitor.rs", "REGISTERED_PROCEDURAL", Saved),
    ("parser/expand_visitor.rs", "PRELUDE_ALIASES", Saved),
    ("parser/expand_visitor.rs", "NO_EXCLUSIONS", Constant),
    ("parser/expander.rs", "BINDINGS", PerCompile),
    ("parser/expander.rs", "BINDINGS_KIND", PerCompile),
    ("parser/expander.rs", "FALLBACK_BINDINGS", PerCompile),
    ("parser/kernel.rs", "KERNEL_IMAGE", Remade),
    ("parser/kernel.rs", "KERNEL_IMAGE_SB", Remade),
    ("parser/kernel.rs", "RECORD_KERNEL_ENGINE", PerCompile),
    ("parser/kernel.rs", "MACRO_INPUTS", PerCompile),
    ("parser/kernel.rs", "STATIC_KERNEL_IMAGE", Remade),
    ("parser/kernel.rs", "STATIC_KERNEL_IMAGE_SB", Remade),
    ("parser/parser.rs", "LAMBDA_SYMBOL", Constant),
    ("parser/parser.rs", "GLOBAL_SOURCE_MAPPING", Saved),
    ("parser/spec_requests.rs", "ANSWERS", PerCompile),
    ("parser/spec_requests.rs", "REQUESTS", PerCompile),
    ("parser/spec_requests.rs", "ASKING", PerCompile),
    ("values/closed.rs", "ROOTS", Runtime),
    ("values/closed.rs", "SPARE_BOXES", Runtime),
    ("values/closed.rs", "SPARE_STRUCTS", Runtime),
    ("values/closed.rs", "MY_SAFEPOINT_SLOT", Runtime),
    ("values/closed.rs", "MY_RUNNING", Runtime),
    ("values/closed.rs", "DEFERRED_DROPS", Runtime),
    ("values/closed.rs", "RELEASE_DEPTH", Runtime),
    ("values/closed.rs", "BORROWS", Runtime),
    ("values/closed.rs", "GLOBAL_ROOTS", Runtime),
    ("values/closed.rs", "MARKER", Runtime),
    ("values/closed.rs", "WILL_EXECUTORS", Runtime),
    ("values/closed.rs", "WILL_TURN", Runtime),
    ("values/closed.rs", "WATCHED", Runtime),
    ("values/closed.rs", "WATCHING", Runtime),
    ("values/closed.rs", "WEAK_BOXES", Runtime),
    ("values/closed.rs", "WEAKLY_BOXED", Runtime),
    ("values/closed.rs", "NEXT", Runtime),
    ("values/closed.rs", "CELLS_SHARED", Runtime),
    ("values/closed.rs", "AWAITED", Runtime),
    ("values/closed.rs", "MUTATORS", Runtime),
    ("values/closed.rs", "ORPHANED_DROPS", Runtime),
    ("values/closed.rs", "DETACHED_STRUCTS", Runtime),
    ("values/closed.rs", "THREADS", Runtime),
    ("values/lists.rs", "DEPTH", Runtime),
    ("values/port.rs", "DEFAULT_OUTPUT_PORT", Runtime),
    ("values/port.rs", "CAPTURED_OUTPUT_PORT", Runtime),
    ("values/port.rs", "WOULD_BLOCK_OBJECT", Constant),
    ("values/recycler.rs", "RECYCLE_LIMIT", Runtime),
    ("values/serde.rs", "SERDE_HANDLERS", Host),
    ("values/structs.rs", "STRUCT_MAP", Remade),
    ("values/structs.rs", "TRANSPARENT_KEY", Constant),
    ("values/structs.rs", "MUTABLE_KEY", Constant),
    ("values/structs.rs", "FIELDS_KEY", Constant),
    ("values/structs.rs", "EQUALITY_SKIP_KEY", Constant),
    ("values/structs.rs", "VTABLE", Remade),
    ("values/structs.rs", "STRUCT_TYPE_COUNT", Remade),
    ("values/structs.rs", "OK_RESULT_LABEL", Constant),
    ("values/structs.rs", "SOME_OPTION_LABEL", Constant),
    ("values/structs.rs", "ERR_RESULT_LABEL", Constant),
    ("values/structs.rs", "NONE_OPTION_LABEL", Constant),
    ("values/structs.rs", "TYPE_ID", Constant),
    ("values/structs.rs", "STRUCT_DEFINITIONS", Remade),
    ("values/structs.rs", "STATIC_VTABLE", Remade),
    ("values/structs.rs", "STATIC_TRANSPARENT_KEY", Constant),
    ("values/structs.rs", "STATIC_MUTABLE_KEY", Constant),
    ("values/structs.rs", "STATIC_FIELDS_KEY", Constant),
    ("values/structs.rs", "STATIC_EQUALITY_SKIP_KEY", Constant),
    ("values/structs.rs", "STATIC_OK_DESCRIPTOR", Constant),
    ("values/structs.rs", "STATIC_ERR_DESCRIPTOR", Constant),
    ("values/structs.rs", "STATIC_SOME_DESCRIPTOR", Constant),
    ("values/structs.rs", "STATIC_NONE_DESCRIPTOR", Constant),
    ("values/structs.rs", "DEFAULT_PROPERTIES", Constant),
    ("values/structs.rs", "STANDARD_OPTIONS", Constant),
    ("values/structs.rs", "OPTION_OPTIONS", Constant),
    ("values/structs.rs", "OK_DESCRIPTOR", Remade),
    ("values/structs.rs", "ERR_DESCRIPTOR", Remade),
    ("values/structs.rs", "SOME_DESCRIPTOR", Remade),
    ("values/structs.rs", "NONE_DESCRIPTOR", Remade),
    ("values/structs.rs", "OK_CONSTRUCTOR", Remade),
    ("values/structs.rs", "ERR_CONSTRUCTOR", Remade),
    ("values/structs.rs", "SOME_CONSTRUCTOR", Remade),
    ("values/structs.rs", "NONE_CONSTRUCTOR", Remade),
    ("jit2/cgen.rs", "BEFORE", Runtime),
    ("jit2/cgen.rs", "DEFERRED_LEAKS", Runtime),
    ("jit2/cgen.rs", "SEEN", Runtime),
    ("jit2/cgen.rs", "APPLIED", Runtime),
    ("jit2/cgen.rs", "SPECULATION_DEOPTS", Runtime),
    ("jit2/cgen.rs", "SPECULATION_DISABLED", Runtime),
    ("jit2/cgen/inline_primitives.rs", "TABLE", Constant),
    ("steel_vm/builtin.rs", "FUNCTION_TABLE", Remade),
    ("steel_vm/builtin.rs", "MODULE_GET", Constant),
    ("steel_vm/builtin.rs", "VOID", Constant),
    ("steel_vm/builtin.rs", "GET_DYLIB", Constant),
    ("steel_vm/builtin.rs", "VOID_MODULE", Constant),
    ("steel_vm/builtin.rs", "STATIC_FUNCTION_TABLE", Remade),
    ("steel_vm/builtin.rs", "GENSYM", Host),
    ("steel_vm/dylib.rs", "BUILT_DYLIBS", Host),
    ("steel_vm/dylib.rs", "LOADED_MODULES", Host),
    ("steel_vm/dylib.rs", "STATIC_BUILT_DYLIBS", Host),
    ("steel_vm/engine.rs", "KERNEL_BIN_FILE", Host),
    ("steel_vm/engine.rs", "PREPARING_PASS_IMAGE", PerCompile),
    ("steel_vm/engine.rs", "BUILDING_BOOT_IMAGE", PerCompile),
    ("steel_vm/engine.rs", "IMAGE", Remade),
    ("steel_vm/engine.rs", "IMAGES", Remade),
    ("steel_vm/engine.rs", "DEFAULT_PRELUDE_MACROS", Saved),
    ("steel_vm/engine.rs", "CHECKED", PerCompile),
    ("steel_vm/engine.rs", "ENGINE_ID", Runtime),
    ("steel_vm/engine.rs", "IMAGES_READ", Host),
    ("steel_vm/engine.rs", "BOOT_IMAGE_CACHE", Host),
    ("steel_vm/engine.rs", "STATIC_DEFAULT_PRELUDE_MACROS", Saved),
    ("steel_vm/engine.rs", "STATIC_DEFAULT_PRELUDE_MACROS_SANDBOX", Saved),
    ("steel_vm/ffi.rs", "FFI_MODULES", Remade),
    ("steel_vm/host_signature.rs", "WRAPPERS", Remade),
    ("steel_vm/primitives.rs", "MAP_MODULE", Remade),
    ("steel_vm/primitives.rs", "SET_MODULE", Remade),
    ("steel_vm/primitives.rs", "LIST_MODULE", Remade),
    ("steel_vm/primitives.rs", "STRING_MODULE", Remade),
    ("steel_vm/primitives.rs", "VECTOR_MODULE", Remade),
    ("steel_vm/primitives.rs", "IMMUTABLE_VECTOR_MODULE", Remade),
    ("steel_vm/primitives.rs", "BYTEVECTOR_MODULE", Remade),
    ("steel_vm/primitives.rs", "STREAM_MODULE", Remade),
    ("steel_vm/primitives.rs", "IDENTITY_MODULE", Remade),
    ("steel_vm/primitives.rs", "NUMBER_MODULE", Remade),
    ("steel_vm/primitives.rs", "EQUALITY_MODULE", Remade),
    ("steel_vm/primitives.rs", "ORD_MODULE", Remade),
    ("steel_vm/primitives.rs", "TRANSDUCER_MODULE", Remade),
    ("steel_vm/primitives.rs", "SYMBOL_MODULE", Remade),
    ("steel_vm/primitives.rs", "IO_MODULE", Remade),
    ("steel_vm/primitives.rs", "FS_MODULE", Remade),
    ("steel_vm/primitives.rs", "FS_MODULE_SB", Remade),
    ("steel_vm/primitives.rs", "PROCESS_MODULE_SB", Remade),
    ("steel_vm/primitives.rs", "GIT_MODULE_SB", Remade),
    ("steel_vm/primitives.rs", "PORT_MODULE", Remade),
    ("steel_vm/primitives.rs", "PORT_MODULE_WITHOUT_FILESYSTEM", Remade),
    ("steel_vm/primitives.rs", "META_MODULE", Remade),
    ("steel_vm/primitives.rs", "JSON_MODULE", Remade),
    ("steel_vm/primitives.rs", "CONSTANTS_MODULE", Remade),
    ("steel_vm/primitives.rs", "SYNTAX_MODULE", Remade),
    ("steel_vm/primitives.rs", "SANDBOXED_META_MODULE", Remade),
    ("steel_vm/primitives.rs", "PROCESS_MODULE", Remade),
    ("steel_vm/primitives.rs", "RANDOM_MODULE", Remade),
    ("steel_vm/primitives.rs", "RESULT_MODULE", Remade),
    ("steel_vm/primitives.rs", "TYPE_ID_MODULE", Remade),
    ("steel_vm/primitives.rs", "OPTION_MODULE", Remade),
    ("steel_vm/primitives.rs", "TCP_MODULE", Remade),
    ("steel_vm/primitives.rs", "HTTP_MODULE", Remade),
    ("steel_vm/primitives.rs", "POLLING_MODULE", Remade),
    ("steel_vm/primitives.rs", "FFI_MODULE", Remade),
    ("steel_vm/primitives.rs", "PRELUDE_MODULE", Remade),
    ("steel_vm/primitives.rs", "SB_PRELUDE", Remade),
    ("steel_vm/primitives.rs", "TIME_MODULE", Remade),
    ("steel_vm/primitives.rs", "THREADING_MODULE", Remade),
    ("steel_vm/primitives.rs", "MUTABLE_VECTOR_MODULE", Remade),
    ("steel_vm/primitives.rs", "PRIVATE_READER_MODULE", Remade),
    ("steel_vm/primitives.rs", "GIT_MODULE", Remade),
    ("steel_vm/primitives.rs", "HASHES_MODULE", Remade),
    ("steel_vm/primitives.rs", "PRELUDE_INTERNED_STRINGS", Constant),
    ("steel_vm/primitives.rs", "CONSTANT_PRIMITIVES", Constant),
    ("steel_vm/primitives.rs", "STEEL_FFI_MODULE", Constant),
    ("steel_vm/primitives.rs", "MODULE_IDENTIFIERS", Constant),
    ("steel_vm/primitives.rs", "PRELUDE_TO_RESERVED_MAP", Constant),
    ("steel_vm/profiling.rs", "PROFILER", Host),
    ("steel_vm/runtime_fn.rs", "APPLIER", Runtime),
    ("steel_vm/tests.rs", "STARTED", Host),
    ("steel_vm/tests.rs", "RELEASE", Host),
    ("steel_vm/tests.rs", "EXCLUSIVE_STARTED", Host),
    ("steel_vm/tests.rs", "EXCLUSIVE_RELEASE", Host),
    ("steel_vm/vm.rs", "THE_EMPTY_INSTRUCTION_SET", Constant),
    ("steel_vm/vm.rs", "DEFAULT_CONSTANT_MAP", Constant),
    ("steel_vm/vm.rs", "THREADS_MADE", Runtime),
    ("steel_vm/vm.rs", "TN", Runtime),
    ("steel_vm/vm/jit.rs", "BACKGROUND_RETURNS", Runtime),
    ("steel_vm/vm/jit.rs", "BACKGROUND_OUTSTANDING", Runtime),
    ("steel_vm/vm/jit.rs", "SETTLE_AT_TOP_LEVEL", Runtime),
    ("steel_vm/vm/jit.rs", "COUNT", Runtime),
    ("steel_vm/vm/jit.rs", "BACKGROUND_ENTRIES", Runtime),
    ("steel_vm/vm/jit.rs", "INTERPRETER_WAITING_ON_JIT", Runtime),
    ("steel_vm/vm/jit.rs", "BACKGROUND_QUEUE", Runtime),
    ("steel_vm/vm/threads.rs", "EMPTY_CHANNEL_OBJECT", Constant),
    ("steel_vm/vm/threads.rs", "DISCONNECTED_CHANNEL_OBJECT", Constant),
    ("steel_vm/vm/jit/census.rs", "DEOPT_CENSUS", Host),
    ("steel_vm/vm/jit/closures.rs", "N", Runtime),
    ("steel_vm/vm/jit/frames.rs", "FRAMELESS_PENDING", Runtime),
    ("steel-parser/interner.rs", "INTERNER", ByValue),
    ("steel-parser/parser.rs", "SOURCE_ID_COUNTER", Saved),
    ("steel-parser/parser.rs", "SYNTAX_OBJECT_ID", Saved),
    ("steel-parser/scopes.rs", "MEMO", Cache),
    ("steel-parser/scopes.rs", "SCOPE_COUNTER", Saved),
    ("steel-parser/scopes.rs", "MODULES", Saved),
    ("steel-parser/scopes.rs", "SERIAL", Cache),
    ("steel-parser/scopes.rs", "INTERNER", ByValue),
    ("steel-parser/tokens.rs", "NUMBER_INTERNER", ByValue),
];

// The statics a source file declares that can change: thread-locals, and
// statics whose type can be written through
fn statics_in(source: &str) -> Vec<String> {
    const MUTABLE: [&str; 10] = [
        "Atomic", "Mutex", "RwLock", "OnceCell", "OnceLock", "Lazy", "RefCell", "Cell", "SpinLock",
        "ArcSwap",
    ];
    // Comments out, so commented-out code isn't counted
    let source: String = source
        .lines()
        .map(|line| if line.trim_start().starts_with("//") { "" } else { line })
        .collect::<Vec<_>>()
        .join("\n");

    let name_after_static = |text: &str| -> Option<(String, usize)> {
        let rest = text.trim_start().strip_prefix("ref ").unwrap_or(text.trim_start());
        let name: String = rest
            .chars()
            .take_while(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || *c == '_')
            .collect();
        let after = rest[name.len()..].trim_start();
        (!name.is_empty() && after.starts_with(':')).then(|| (name, 0))
    };

    let mut found = Vec::new();
    // Thread-locals: every static between `thread_local! {` and its closing brace
    let mut at = 0;
    while let Some(start) = source[at..].find("thread_local!") {
        let start = at + start;
        let Some(open) = source[start..].find('{') else { break };
        let open = start + open;
        let mut depth = 0;
        let mut close = source.len();
        for (i, c) in source[open..].char_indices() {
            match c {
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        close = open + i;
                        break;
                    }
                }
                _ => {}
            }
        }
        let block = &source[open..close];
        for piece in block.split("static ").skip(1) {
            if let Some((name, _)) = name_after_static(piece) {
                found.push(name);
            }
        }
        at = close;
    }
    // Statics: `static NAME: Type =`, with a type that can be written through
    for (i, line) in source.lines().enumerate() {
        let trimmed = line.trim_start();
        let rest = ["pub(crate) static ", "pub static ", "static "]
            .iter()
            .find_map(|prefix| trimmed.strip_prefix(prefix));
        let Some(rest) = rest else { continue };
        let Some((name, _)) = name_after_static(rest) else { continue };
        // The type, up to the `=` or `;`, maybe over several lines
        let mut ty = String::new();
        for next in source.lines().skip(i).take(4) {
            ty.push_str(next);
            if next.contains('=') || next.contains(';') {
                break;
            }
        }
        let ty = ty.split(['=', ';']).next().unwrap_or("");
        if MUTABLE.iter().any(|marker| ty.contains(marker)) {
            found.push(name);
        }
    }
    found.sort();
    found.dedup();
    found
}

fn walk(dir: &std::path::Path, root: &std::path::Path, prefix: &str, out: &mut Vec<(String, String)>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir).unwrap().filter_map(|e| e.ok()).collect();
    entries.sort_by_key(|e| e.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            walk(&path, root, prefix, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            let source = std::fs::read_to_string(&path).unwrap();
            let relative = path.strip_prefix(root).unwrap().to_string_lossy().replace('\\', "/");
            for name in statics_in(&source) {
                out.push((format!("{prefix}{relative}"), name));
            }
        }
    }
}

#[test]
fn every_process_wide_value_has_a_boot_image_kind() {
    let core = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let parser = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../steel-parser/src");
    let mut found = Vec::new();
    walk(&core, &core, "", &mut found);
    walk(&parser, &parser, "steel-parser/", &mut found);

    let listed: std::collections::HashSet<(String, String)> = CENSUS
        .iter()
        .map(|(file, name, _)| (file.to_string(), name.to_string()))
        .collect();
    let present: std::collections::HashSet<(String, String)> = found.iter().cloned().collect();

    let mut unlisted: Vec<_> = present.difference(&listed).collect();
    unlisted.sort();
    let mut gone: Vec<_> = listed.difference(&present).collect();
    gone.sort();
    assert!(
        unlisted.is_empty() && gone.is_empty(),
        "process-wide values not in the census (add them with what a boot image does about them): {unlisted:?}\n\
         listed but not found: {gone:?}"
    );

    let kinds: Vec<Kind> = CENSUS.iter().map(|(_, _, kind)| *kind).collect();
    assert!(kinds.contains(&Saved) && kinds.contains(&Runtime));
}
