//! Every `STEEL_*` tuning and debugging switch, in one place.
//!
//! Each setting is declared once below with its field, type, default, environment
//! variable and description, and read once, the first time anything asks for
//! [`config()`]. Code reads the field (`config().jit_frameless`) instead of the environment.
//!
//! Parsing is the same for every setting of a type:
//! - `bool`: unset means the default; `0`, `false`, `off` or `no` mean off, and
//!   anything else, including an empty value, means on.
//! - numbers: parsed after trimming; a value that doesn't parse means the default.
//! - `Option<String>`: the raw value, when set.
//! - `Option<Vec<u32>>` / `Option<Vec<String>>`: a comma separated list, when set.
//!
//! `STEEL_HOME`, `STEEL_LSP_HOME` and `STEEL_SEARCH_PATHS` are not here since they
//! are search paths, and other crates read them too.
//!
//! [`describe`] lists every setting with its current value.

/// How one type of setting is read from its environment variable.
pub trait ConfigValue: Sized {
    fn parse(raw: &str) -> Option<Self>;
}

impl ConfigValue for bool {
    fn parse(raw: &str) -> Option<Self> {
        Some(!matches!(
            raw.trim().to_ascii_lowercase().as_str(),
            "0" | "false" | "off" | "no"
        ))
    }
}

macro_rules! numeric_config_value {
    ($($t:ty),*) => {
        $(impl ConfigValue for $t {
            fn parse(raw: &str) -> Option<Self> {
                raw.trim().parse().ok()
            }
        })*
    };
}

numeric_config_value!(usize, u64);

impl ConfigValue for String {
    fn parse(raw: &str) -> Option<Self> {
        Some(raw.to_string())
    }
}

impl ConfigValue for Vec<u32> {
    fn parse(raw: &str) -> Option<Self> {
        Some(
            raw.split(',')
                .filter_map(|x| x.trim().parse().ok())
                .collect(),
        )
    }
}

impl ConfigValue for Vec<String> {
    fn parse(raw: &str) -> Option<Self> {
        Some(
            raw.split(',')
                .map(str::trim)
                .filter(|x| !x.is_empty())
                .map(str::to_string)
                .collect(),
        )
    }
}

impl<T: ConfigValue> ConfigValue for Option<T> {
    fn parse(raw: &str) -> Option<Self> {
        T::parse(raw).map(Some)
    }
}

fn read<T: ConfigValue>(var: &str, default: T) -> T {
    match std::env::var(var) {
        Ok(raw) => T::parse(&raw).unwrap_or(default),
        Err(_) => default,
    }
}

/// One setting, as [`describe`] reports it.
pub struct Setting {
    pub var: &'static str,
    pub default: &'static str,
    pub current: String,
    pub doc: &'static str,
}

macro_rules! steel_config {
    ($(
        $(#[doc = $doc:literal])*
        $field:ident : $ty:ty = $default:expr, $var:literal;
    )*) => {
        /// The process's settings; see the module docs.
        #[derive(Debug, Clone)]
        pub struct Config {
            $(
                $(#[doc = $doc])*
                #[doc = concat!("\n\n`", $var, "`, default `", stringify!($default), "`.")]
                pub $field: $ty,
            )*
        }

        impl Config {
            /// Read every setting from the environment.
            pub fn from_env() -> Self {
                Self {
                    $($field: read::<$ty>($var, $default),)*
                }
            }

            /// Every setting and its value.
            pub fn settings(&self) -> Vec<Setting> {
                vec![$(
                    Setting {
                        var: $var,
                        default: stringify!($default),
                        current: format!("{:?}", self.$field),
                        doc: concat!($($doc, "\n",)*).trim_ascii(),
                    },
                )*]
            }
        }
    };
}

steel_config! {
    // Jit

    /// Compile functions with the jit.
    jit: bool = true, "STEEL_JIT";
    /// Compile on a background thread while the interpreter runs; off compiles
    /// each function as it is defined. Turn off when bisecting a miscompile or
    /// profiling compiled code, so which code is native doesn't depend on timing.
    jit_background: bool = true, "STEEL_JIT_BACKGROUND";
    /// Run cranelift's verifier on everything the jit emits. Slow on compile
    /// heavy programs.
    jit_verifier: bool = true, "STEEL_JIT_VERIFIER";
    /// Size of the code arena in MiB (default 1024, 256 on windows; at most 2047).
    jit_memory_size: Option<usize> = None, "STEEL_JIT_MEMORY_SIZE";
    /// Past this many speculation exits a function is recompiled without
    /// speculating.
    jit_deopt_limit: u64 = 64, "STEEL_JIT_DEOPT_LIMIT";

    // Jit bisecting and dumping

    /// Log every compile candidate by index and id.
    jit_log: bool = false, "STEEL_JIT_LOG";
    /// Log the names of the functions being compiled.
    jit_log_names: bool = false, "STEEL_JIT_LOG_NAMES";
    /// Compile only the first n candidates (bisect on this).
    jit_max: Option<usize> = None, "STEEL_JIT_MAX";
    /// Compile only these function ids.
    jit_only: Option<Vec<u32>> = None, "STEEL_JIT_ONLY";
    /// Never compile these function ids.
    jit_skip: Option<Vec<u32>> = None, "STEEL_JIT_SKIP";
    /// Print the bytecode of these function ids before compiling them.
    jit_dump_bc: Option<Vec<u32>> = None, "STEEL_JIT_DUMP_BC";
    /// Print the bytecode of these function ids as the code generator sees it.
    jit_dump_id: Option<Vec<u32>> = None, "STEEL_JIT_DUMP_ID";
    /// Print the cranelift ir of functions whose name contains this; `1` prints
    /// only functions the verifier rejected.
    jit_dump_clif: Option<String> = None, "STEEL_JIT_DUMP_CLIF";
    /// Print the machine code of functions whose name contains this.
    jit_dump_asm: Option<String> = None, "STEEL_JIT_DUMP_ASM";
    /// Append compiled functions to /tmp/perf-<pid>.map for `perf report`.
    jit_perf_map: bool = false, "STEEL_JIT_PERF_MAP";
    /// Write jit-<pid>.dump for `perf inject --jit`.
    jit_dump: bool = false, "STEEL_JIT_DUMP";
    /// Trace entries into and exits out of native code.
    jit_trace: bool = false, "STEEL_JIT_TRACE";
    /// Report a mismatch between a native branch and the interpreter's.
    jit_branch_check: bool = false, "STEEL_JIT_BRANCH_CHECK";
    /// Tally what the global tail call deopt handler dispatches on.
    deopt_census: bool = false, "STEEL_DEOPT_CENSUS";
    /// Report native stack use at the trampoline.
    stack_debug: bool = false, "STEEL_STACK_DEBUG";

    // Jit calls and the stack

    /// Check the remaining native stack on calls instead of capping at 100 vm
    /// frames.
    jit_stack_check: bool = true, "STEEL_JIT_STACK_CHECK";
    /// Native stack the check allows, in MiB, measured from where it is first
    /// taken. Defaults to what the thread has.
    jit_stack_budget: Option<usize> = None, "STEEL_JIT_STACK_BUDGET";
    /// Self-recursive calls that push no vm frame.
    jit_frameless: bool = true, "STEEL_JIT_FRAMELESS";
    /// Direct calls to global closures proven never `set!`.
    jit_callglobal_direct: bool = true, "STEEL_JIT_CALLGLOBAL_DIRECT";
    /// Direct (address baked) global tail calls.
    jit_inline_tail_call: bool = true, "STEEL_JIT_INLINE_TAIL_CALL";
    /// A run time compiled check on a direct global tail call whose callee
    /// wasn't compiled yet.
    jit_late_tail: bool = true, "STEEL_JIT_LATE_TAIL";
    /// Build rest lists at call sites of variadic functions.
    jit_rest_args: bool = true, "STEEL_JIT_REST_ARGS";

    // Jit primitive inlining

    /// Inline primitive calls in tail position.
    jit_prim_tail: bool = true, "STEEL_JIT_PRIM_TAIL";
    /// The list predicates, `cdr` and `mutable-vector` in tail position.
    jit_prim_tail2: bool = true, "STEEL_JIT_PRIM_TAIL2";
    /// Of those, the predicates.
    jit_tail_pred: bool = true, "STEEL_JIT_TAIL_PRED";
    /// Of those, `cdr`.
    jit_tail_cdr: bool = true, "STEEL_JIT_TAIL_CDR";
    /// Of those, `reverse`.
    jit_tail_rev: bool = true, "STEEL_JIT_TAIL_REV";
    /// Of those, `mutable-vector`.
    jit_tail_vec: bool = true, "STEEL_JIT_TAIL_VEC";
    /// `eq?`, `vector-set!` and `#%make-mutable-struct` in tail position.
    jit_prim_tail_extra: bool = true, "STEEL_JIT_PRIM_TAIL_EXTRA";
    /// Inline float arithmetic.
    jit_float_inline: bool = true, "STEEL_JIT_FLOAT_INLINE";
    /// Inline comparisons and `vector-ref` on arbitrary values.
    jit_generic_inline: bool = true, "STEEL_JIT_GENERIC_INLINE";
    /// Inline the integer division operators.
    jit_inline_divmod: bool = true, "STEEL_JIT_INLINE_DIVMOD";
    /// Inline the bitwise operators on two fixnums.
    jit_inline_bitwise: bool = true, "STEEL_JIT_INLINE_BITWISE";
    /// Inline `floor`, `ceiling`, `truncate`, `round` and `exact`.
    jit_inline_rounding: bool = true, "STEEL_JIT_INLINE_ROUNDING";
    /// Inline `eq?` on two non-constant operands.
    jit_inline_eq: bool = true, "STEEL_JIT_INLINE_EQ";
    /// Inline `bytes-ref` / `bytes-set!`.
    jit_inline_bytevector: bool = true, "STEEL_JIT_INLINE_BYTEVECTOR";
    /// Make bytevector lock elision also require a reference count of one.
    jit_bytevector_strict_unshared: bool = false, "STEEL_JIT_BYTEVECTOR_STRICT_UNSHARED";
    /// Inline the `SETBOX` opcode.
    jit_inline_setbox: bool = true, "STEEL_JIT_INLINE_SETBOX";
    /// Inline heap reference clones instead of calling `clone_one`.
    jit_inline_weak_clone: bool = true, "STEEL_JIT_INLINE_WEAK_CLONE";
    /// Leave values read out of containers borrowed until a consumer needs
    /// them owned.
    jit_borrow: bool = true, "STEEL_JIT_BORROW";
    /// Keep a pushed closed global as a reference to its slot until it is used.
    jit_closed_global_operands: bool = true, "STEEL_JIT_CLOSED_GLOBAL_OPERANDS";
    /// Inline type predicates.
    jit_predicate: bool = true, "STEEL_JIT_PREDICATE";
    /// Inline `pair?` on a local, and let `car` of a local known to be a pair
    /// or non-empty list skip its checks.
    jit_inline_pair_p: bool = true, "STEEL_JIT_INLINE_PAIR_P";
    /// Inline mutable struct getters.
    jit_getter_proto: bool = true, "STEEL_JIT_GETTER_PROTO";
    /// Read a mutable struct field straight through its box.
    jit_fuse_getter_unbox: bool = true, "STEEL_JIT_FUSE_GETTER_UNBOX";
    /// Type the result of a primitive call by the return type its definition
    /// declares.
    jit_primitive_types: bool = true, "STEEL_JIT_PRIMITIVE_TYPES";
    /// After a primitive call returns normally, narrow the locals passed to it
    /// to the argument types its definition declares (`args(..)`).
    jit_primitive_arg_types: bool = true, "STEEL_JIT_PRIMITIVE_ARG_TYPES";
    /// Log each generic primitive call site the jit compiles, with the type
    /// its result was given.
    jit_primitive_types_log: bool = false, "STEEL_JIT_PRIMITIVE_TYPES_LOG";
    /// Treat a value's inferred type as proof of its tag, so tag checks on it
    /// fold away.
    jit_proven_tags: bool = true, "STEEL_JIT_PROVEN_TAGS";

    // Jit specialization and experiments

    /// Type specialized copies of loops and self-recursive functions.
    jit_specialize_loops: bool = true, "STEEL_JIT_SPECIALIZE_LOOPS";
    /// Log specialization seeds and results.
    jit_spec_debug: bool = false, "STEEL_JIT_SPEC_DEBUG";
    /// Specialize only functions whose name contains one of these.
    jit_spec_only: Option<Vec<String>> = None, "STEEL_JIT_SPEC_ONLY";
    /// Don't specialize functions whose name contains one of these.
    jit_spec_skip: Option<Vec<String>> = None, "STEEL_JIT_SPEC_SKIP";
    /// Speculate on integer tags (experimental).
    jit_speculate_int: bool = false, "STEEL_JIT_SPECULATE_INT";
    /// Cache the value stack's buffer pointer (experimental).
    jit_buf_ptr_cache: bool = false, "STEEL_JIT_BUF_PTR_CACHE";
    /// Cache let-variable reads (experimental).
    jit_letvar_cache: bool = false, "STEEL_JIT_LETVAR_CACHE";
    /// Lazy basic block versioning (experimental).
    lbbv: bool = false, "STEEL_LBBV";
    /// Unboxed list cursors in loops (experimental).
    jit_list_cursor: bool = false, "STEEL_JIT_LIST_CURSOR";
    /// Cursor only functions whose name contains one of these (comma separated).
    cursor_only: Option<String> = None, "STEEL_CURSOR_ONLY";
    /// Why a function kept or lost its cursors.
    cursor_stats: bool = false, "STEEL_CURSOR_STATS";
    /// Where cursors were seeded and which join lost one.
    cursor_trace: bool = false, "STEEL_CURSOR_TRACE";
    /// The bytecode of functions given cursors.
    cursor_dump: bool = false, "STEEL_CURSOR_DUMP";

    // Compiler

    /// Inline across module boundaries.
    module_inline: bool = true, "STEEL_MODULE_INLINE";
    /// Lift closures into top level functions.
    closure_lifting: bool = true, "STEEL_CLOSURE_LIFTING";
    /// Lift closures in place.
    lift_in_place: bool = true, "STEEL_LIFT_IN_PLACE";
    /// Drop captures a lifted closure never uses.
    lift_dead_captures: bool = true, "STEEL_LIFT_DEAD_CAPTURES";
    /// Log closure lifting.
    lift_debug: bool = false, "STEEL_LIFT_DEBUG";
    /// An extra inlining round with a size budget of 75.
    inline: bool = false, "STEEL_INLINE";
    /// Unroll recursive functions: bare presence unrolls 2 levels, a number that
    /// many.
    inline_recursive: Option<String> = None, "STEEL_INLINE_RECURSIVE";
    /// Size budget for inlining a recursive function into itself.
    inline_recursive_size: usize = 75, "STEEL_INLINE_RECURSIVE_SIZE";
    /// Extra inlining rounds: `1` for one round at 25, or comma separated sizes.
    inline_more: Option<String> = None, "STEEL_INLINE_MORE";
    /// Re-snapshot inlined bodies after they change.
    inline_fresh_snapshots: bool = true, "STEEL_INLINE_FRESH_SNAPSHOTS";
    /// Inline a function because a call site passes a known function for a
    /// parameter it calls.
    inline_known_function_args: bool = true, "STEEL_INLINE_KNOWN_FUNCTION_ARGS";
    /// Size budget for that.
    inline_known_args_size: usize = 150, "STEEL_INLINE_KNOWN_ARGS_SIZE";
    /// Primitive density threshold for inlining.
    inline_prim_density: usize = 5, "STEEL_INLINE_PRIM_DENSITY";
    /// Resolve defines that alias another function.
    resolve_function_aliases: bool = true, "STEEL_RESOLVE_FUNCTION_ALIASES";
    /// Substitute lambdas bound by `let` into their call sites.
    substitute_lambdas: bool = true, "STEEL_SUBSTITUTE_LAMBDAS";
    /// Drop the binding `or` expands to where it is dead.
    simplify_or: bool = true, "STEEL_SIMPLIFY_OR";
    /// Refer to builtins in programs by their `#%prim.` names, as modules do.
    rewrite_program_builtins: bool = true, "STEEL_REWRITE_PROGRAM_BUILTINS";
    /// Remove the `(let () body)` shells copy propagation leaves.
    flatten_empty_lets: bool = true, "STEEL_FLATTEN_EMPTY_LETS";
    /// Bind `(car x)` before a recursive call passing `(cdr x)`.
    hoist_car: bool = true, "STEEL_HOIST_CAR";
    /// Hoist at every call, not only in tail position.
    hoist_car_all: bool = false, "STEEL_HOIST_CAR_ALL";
    /// Log hoisted sites.
    hoist_car_debug: bool = false, "STEEL_HOIST_CAR_DEBUG";
    /// Order top level defines callee first.
    topo_defines: bool = true, "STEEL_TOPO_DEFINES";
    /// Log the ordering.
    topo_log: bool = false, "STEEL_TOPO_LOG";
    /// Lower acyclic internal defines sequentially rather than as `letrec`.
    sequential_defines: bool = true, "STEEL_SEQUENTIAL_DEFINES";
    /// Keep module globals that are reassigned with `set!` in boxes.
    box_module_globals: bool = true, "STEEL_BOX_MODULE_GLOBALS";
    /// Log internal defines that couldn't be lowered sequentially.
    seq_defines_log: bool = false, "STEEL_SEQ_DEFINES_LOG";
    /// The flat vector representation.
    flat_vectors: bool = false, "STEEL_FLAT_VECTORS";
    /// Print the program after optimization.
    debug_ast: bool = false, "STEEL_DEBUG_AST";
    /// Print the program as inlining left it, before closure lifting.
    debug_ast_pre_lift: bool = false, "STEEL_DEBUG_AST_PRE_LIFT";

    // Gc

    /// Allocation work per unit of collection work.
    gc_work_ratio: usize = 16, "STEEL_GC_WORK_RATIO";
    /// How much a weak collection must reclaim, as a fraction of the list it
    /// walks, to be repeated.
    gc_weak_yield: usize = 32, "STEEL_GC_WEAK_YIELD";
    /// Ceiling on the heap's headroom, in slots.
    gc_max_headroom: usize = 256 * 100 * 128, "STEEL_GC_MAX_HEADROOM";
    /// Threads marking in parallel (0 means 1).
    gc_mark_threads: usize = 1, "STEEL_GC_MARK_THREADS";
}

static CONFIG: std::sync::LazyLock<Config> = std::sync::LazyLock::new(Config::from_env);

/// The settings, read from the environment the first time this is called.
#[inline]
pub fn config() -> &'static Config {
    &CONFIG
}

/// Every setting as `VAR = current (default ...)`, one per line.
pub fn describe() -> String {
    let mut out = String::new();
    for setting in config().settings() {
        out.push_str(&format!(
            "{} = {} (default {})\n",
            setting.var, setting.current, setting.default
        ));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bools_are_on_unless_turned_off() {
        for off in ["0", "false", "FALSE", "off", "no", " 0 "] {
            assert_eq!(bool::parse(off), Some(false), "{off:?}");
        }
        for on in ["1", "true", "", "yes", "anything"] {
            assert_eq!(bool::parse(on), Some(true), "{on:?}");
        }
    }

    #[test]
    fn numbers_fall_back_to_the_default() {
        assert_eq!(usize::parse(" 42 "), Some(42));
        assert_eq!(usize::parse("lots"), None);
        assert_eq!(read::<usize>("STEEL_CONFIG_TEST_UNSET_VARIABLE", 7), 7);
    }

    #[test]
    fn lists_split_on_commas() {
        assert_eq!(Vec::<u32>::parse("1, 2,x,3"), Some(vec![1, 2, 3]));
        assert_eq!(
            Vec::<String>::parse("a, b,,c"),
            Some(vec!["a".to_string(), "b".to_string(), "c".to_string()])
        );
        assert_eq!(Option::<Vec<u32>>::parse(""), Some(Some(vec![])));
    }

    #[test]
    fn every_setting_is_listed_once() {
        let settings = Config::from_env().settings();
        let mut vars: Vec<_> = settings.iter().map(|s| s.var).collect();
        vars.sort_unstable();
        let count = vars.len();
        vars.dedup();
        assert_eq!(vars.len(), count, "a variable is declared twice");
        assert!(settings.iter().all(|s| s.var.starts_with("STEEL_")));
    }
}
