use super::{
    builtin::{Arity, BuiltInModule, MarkdownDoc},
    cache::WeakMemoizationTable,
    engine::Engine,
    register_fn::RegisterFn,
    vm::{
        get_test_mode, list_modules, set_test_mode, VmCore, CALLSTACK_HYDRATE_NAMES_DEFINITION,
        CALL_CC_DEFINITION, CALL_WITH_EXCEPTION_HANDLER_DEFINITION, DUMP_PROFILER_DEFINITION,
        EVAL_DEFINITION, EVAL_FILE_DEFINITION, EVAL_STRING_DEFINITION,
        EXPAND_SYNTAX_CASE_DEFINITION, EXPAND_SYNTAX_OBJECTS_DEFINITION, INSPECT_DEFINITION,
        MACRO_CASE_BINDINGS_DEFINITION, MAKE_CALLSTACK_PROFILER_DEFINITION,
        MATCH_SYNTAX_CASE_DEFINITION, SAMPLE_STACKS_DEFINITION,
    },
};
use crate::{
    compiler::modules::steel_home,
    gc::{shared::ShareableMut, GcMut},
    parser::{
        ast::TryFromSteelValVisitorForExprKind, interner::InternedString, span::Span,
        tryfrom_visitor::TryFromExprKindForSteelVal,
    },
    primitives::{
        bytevectors::bytevector_module,
        fs_module, fs_module_sandbox,
        git::git_module,
        hashes::hashes_module,
        hashmaps::{hashmap_module, HM_CONSTRUCT, HM_GET, HM_INSERT},
        hashsets::hashset_module,
        http::http_module,
        lists::{list_module, UnRecoverableResult},
        numbers::{self, realp},
        port_module,
        ports::{port_module_without_filesystem, EOF_OBJECTP_DEFINITION},
        process::process_module,
        random::random_module,
        string_module, symbol_module,
        tcp::tcp_module,
        time::time_module,
        transducers::transducer_module,
        vectors::{
            immutable_vectors_module, IMMUTABLE_VECTOR_CONSTRUCT_DEFINITION,
            LIST_VEC_NULL_DEFINITION, MAKE_VECTOR_DEFINITION, MUTABLE_VECTOR_CLEAR_DEFINITION,
            MUTABLE_VECTOR_POP_DEFINITION, MUTABLE_VECTOR_TO_STRING_DEFINITION,
            MUT_VECTOR_COPY_DEFINITION, MUT_VEC_APPEND_DEFINITION, MUT_VEC_CONSTRUCT_DEFINITION,
            MUT_VEC_CONSTRUCT_VEC_DEFINITION, MUT_VEC_GET_DEFINITION, MUT_VEC_LENGTH_DEFINITION,
            MUT_VEC_PUSH_DEFINITION, MUT_VEC_SET_DEFINITION, MUT_VEC_SWAP_DEFINITION,
            MUT_VEC_TO_LIST_DEFINITION, VECTOR_FILL_DEFINITION, VEC_APPEND_DEFINITION,
            VEC_CAR_DEFINITION, VEC_CDR_DEFINITION, VEC_CONS_DEFINITION, VEC_LENGTH_DEFINITION,
            VEC_PUSH_DEFINITION, VEC_RANGE_DEFINITION, VEC_REF_DEFINITION,
        },
        ControlOperations, IoFunctions, MetaOperations, StreamOperations,
    },
    rerrs::ErrorKind,
    rvals::{
        as_underlying_type,
        cycles::{BreadthFirstSearchSteelValVisitor, SteelCycleCollector},
        CustomType, FromSteelVal, SteelString, ITERATOR_FINISHED, NUMBER_EQUALITY_DEFINITION,
    },
    steel_vm::{
        builtin::{get_function_metadata, get_function_name, BuiltInFunctionType},
        vm::threads::threading_module,
    },
    values::{
        closed::{HeapRef, MAKE_WEAK_BOX_DEFINITION, WEAK_BOX_VALUE_DEFINITION},
        functions::{attach_contract_struct, get_contract, LambdaMetadataTable},
        lists::{List, SteelList},
        structs::{
            build_type_id_module, make_struct_type, struct_update_primitive, SteelResult,
            UserDefinedStruct,
        },
    },
};

use crate::values::closed::{
    MAKE_WILL_EXECUTOR_DEFINITION, WILL_EXECUTE_DEFINITION, WILL_REGISTER_DEFINITION,
};

use crate::{
    rvals::IntoSteelVal,
    values::structs::{build_option_structs, build_result_structs},
};
use crate::{
    rvals::{Result, SteelVal},
    SteelErr,
};
use compact_str::CompactString;
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use once_cell::sync::Lazy;
use std::{borrow::Cow, cmp::Ordering};
use steel_parser::{ast::ExprKind, interner::interned_current_memory_usage, parser::SourceId};

#[cfg(all(feature = "std", not(target_family = "wasm")))]
use crate::primitives::polling::polling_module;

#[cfg(target_family = "wasm")]
fn polling_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/polling".to_string());

    module
}

#[cfg(feature = "dylibs")]
use crate::steel_vm::ffi::ffi_module;

macro_rules! ensure_tonicity_two {
    ($check_fn:expr) => {{
        |args: &[SteelVal]| -> Result<SteelVal> {

            if args.is_empty() {
                stop!(ArityMismatch => "expected at least one argument");
            }

            for window in args.windows(2) {
                if let &[left, right] = &window {
                    if !$check_fn(&left, &right) {
                        return Ok(SteelVal::BoolV(false))
                    }
                } else {
                    unreachable!()
                }

            }

            Ok(SteelVal::BoolV(true))

        }
    }};
}

macro_rules! gen_pred {
    ($variant:ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            Ok(if let Some(SteelVal::$variant(..)) = args.first() {
                SteelVal::BoolV(true)
            } else {
                SteelVal::BoolV(false)
            })
        })
    }};

    ($variant1:ident, $variant2:ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some(first) = args.first() {
                match first {
                    SteelVal::$variant1(..) | SteelVal::$variant2(..) => {
                        return Ok(SteelVal::BoolV(true));
                    }
                    _ => {}
                }
            }
            Ok(SteelVal::BoolV(false))
        })
    }};

    // TODO replace this with something better
    ($variant1:ident, $variant2:ident, $variant3:ident, $variant4:ident, $variant5:ident, $variant6: ident, $variant7: ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some(first) = args.first() {
                match first {
                    SteelVal::$variant1(..)
                    | SteelVal::$variant2(..)
                    | SteelVal::$variant3(..)
                    | SteelVal::$variant4(..)
                    | SteelVal::$variant5(..)
                    | SteelVal::$variant6(..)
                    | SteelVal::$variant7(..) => {
                        return Ok(SteelVal::BoolV(true));
                    }
                    _ => {}
                }
            }
            Ok(SteelVal::BoolV(false))
        })
    }};

    ($variant1:ident, $variant2:ident, $variant3:ident, $variant4:ident, $variant5:ident, $variant6:ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some(first) = args.first() {
                match first {
                    SteelVal::$variant1(..)
                    | SteelVal::$variant2(..)
                    | SteelVal::$variant3(..)
                    | SteelVal::$variant4(..)
                    | SteelVal::$variant5(..)
                    | SteelVal::$variant6(..) => {
                        return Ok(SteelVal::BoolV(true));
                    }
                    _ => {}
                }
            }
            Ok(SteelVal::BoolV(false))
        })
    }};
}

pub const CONSTANTS: &[&str] = &[
    "#%prim.+",
    "#%prim.i+",
    "#%prim.f+",
    "#%prim.*",
    "#%prim./",
    "#%prim.-",
    "#%prim.car",
    "#%prim.cdr",
    "#%prim.first",
    "#%prim.rest",
    "#%prim.null?",
    "#%prim.int?",
    "#%prim.float?",
    "#%prim.number?",
    "#%prim.string?",
    "#%prim.symbol?",
    "#%prim.vector?",
    "#%prim.list?",
    "#%prim.integer?",
    "#%prim.boolean?",
    "#%prim.=",
    "#%prim.equal?",
    "#%prim.>",
    "#%prim.>=",
    "#%prim.<",
    "#%prim.<=",
    "#%prim.string-append",
    "#%prim.string->list",
    "#%prim.string-upcase",
    "#%prim.string-lowercase",
    "#%prim.trim",
    "#%prim.trim-start",
    "#%prim.trim-end",
    "#%prim.split-whitespace",
    "#%prim.void",
    "#%prim.list->string",
    "#%prim.concat-symbols",
    "#%prim.string->int",
    "#%prim.even?",
    "#%prim.append",
    "#%prim.length",
    "#%prim.list->string",
    "#%prim.not",
];

#[macro_export]
macro_rules! define_modules {
    ($($name:tt => $func:expr,) * ) => {
        $(
            pub static $name: once_cell::sync::Lazy<BuiltInModule> = once_cell::sync::Lazy::new($func);
        )*
    };
}

#[cfg(feature = "sync")]
define_modules! {
    STEEL_MAP_MODULE => hashmap_module,
    STEEL_SET_MODULE => hashset_module,
    STEEL_LIST_MODULE => list_module,
    STEEL_STRING_MODULE => string_module,
    STEEL_VECTOR_MODULE => vector_module,
    STEEL_IMMUTABLE_VECTOR_MODULE => immutable_vectors_module,
    STEEL_BYTEVECTOR_MODULE => bytevector_module,
    STEEL_STREAM_MODULE => stream_module,
    STEEL_IDENTITY_MODULE => identity_module,
    STEEL_NUMBER_MODULE => number_module,
    STEEL_EQUALITY_MODULE => equality_module,
    STEEL_ORD_MODULE => ord_module,
    STEEL_TRANSDUCER_MODULE => transducer_module,
    STEEL_SYMBOL_MODULE => symbol_module,
    STEEL_IO_MODULE => io_module,
    STEEL_FS_MODULE => fs_module,
    STEEL_FS_MODULE_SB => fs_module_sandbox,
    STEEL_PORT_MODULE => port_module,
    STEEL_PORT_WITHOUT_FS_MODULE => port_module_without_filesystem,
    STEEL_META_MODULE => meta_module,
    STEEL_JSON_MODULE => json_module,
    STEEL_CONSTANTS_MODULE => constants_module,
    STEEL_SYNTAX_MODULE => syntax_module,
    STEEL_SANDBOXED_META_MODULE => sandboxed_meta_module,
    STEEL_PROCESS_MODULE => process_module,
    STEEL_RANDOM_MODULE => random_module,
    STEEL_RESULT_MODULE => build_result_structs,
    STEEL_TYPE_ID_MODULE => build_type_id_module,
    STEEL_OPTION_MODULE => build_option_structs,
    STEEL_THREADING_MODULE => threading_module,
    STEEL_TIME_MODULE => time_module,
    STEEL_MUTABLE_VECTOR_MODULE => mutable_vector_module,
    STEEL_PRIVATE_READER_MODULE => reader_module,
    STEEL_TCP_MODULE => tcp_module,
    STEEL_POLLING_MODULE => polling_module,
    STEEL_HTTP_MODULE => http_module,
    STEEL_PRELUDE_MODULE => prelude,
    STEEL_SB_PRELUDE => sandboxed_prelude,

    STEEL_GIT_MODULE => git_module,
    STEEL_HASH_MODULE => hashes_module,
}

#[cfg(all(feature = "dylibs", feature = "sync"))]
pub static STEEL_FFI_MODULE: once_cell::sync::Lazy<BuiltInModule> =
    once_cell::sync::Lazy::new(ffi_module);

#[cfg(not(feature = "sync"))]
thread_local! {
    pub static MAP_MODULE: BuiltInModule = hashmap_module();
    pub static SET_MODULE: BuiltInModule = hashset_module();
    pub static LIST_MODULE: BuiltInModule = list_module();
    pub static STRING_MODULE: BuiltInModule = string_module();
    pub static VECTOR_MODULE: BuiltInModule = vector_module();

    pub static IMMUTABLE_VECTOR_MODULE: BuiltInModule = immutable_vectors_module();

    pub static BYTEVECTOR_MODULE: BuiltInModule = bytevector_module();

    pub static STREAM_MODULE: BuiltInModule = stream_module();
    pub static IDENTITY_MODULE: BuiltInModule = identity_module();
    pub static NUMBER_MODULE: BuiltInModule = number_module();
    pub static EQUALITY_MODULE: BuiltInModule = equality_module();
    pub static ORD_MODULE: BuiltInModule = ord_module();
    pub static TRANSDUCER_MODULE: BuiltInModule = transducer_module();
    pub static SYMBOL_MODULE: BuiltInModule = symbol_module();
    pub static IO_MODULE: BuiltInModule = io_module();
    pub static FS_MODULE: BuiltInModule = fs_module();
    pub static FS_MODULE_SB: BuiltInModule = fs_module_sandbox();
    pub static PORT_MODULE: BuiltInModule = port_module();
    pub static PORT_MODULE_WITHOUT_FILESYSTEM: BuiltInModule = port_module_without_filesystem();
    pub static META_MODULE: BuiltInModule = meta_module();
    pub static JSON_MODULE: BuiltInModule = json_module();
    pub static CONSTANTS_MODULE: BuiltInModule = constants_module();
    pub static SYNTAX_MODULE: BuiltInModule = syntax_module();
    pub static SANDBOXED_META_MODULE: BuiltInModule = sandboxed_meta_module();
    pub static PROCESS_MODULE: BuiltInModule = process_module();
    pub static RANDOM_MODULE: BuiltInModule = random_module();
    pub static RESULT_MODULE: BuiltInModule = build_result_structs();
    pub static TYPE_ID_MODULE: BuiltInModule = build_type_id_module();
    pub static OPTION_MODULE: BuiltInModule = build_option_structs();
    pub static TCP_MODULE: BuiltInModule = tcp_module();
    pub static HTTP_MODULE: BuiltInModule = http_module();
    pub static POLLING_MODULE: BuiltInModule = polling_module();

    #[cfg(feature = "dylibs")]
    pub static FFI_MODULE: BuiltInModule = ffi_module();

    pub static PRELUDE_MODULE: BuiltInModule = prelude();
    pub static SB_PRELUDE: BuiltInModule = sandboxed_prelude();

    pub static TIME_MODULE: BuiltInModule = time_module();
    pub static THREADING_MODULE: BuiltInModule = threading_module();

    pub static MUTABLE_VECTOR_MODULE: BuiltInModule = mutable_vector_module();
    pub static PRIVATE_READER_MODULE: BuiltInModule = reader_module();

    pub static GIT_MODULE: BuiltInModule = git_module();
    pub static HASHES_MODULE: BuiltInModule = hashes_module();
}

#[cfg(not(feature = "sync"))]
thread_local! {
    pub(crate) static PRELUDE_INTERNED_STRINGS: FxHashSet<InternedString> = PRELUDE_MODULE.with(|x| x.names().into_iter().map(|x| x.into()).collect());
}

#[cfg(feature = "sync")]
thread_local! {
    pub(crate) static PRELUDE_INTERNED_STRINGS: FxHashSet<InternedString> = STEEL_PRELUDE_MODULE.names().into_iter().map(|x| x.into()).collect();
}

pub fn prelude() -> BuiltInModule {
    #[cfg(feature = "sync")]
    {
        BuiltInModule::new("steel/base")
            .with_module(STEEL_MAP_MODULE.clone())
            .with_module(STEEL_SET_MODULE.clone())
            .with_module(STEEL_LIST_MODULE.clone())
            .with_module(STEEL_STRING_MODULE.clone())
            .with_module(STEEL_VECTOR_MODULE.clone())
            .with_module(STEEL_STREAM_MODULE.clone())
            .with_module(STEEL_IDENTITY_MODULE.clone())
            .with_module(STEEL_NUMBER_MODULE.clone())
            .with_module(STEEL_EQUALITY_MODULE.clone())
            .with_module(STEEL_ORD_MODULE.clone())
            .with_module(STEEL_TRANSDUCER_MODULE.clone())
            .with_module(STEEL_SYMBOL_MODULE.clone())
            .with_module(STEEL_IO_MODULE.clone())
            .with_module(STEEL_FS_MODULE.clone())
            .with_module(STEEL_PORT_MODULE.clone())
            .with_module(STEEL_META_MODULE.clone())
            .with_module(STEEL_JSON_MODULE.clone())
            .with_module(STEEL_CONSTANTS_MODULE.clone())
            .with_module(STEEL_SYNTAX_MODULE.clone())
            .with_module(STEEL_PROCESS_MODULE.clone())
            .with_module(STEEL_RESULT_MODULE.clone())
            .with_module(STEEL_OPTION_MODULE.clone())
            .with_module(STEEL_TYPE_ID_MODULE.clone())
            .with_module(STEEL_TIME_MODULE.clone())
            .with_module(STEEL_THREADING_MODULE.clone())
            .with_module(STEEL_BYTEVECTOR_MODULE.clone())
    }

    #[cfg(not(feature = "sync"))]
    {
        BuiltInModule::new("steel/base")
            .with_module(MAP_MODULE.with(|x| x.clone()))
            .with_module(SET_MODULE.with(|x| x.clone()))
            .with_module(LIST_MODULE.with(|x| x.clone()))
            .with_module(STRING_MODULE.with(|x| x.clone()))
            .with_module(VECTOR_MODULE.with(|x| x.clone()))
            .with_module(STREAM_MODULE.with(|x| x.clone()))
            .with_module(IDENTITY_MODULE.with(|x| x.clone()))
            .with_module(NUMBER_MODULE.with(|x| x.clone()))
            .with_module(EQUALITY_MODULE.with(|x| x.clone()))
            .with_module(ORD_MODULE.with(|x| x.clone()))
            .with_module(TRANSDUCER_MODULE.with(|x| x.clone()))
            .with_module(SYMBOL_MODULE.with(|x| x.clone()))
            .with_module(IO_MODULE.with(|x| x.clone()))
            .with_module(FS_MODULE.with(|x| x.clone()))
            .with_module(PORT_MODULE.with(|x| x.clone()))
            .with_module(META_MODULE.with(|x| x.clone()))
            .with_module(JSON_MODULE.with(|x| x.clone()))
            .with_module(CONSTANTS_MODULE.with(|x| x.clone()))
            .with_module(SYNTAX_MODULE.with(|x| x.clone()))
            .with_module(PROCESS_MODULE.with(|x| x.clone()))
            .with_module(RESULT_MODULE.with(|x| x.clone()))
            .with_module(OPTION_MODULE.with(|x| x.clone()))
            .with_module(TYPE_ID_MODULE.with(|x| x.clone()))
            .with_module(TIME_MODULE.with(|x| x.clone()))
            .with_module(THREADING_MODULE.with(|x| x.clone()))
            .with_module(BYTEVECTOR_MODULE.with(|x| x.clone()))
    }
}

pub fn sandboxed_prelude() -> BuiltInModule {
    #[cfg(feature = "sync")]
    {
        BuiltInModule::new("steel/base")
            .with_module(STEEL_MAP_MODULE.clone())
            .with_module(STEEL_SET_MODULE.clone())
            .with_module(STEEL_LIST_MODULE.clone())
            .with_module(STEEL_STRING_MODULE.clone())
            .with_module(STEEL_VECTOR_MODULE.clone())
            .with_module(STEEL_STREAM_MODULE.clone())
            .with_module(STEEL_IDENTITY_MODULE.clone())
            .with_module(STEEL_NUMBER_MODULE.clone())
            .with_module(STEEL_EQUALITY_MODULE.clone())
            .with_module(STEEL_ORD_MODULE.clone())
            .with_module(STEEL_TRANSDUCER_MODULE.clone())
            .with_module(STEEL_SYMBOL_MODULE.clone())
            .with_module(STEEL_IO_MODULE.clone())
            .with_module(STEEL_FS_MODULE_SB.clone())
            .with_module(STEEL_PORT_WITHOUT_FS_MODULE.clone())
            .with_module(STEEL_META_MODULE.clone())
            .with_module(STEEL_JSON_MODULE.clone())
            .with_module(STEEL_CONSTANTS_MODULE.clone())
            .with_module(STEEL_SYNTAX_MODULE.clone())
            .with_module(STEEL_RESULT_MODULE.clone())
            .with_module(STEEL_OPTION_MODULE.clone())
            .with_module(STEEL_TYPE_ID_MODULE.clone())
            .with_module(STEEL_TIME_MODULE.clone())
            .with_module(STEEL_THREADING_MODULE.clone())
            .with_module(STEEL_BYTEVECTOR_MODULE.clone())
    }

    #[cfg(not(feature = "sync"))]
    {
        BuiltInModule::new("steel/base")
            .with_module(MAP_MODULE.with(|x| x.clone()))
            .with_module(SET_MODULE.with(|x| x.clone()))
            .with_module(LIST_MODULE.with(|x| x.clone()))
            .with_module(STRING_MODULE.with(|x| x.clone()))
            .with_module(VECTOR_MODULE.with(|x| x.clone()))
            .with_module(STREAM_MODULE.with(|x| x.clone()))
            .with_module(IDENTITY_MODULE.with(|x| x.clone()))
            .with_module(NUMBER_MODULE.with(|x| x.clone()))
            .with_module(EQUALITY_MODULE.with(|x| x.clone()))
            .with_module(ORD_MODULE.with(|x| x.clone()))
            .with_module(TRANSDUCER_MODULE.with(|x| x.clone()))
            .with_module(SYMBOL_MODULE.with(|x| x.clone()))
            .with_module(IO_MODULE.with(|x| x.clone()))
            .with_module(FS_MODULE_SB.with(|x| x.clone()))
            .with_module(PORT_MODULE_WITHOUT_FILESYSTEM.with(|x| x.clone()))
            .with_module(META_MODULE.with(|x| x.clone()))
            .with_module(JSON_MODULE.with(|x| x.clone()))
            .with_module(CONSTANTS_MODULE.with(|x| x.clone()))
            .with_module(SYNTAX_MODULE.with(|x| x.clone()))
            .with_module(RESULT_MODULE.with(|x| x.clone()))
            .with_module(OPTION_MODULE.with(|x| x.clone()))
            .with_module(TYPE_ID_MODULE.with(|x| x.clone()))
            .with_module(TIME_MODULE.with(|x| x.clone()))
            .with_module(THREADING_MODULE.with(|x| x.clone()))
            .with_module(BYTEVECTOR_MODULE.with(|x| x.clone()))
    }
}

fn render_as_md(text: String) {
    #[cfg(feature = "markdown")]
    println!("{}", termimad::text(&text));

    #[cfg(not(feature = "markdown"))]
    println!("{}", text);
}

pub fn register_builtin_modules(engine: &mut Engine, sandbox: bool) {
    engine.register_value("std::env::args", SteelVal::ListV(List::new()));

    engine.register_fn("##__module-get", BuiltInModule::get);
    engine.register_fn("%module-get%", BuiltInModule::get);
    engine.register_fn("%#maybe-module-get", BuiltInModule::try_get);

    engine.register_fn("load-from-module!", BuiltInModule::get);

    // Registering values in modules
    engine.register_fn("#%module", BuiltInModule::new::<String>);
    engine.register_fn(
        "#%module-add",
        |module: &mut BuiltInModule, name: SteelString, value: SteelVal| {
            module.register_value(&name, value);
        },
    );

    engine.register_fn(
        "#%module-add-doc",
        |module: &mut BuiltInModule, name: SteelString, value: String| {
            module.register_doc(
                Cow::Owned(name.as_str().to_string()),
                super::builtin::Documentation::Markdown(MarkdownDoc(value.into())),
            );
        },
    );

    engine.register_fn("%doc?", BuiltInModule::get_doc);
    engine.register_value("%list-modules!", SteelVal::BuiltIn(list_modules));
    engine.register_fn("%module/lookup-function", BuiltInModule::search);
    engine.register_fn("%string->render-markdown", render_as_md);
    engine.register_fn(
        "%module-bound-identifiers->list",
        BuiltInModule::bound_identifiers,
    );
    engine.register_value("%proto-hash%", HM_CONSTRUCT);
    engine.register_value("%proto-hash-insert%", HM_INSERT);
    engine.register_value("%proto-hash-get%", HM_GET);
    engine.register_value("error!", ControlOperations::error());

    engine.register_value("error", ControlOperations::error());

    engine.register_value("#%error", ControlOperations::error());

    engine.register_value(
        "%memo-table",
        WeakMemoizationTable::new().into_steelval().unwrap(),
    );
    engine.register_fn("%memo-table-ref", WeakMemoizationTable::get);
    engine.register_fn("%memo-table-set!", WeakMemoizationTable::insert);

    #[cfg(feature = "sync")]
    {
        engine
            .register_module(STEEL_MAP_MODULE.clone())
            .register_module(STEEL_SET_MODULE.clone())
            .register_module(STEEL_LIST_MODULE.clone())
            .register_module(STEEL_STRING_MODULE.clone())
            .register_module(STEEL_VECTOR_MODULE.clone())
            .register_module(STEEL_STREAM_MODULE.clone())
            .register_module(STEEL_IDENTITY_MODULE.clone())
            .register_module(STEEL_NUMBER_MODULE.clone())
            .register_module(STEEL_EQUALITY_MODULE.clone())
            .register_module(STEEL_ORD_MODULE.clone())
            .register_module(STEEL_TRANSDUCER_MODULE.clone())
            .register_module(STEEL_SYMBOL_MODULE.clone())
            .register_module(STEEL_IO_MODULE.clone())
            .register_module(STEEL_PORT_MODULE.clone())
            .register_module(STEEL_FS_MODULE.clone())
            .register_module(STEEL_META_MODULE.clone())
            .register_module(STEEL_JSON_MODULE.clone())
            .register_module(STEEL_CONSTANTS_MODULE.clone())
            .register_module(STEEL_SYNTAX_MODULE.clone())
            .register_module(STEEL_PROCESS_MODULE.clone())
            .register_module(STEEL_RESULT_MODULE.clone())
            .register_module(STEEL_OPTION_MODULE.clone())
            .register_module(STEEL_TYPE_ID_MODULE.clone())
            .register_module(STEEL_PRELUDE_MODULE.clone())
            .register_module(STEEL_TIME_MODULE.clone())
            .register_module(STEEL_RANDOM_MODULE.clone())
            .register_module(STEEL_THREADING_MODULE.clone())
            .register_module(STEEL_BYTEVECTOR_MODULE.clone());

        engine.register_module(STEEL_GIT_MODULE.clone());

        engine.register_module(STEEL_HASH_MODULE.clone());

        if !sandbox {
            engine
                .register_module(STEEL_TCP_MODULE.clone())
                .register_module(STEEL_HTTP_MODULE.clone())
                .register_module(STEEL_POLLING_MODULE.clone());
        } else {
            engine
                .register_module(STEEL_FS_MODULE_SB.clone())
                .register_module(STEEL_PORT_WITHOUT_FS_MODULE.clone())
                .register_module(STEEL_SB_PRELUDE.clone());
        }

        #[cfg(feature = "dylibs")]
        engine.register_module(STEEL_FFI_MODULE.clone());

        // Private module
        engine.register_module(STEEL_MUTABLE_VECTOR_MODULE.clone());
        engine.register_module(STEEL_PRIVATE_READER_MODULE.clone());
        engine.register_module(STEEL_IMMUTABLE_VECTOR_MODULE.clone());
    }

    #[cfg(not(feature = "sync"))]
    {
        engine
            .register_module(MAP_MODULE.with(|x| x.clone()))
            .register_module(SET_MODULE.with(|x| x.clone()))
            .register_module(LIST_MODULE.with(|x| x.clone()))
            .register_module(STRING_MODULE.with(|x| x.clone()))
            .register_module(VECTOR_MODULE.with(|x| x.clone()))
            .register_module(STREAM_MODULE.with(|x| x.clone()))
            .register_module(IDENTITY_MODULE.with(|x| x.clone()))
            .register_module(NUMBER_MODULE.with(|x| x.clone()))
            .register_module(EQUALITY_MODULE.with(|x| x.clone()))
            .register_module(ORD_MODULE.with(|x| x.clone()))
            .register_module(TRANSDUCER_MODULE.with(|x| x.clone()))
            .register_module(SYMBOL_MODULE.with(|x| x.clone()))
            .register_module(IO_MODULE.with(|x| x.clone()))
            .register_module(FS_MODULE.with(|x| x.clone()))
            .register_module(PORT_MODULE.with(|x| x.clone()))
            .register_module(META_MODULE.with(|x| x.clone()))
            .register_module(JSON_MODULE.with(|x| x.clone()))
            .register_module(CONSTANTS_MODULE.with(|x| x.clone()))
            .register_module(SYNTAX_MODULE.with(|x| x.clone()))
            .register_module(PROCESS_MODULE.with(|x| x.clone()))
            .register_module(RESULT_MODULE.with(|x| x.clone()))
            .register_module(OPTION_MODULE.with(|x| x.clone()))
            .register_module(TYPE_ID_MODULE.with(|x| x.clone()))
            .register_module(PRELUDE_MODULE.with(|x| x.clone()))
            .register_module(TIME_MODULE.with(|x| x.clone()))
            .register_module(RANDOM_MODULE.with(|x| x.clone()))
            .register_module(THREADING_MODULE.with(|x| x.clone()))
            .register_module(BYTEVECTOR_MODULE.with(|x| x.clone()));

        engine.register_module(GIT_MODULE.with(|x| x.clone()));

        engine.register_module(HASHES_MODULE.with(|x| x.clone()));

        if !sandbox {
            engine
                .register_module(TCP_MODULE.with(|x| x.clone()))
                .register_module(HTTP_MODULE.with(|x| x.clone()))
                .register_module(POLLING_MODULE.with(|x| x.clone()));
        } else {
            engine
                .register_module(FS_MODULE_SB.with(|x| x.clone()))
                .register_module(PORT_MODULE_WITHOUT_FILESYSTEM.with(|x| x.clone()))
                .register_module(SB_PRELUDE.with(|x| x.clone()));
        }

        #[cfg(feature = "dylibs")]
        engine.register_module(FFI_MODULE.with(|x| x.clone()));

        // Private module
        engine.register_module(MUTABLE_VECTOR_MODULE.with(|x| x.clone()));
        engine.register_module(PRIVATE_READER_MODULE.with(|x| x.clone()));

        engine.register_module(IMMUTABLE_VECTOR_MODULE.with(|x| x.clone()));
    }
}

pub static MODULE_IDENTIFIERS: Lazy<fxhash::FxHashSet<InternedString>> = Lazy::new(|| {
    let mut set = fxhash::FxHashSet::default();

    // TODO: Consolidate the prefixes and module names into one spot
    set.insert("%-builtin-module-steel/hash".into());
    set.insert("%-builtin-module-steel/sets".into());
    set.insert("%-builtin-module-steel/lists".into());
    set.insert("%-builtin-module-steel/strings".into());
    set.insert("%-builtin-module-steel/vectors".into());
    set.insert("%-builtin-module-steel/immutable-vectors".into());
    set.insert("%-builtin-module-steel/streams".into());
    set.insert("%-builtin-module-steel/identity".into());
    set.insert("%-builtin-module-steel/numbers".into());
    set.insert("%-builtin-module-steel/equality".into());
    set.insert("%-builtin-module-steel/ord".into());
    set.insert("%-builtin-module-steel/transducers".into());
    set.insert("%-builtin-module-steel/io".into());
    set.insert("%-builtin-module-steel/filesystem".into());
    set.insert("%-builtin-module-steel/ports".into());
    set.insert("%-builtin-module-steel/meta".into());
    set.insert("%-builtin-module-steel/constants".into());
    set.insert("%-builtin-module-steel/syntax".into());
    set.insert("%-builtin-module-steel/process".into());
    set.insert("%-builtin-module-steel/core/result".into());
    set.insert("%-builtin-module-steel/core/option".into());
    set.insert("%-builtin-module-steel/threads".into());
    set.insert("%-builtin-module-steel/bytevectors".into());
    set.insert("%-builtin-module-steel/base".into());

    set
});

pub(crate) static PRELUDE_TO_RESERVED_MAP: Lazy<FxHashMap<String, InternedString>> =
    Lazy::new(|| {
        PRELUDE_INTERNED_STRINGS.with(|x| {
            x.iter()
                .map(|x| {
                    (
                        x.resolve().to_string(),
                        (CompactString::new("#%prim.") + x.resolve()).into(),
                    )
                })
                .collect()
        })
    });

pub fn builtin_to_reserved(ident: &str) -> InternedString {
    if let Some(value) = PRELUDE_TO_RESERVED_MAP.get(ident) {
        *value
    } else {
        (CompactString::new("#%prim.") + ident).into()
    }
}

// TODO: Do the same for the single threaded version as well

pub(crate) fn constant_primitives(
) -> crate::values::HashMap<InternedString, SteelVal, FxBuildHasher> {
    #[cfg(feature = "sync")]
    {
        CONSTANT_PRIMITIVES.clone()
    }

    #[cfg(not(feature = "sync"))]
    {
        CONSTANT_PRIMITIVES.with(|x| x.clone())
    }
}

#[cfg(feature = "sync")]
pub static CONSTANT_PRIMITIVES: Lazy<
    crate::values::HashMap<InternedString, SteelVal, FxBuildHasher>,
> = Lazy::new(|| STEEL_PRELUDE_MODULE.constant_funcs());

#[cfg(not(feature = "sync"))]
thread_local! {
    pub static CONSTANT_PRIMITIVES: crate::values::HashMap<InternedString, SteelVal, FxBuildHasher> = {
        PRELUDE_MODULE.with(|x| x.constant_funcs())
    };

}

// TODO: Make the prelude string generation lazy - so that
// the first time we load (steel/base) we don't have to regenerate
// the string. Probably just need a lazy static for loading 'steel/base'
// and then reference that directly.
pub static ALL_MODULES: &str = r#"
    (require-builtin steel/hash)
    (require-builtin steel/sets)
    (require-builtin steel/lists)
    (require-builtin steel/strings)
    (require-builtin steel/symbols)
    (require-builtin steel/vectors)
    (require-builtin steel/immutable-vectors)
    (require-builtin steel/streams)
    (require-builtin steel/identity)
    (require-builtin steel/numbers)
    (require-builtin steel/equality)
    (require-builtin steel/ord)
    (require-builtin steel/transducers)
    (require-builtin steel/io)
    (require-builtin steel/filesystem)
    (require-builtin steel/ports)
    (require-builtin steel/meta)
    (require-builtin steel/json)
    (require-builtin steel/constants)
    (require-builtin steel/syntax)
    (require-builtin steel/process)
    (require-builtin steel/core/result)
    (require-builtin steel/core/option)
    (require-builtin steel/core/types)
    (require-builtin steel/threads)
    (require-builtin steel/bytevectors)

    (require-builtin steel/hash as #%prim.)
    (require-builtin steel/sets as #%prim.)
    (require-builtin steel/lists as #%prim.)
    (require-builtin steel/strings as #%prim.)
    (require-builtin steel/symbols as #%prim.)
    (require-builtin steel/vectors as #%prim.)
    (require-builtin steel/immutable-vectors as #%prim.)
    (require-builtin steel/streams as #%prim.)
    (require-builtin steel/identity as #%prim.)
    (require-builtin steel/numbers as #%prim.)
    (require-builtin steel/equality as #%prim.)
    (require-builtin steel/ord as #%prim.)
    (require-builtin steel/transducers as #%prim.)
    (require-builtin steel/io as #%prim.)
    (require-builtin steel/filesystem as #%prim.)
    (require-builtin steel/ports as #%prim.)
    (require-builtin steel/meta as #%prim.)
    (require-builtin steel/json as #%prim.)
    (require-builtin steel/constants as #%prim.)
    (require-builtin steel/syntax as #%prim.)
    (require-builtin steel/process as #%prim.)
    (require-builtin steel/core/result as #%prim.)
    (require-builtin steel/core/option as #%prim.)
    (require-builtin steel/core/types as #%prim.)
    (require-builtin steel/threads as #%prim.)
    (require-builtin steel/bytevectors as #%prim.)
"#;

pub static ALL_MODULES_RESERVED: &str = r#"
    (require-builtin steel/hash as #%prim.)
    (require-builtin steel/sets as #%prim.)
    (require-builtin steel/lists as #%prim.)
    (require-builtin steel/strings as #%prim.)
    (require-builtin steel/symbols as #%prim.)
    (require-builtin steel/vectors as #%prim.)
    (require-builtin steel/immutable-vectors as #%prim.)
    (require-builtin steel/streams as #%prim.)
    (require-builtin steel/identity as #%prim.)
    (require-builtin steel/numbers as #%prim.)
    (require-builtin steel/equality as #%prim.)
    (require-builtin steel/ord as #%prim.)
    (require-builtin steel/transducers as #%prim.)
    (require-builtin steel/io as #%prim.)
    (require-builtin steel/filesystem as #%prim.)
    (require-builtin steel/ports as #%prim.)
    (require-builtin steel/meta as #%prim.)
    (require-builtin steel/json as #%prim.)
    (require-builtin steel/constants as #%prim.)
    (require-builtin steel/syntax as #%prim.)
    (require-builtin steel/process as #%prim.)
    (require-builtin steel/core/result as #%prim.)
    (require-builtin steel/core/option as #%prim.)
    (require-builtin steel/core/types as #%prim.)
    (require-builtin steel/threads as #%prim.)
    (require-builtin steel/bytevectors as #%prim.)
"#;

pub static SANDBOXED_MODULES: &str = r#"
    (require-builtin steel/hash)
    (require-builtin steel/sets)
    (require-builtin steel/lists)
    (require-builtin steel/strings)
    (require-builtin steel/symbols)
    (require-builtin steel/vectors)
    (require-builtin steel/immutable-vectors)
    (require-builtin steel/streams)
    (require-builtin steel/identity)
    (require-builtin steel/numbers)
    (require-builtin steel/equality)
    (require-builtin steel/ord)
    (require-builtin steel/transducers)
    (require-builtin steel/io)
    (require-builtin steel/meta)
    (require-builtin steel/json)
    (require-builtin steel/constants)
    (require-builtin steel/syntax)
"#;

// TODO: Clean this up a lot
fn vector_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/vectors");
    module
        .register_native_fn_definition(MUT_VEC_CONSTRUCT_DEFINITION)
        .register_native_fn_definition(MUT_VEC_SWAP_DEFINITION)
        .register_native_fn_definition(MUT_VEC_CONSTRUCT_VEC_DEFINITION)
        .register_native_fn_definition(MAKE_VECTOR_DEFINITION)
        .register_native_fn_definition(MUT_VEC_TO_LIST_DEFINITION)
        .register_native_fn_definition(VECTOR_FILL_DEFINITION)
        .register_native_fn_definition(MUT_VECTOR_COPY_DEFINITION)
        .register_native_fn_definition(MUT_VEC_PUSH_DEFINITION)
        .register_native_fn_definition(MUT_VEC_LENGTH_DEFINITION)
        .register_native_fn_definition(VEC_LENGTH_DEFINITION)
        .register_native_fn_definition(MUT_VEC_APPEND_DEFINITION)
        .register_native_fn_definition(MUT_VEC_GET_DEFINITION)
        .register_native_fn_definition(MUT_VEC_SET_DEFINITION)
        // Immutable vector operations
        .register_native_fn_definition(IMMUTABLE_VECTOR_CONSTRUCT_DEFINITION)
        .register_native_fn_definition(VEC_CONS_DEFINITION)
        .register_native_fn_definition(VEC_CAR_DEFINITION)
        .register_native_fn_definition(VEC_CDR_DEFINITION)
        .register_native_fn_definition(LIST_VEC_NULL_DEFINITION)
        .register_native_fn_definition(VEC_PUSH_DEFINITION)
        .register_native_fn_definition(VEC_RANGE_DEFINITION)
        .register_native_fn_definition(VEC_APPEND_DEFINITION)
        // TODO: This has to be cleaned up
        .register_native_fn_definition(VEC_REF_DEFINITION)
        .register_native_fn_definition(MUTABLE_VECTOR_CLEAR_DEFINITION)
        .register_native_fn_definition(MUTABLE_VECTOR_TO_STRING_DEFINITION)
        .register_native_fn_definition(MUTABLE_VECTOR_POP_DEFINITION);
    module
}

/// Returns true if the given value is exactly `#false`.
///
/// (not value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (not #false)
/// #true
///
/// > (not #true)
/// #false
///
/// > (not "hello")
/// #false
/// ```
#[steel_derive::function(name = "not", constant = true)]
fn not(value: &SteelVal) -> bool {
    matches!(value, SteelVal::BoolV(false))
}

/// Returns true if the value is a string.
///
/// (string? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (string? "hello")
/// #true
///
/// > (string? 'foo)
/// #false
/// ```
#[steel_derive::function(name = "string?", constant = true)]
fn stringp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::StringV(_))
}

/// Returns true if the value is a list.
///
/// (list? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (list? '(1 2 3))
/// #true
///
/// > (list? "not-a-list")
/// #false
/// ```
#[steel_derive::function(name = "list?", constant = true)]
fn listp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::ListV(_))
}

/// Returns true if the value is a vector (mutable or immutable).
///
/// (vector? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (vector? #(1 2 3))
/// #true
///
/// > (vector? 'foo)
/// #false
/// ```
#[steel_derive::function(name = "vector?", constant = true)]
fn vectorp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::VectorV(_) | SteelVal::MutableVector(_))
}

/// Returns true if the value is a symbol.
///
/// (symbol? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (symbol? 'hello)
/// #true
///
/// > (symbol? "hello")
/// #false
/// ```
#[steel_derive::function(name = "symbol?", constant = true)]
fn symbolp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::SymbolV(_))
}

/// Returns true if the value is a hash map.
///
/// (hash? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (hash? (hash 'a 10 'b 20))
/// #true
///
/// > (hash? '(a b c))
/// #false
/// ```
#[steel_derive::function(name = "hash?", constant = true)]
fn hashp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::HashMapV(_))
}

/// Returns true if the value is a hash set.
///
/// (set? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (set? (hashset 10 20 30 40))
/// #true
///
/// > (set? "abc")
/// #false
/// ```
#[steel_derive::function(name = "set?", constant = true)]
fn hashsetp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::HashSetV(_))
}

#[steel_derive::function(name = "continuation?", constant = true)]
fn continuationp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::ContinuationFunction(_))
}

/// Returns true if the value is a boolean (`#true` or `#false`).
///
/// (boolean? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (boolean? #true)
/// #true
///
/// > (boolean? #false)
/// #true
///
/// > (boolean? 0)
/// #false
/// ```
#[steel_derive::function(name = "boolean?", constant = true)]
fn booleanp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::BoolV(_))
}

/// Alias for `boolean?`. Returns true if the value is a boolean.
///
/// (bool? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (bool? #false)
/// #true
///
/// > (bool? "hi")
/// #false
/// ```
#[steel_derive::function(name = "bool?", constant = true)]
fn boolp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::BoolV(_))
}

/// Returns true if the value is `void`.
///
/// (void? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (void? void)
/// #true
///
/// > (void? 42)
/// #false
/// ```
#[steel_derive::function(name = "void?", constant = true)]
fn voidp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::Void)
}

#[steel_derive::function(name = "struct?", constant = true)]
fn structp(value: &SteelVal) -> bool {
    if let SteelVal::CustomStruct(s) = value {
        s.is_transparent()
    } else {
        false
    }
}

#[steel_derive::function(name = "port?", constant = true)]
fn portp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::PortV(..))
}

#[steel_derive::function(name = "#%private-struct?", constant = true)]
fn private_structp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::CustomStruct(_))
}

#[steel_derive::function(name = "error-object?", constant = true)]
fn error_objectp(value: &SteelVal) -> bool {
    let SteelVal::Custom(val) = value else {
        return false;
    };

    // let cell: &RefCell<_> = &*val;
    as_underlying_type::<SteelErr>(val.read().as_ref()).is_some()
}

/// Returns true if the value is a function or callable.
///
/// (function? value) -> boolean?
///
/// * `value` : any — the value to test
///
/// # Examples
/// ```scheme
/// > (function? (lambda (x) x))
/// #true
///
/// > (function? map)
/// #true
///
/// > (function? 42)
/// #false
/// ```
#[steel_derive::function(name = "function?", constant = true)]
fn functionp(value: &SteelVal) -> bool {
    matches!(
        value,
        SteelVal::Closure(_)
            | SteelVal::FuncV(_)
            // | SteelVal::ContractedFunction(_)
            | SteelVal::BoxedFunction(_)
            | SteelVal::ContinuationFunction(_)
            | SteelVal::MutFunc(_)
            | SteelVal::BuiltIn(_)
    )
}

#[steel_derive::function(name = "procedure?", constant = true)]
fn procedurep(value: &SteelVal) -> bool {
    if let SteelVal::CustomStruct(s) = value {
        return s.maybe_proc().map(|x| procedurep(x)).unwrap_or(false);
    }

    matches!(
        value,
        SteelVal::Closure(_)
            | SteelVal::FuncV(_)
            | SteelVal::BoxedFunction(_)
            | SteelVal::ContinuationFunction(_)
            | SteelVal::MutFunc(_)
            | SteelVal::BuiltIn(_)
    )
}
fn identity_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/identity");
    module
        .register_native_fn_definition(NOT_DEFINITION)
        .register_native_fn_definition(numbers::COMPLEXP_DEFINITION)
        .register_native_fn_definition(numbers::EXACT_INTEGERP_DEFINITION)
        .register_native_fn_definition(numbers::FLOATP_DEFINITION)
        .register_native_fn_definition(numbers::INTEGERP_DEFINITION)
        .register_native_fn_definition(numbers::INTP_DEFINITION)
        .register_native_fn_definition(numbers::NUMBERP_DEFINITION)
        .register_native_fn_definition(numbers::RATIONALP_DEFINITION)
        .register_native_fn_definition(numbers::REALP_DEFINITION)
        .register_native_fn_definition(STRINGP_DEFINITION)
        .register_native_fn_definition(LISTP_DEFINITION)
        .register_native_fn_definition(VECTORP_DEFINITION)
        .register_native_fn_definition(SYMBOLP_DEFINITION)
        .register_native_fn_definition(HASHP_DEFINITION)
        .register_native_fn_definition(HASHSETP_DEFINITION)
        .register_native_fn_definition(CONTINUATIONP_DEFINITION)
        .register_native_fn_definition(BOOLEANP_DEFINITION)
        .register_native_fn_definition(BOOLP_DEFINITION)
        .register_native_fn_definition(VOIDP_DEFINITION)
        .register_native_fn_definition(STRUCTP_DEFINITION)
        .register_native_fn_definition(PORTP_DEFINITION)
        .register_native_fn_definition(EOF_OBJECTP_DEFINITION)
        .register_native_fn_definition(PRIVATE_STRUCTP_DEFINITION)
        .register_native_fn_definition(ERROR_OBJECTP_DEFINITION)
        .register_value("mutable-vector?", gen_pred!(MutableVector))
        .register_value("immutable-vector?", gen_pred!(VectorV))
        .register_value("char?", gen_pred!(CharV))
        .register_value("future?", gen_pred!(FutureV))
        .register_native_fn_definition(FUNCTIONP_DEFINITION)
        .register_native_fn_definition(PROCEDUREP_DEFINITION)
        .register_value(
            "atom?",
            gen_pred!(NumV, IntV, StringV, SymbolV, BoolV, CharV),
        );
    module
}

fn stream_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/streams");
    module
        .register_value("stream-cons", StreamOperations::stream_cons())
        .register_value("empty-stream", StreamOperations::empty_stream())
        .register_value("stream-empty?", StreamOperations::stream_empty_huh())
        .register_value("stream-car", StreamOperations::stream_car())
        .register_value("#%stream-cdr", StreamOperations::stream_cdr());
    module
}

// fn contract_module() -> BuiltInModule {
//     let mut module = BuiltInModule::new("steel/contracts");
//     module
//         .register_value("bind/c", contracts::BIND_CONTRACT_TO_FUNCTION)
//         .register_value("make-flat/c", contracts::MAKE_FLAT_CONTRACT)
//         .register_value(
//             "make-dependent-function/c",
//             contracts::MAKE_DEPENDENT_CONTRACT,
//         )
//         .register_value("make-function/c", contracts::MAKE_FUNCTION_CONTRACT)
//         .register_value("make/c", contracts::MAKE_C);

//     module
// }

fn number_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/numbers");
    module
        .register_native_fn_definition(numbers::ADD_PRIMITIVE_DEFINITION)
        .register_native_fn_definition(numbers::FLOAT_ADD_DEFINITION)
        .register_native_fn_definition(numbers::MULTIPLY_PRIMITIVE_DEFINITION)
        .register_native_fn_definition(numbers::DIVIDE_PRIMITIVE_DEFINITION)
        .register_native_fn_definition(numbers::SUBTRACT_PRIMITIVE_DEFINITION)
        .register_native_fn_definition(numbers::TRUNCATE_DEFINITION)
        .register_native_fn_definition(numbers::EVEN_DEFINITION)
        .register_native_fn_definition(numbers::ODD_DEFINITION)
        .register_native_fn_definition(numbers::ARITHMETIC_SHIFT_DEFINITION)
        .register_native_fn_definition(numbers::ABS_DEFINITION)
        .register_native_fn_definition(numbers::NANP_DEFINITION)
        .register_native_fn_definition(numbers::ZEROP_DEFINITION)
        .register_native_fn_definition(numbers::POSITIVEP_DEFINITION)
        .register_native_fn_definition(numbers::NEGATIVEP_DEFINITION)
        .register_native_fn_definition(numbers::CEILING_DEFINITION)
        .register_native_fn_definition(numbers::DENOMINATOR_DEFINITION)
        .register_native_fn_definition(numbers::EXACTP_DEFINITION)
        .register_native_fn_definition(numbers::EXACT_INTEGER_SQRT_DEFINITION)
        .register_native_fn_definition(numbers::EXPT_DEFINITION)
        .register_native_fn_definition(numbers::EXP_DEFINITION)
        .register_native_fn_definition(numbers::FINITEP_DEFINITION)
        .register_native_fn_definition(numbers::FLOOR_DEFINITION)
        .register_native_fn_definition(numbers::INEXACTP_DEFINITION)
        .register_native_fn_definition(numbers::INEXACT_DEFINITION)
        .register_native_fn_definition(numbers::EXACT_DEFINITION)
        .register_native_fn_definition(numbers::INFINITEP_DEFINITION)
        .register_native_fn_definition(numbers::LOG_DEFINITION)
        .register_native_fn_definition(numbers::MAGNITUDE_DEFINITION)
        .register_native_fn_definition(numbers::ANGLE_DEFINITION)
        .register_native_fn_definition(numbers::MAKE_RECTANGULAR_DEFINITION)
        .register_native_fn_definition(numbers::MAKE_POLAR_DEFINITION)
        .register_native_fn_definition(numbers::REAL_PART_DEFINITION)
        .register_native_fn_definition(numbers::IMAG_PART_DEFINITION)
        .register_native_fn_definition(numbers::NUMERATOR_DEFINITION)
        .register_native_fn_definition(numbers::QUOTIENT_DEFINITION)
        .register_native_fn_definition(numbers::MODULO_DEFINITION)
        .register_native_fn_definition(numbers::REMAINDER_DEFINITION)
        .register_native_fn_definition(numbers::ROUND_DEFINITION)
        .register_native_fn_definition(numbers::SQUARE_DEFINITION)
        .register_native_fn_definition(numbers::SQRT_DEFINITION)
        .register_native_fn_definition(numbers::SIN_DEFINITION)
        .register_native_fn_definition(numbers::COS_DEFINITION)
        .register_native_fn_definition(numbers::TAN_DEFINITION)
        .register_native_fn_definition(numbers::ASIN_DEFINITION)
        .register_native_fn_definition(numbers::ACOS_DEFINITION)
        .register_native_fn_definition(numbers::ATAN_DEFINITION);

    module
}

#[inline(always)]
pub fn equality_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(args.windows(2).all(|x| x[0] == x[1])))
}

pub fn gte_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        stop!(ArityMismatch => "expected at least one argument");
    }

    Ok(SteelVal::BoolV(args.windows(2).all(|x| {
        x[0].partial_cmp(&x[1])
            .map(|x| x != Ordering::Less)
            .unwrap_or(false)
    })))
}

#[inline(always)]
pub fn lte_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        stop!(ArityMismatch => "expected at least one argument");
    }

    Ok(SteelVal::BoolV(args.windows(2).all(|x| {
        x[0].partial_cmp(&x[1])
            .map(|x| x != Ordering::Greater)
            .unwrap_or(false)
    })))
}

fn equality_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/equality");
    module
        .register_value(
            "equal?",
            SteelVal::FuncV(ensure_tonicity_two!(|a, b| a == b)),
        )
        .register_value(
            "eqv?",
            SteelVal::FuncV(ensure_tonicity_two!(
                |a: &SteelVal, b: &SteelVal| a.ptr_eq(b)
            )),
        )
        .register_value(
            "eq?",
            SteelVal::FuncV(ensure_tonicity_two!(
                |a: &SteelVal, b: &SteelVal| a.ptr_eq(b)
            )),
        )
        .register_native_fn_definition(NUMBER_EQUALITY_DEFINITION);

    // TODO: Replace this with just numeric equality!
    // .register_value("=", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a == b)));

    module
}

/// Real numbers ordering module.
#[steel_derive::define_module(name = "steel/ord")]
fn ord_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ord");

    fn ensure_real(x: &SteelVal) -> Result<&SteelVal> {
        realp(x).then_some(x).ok_or_else(|| {
            SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("expected real numbers, found: {}", x),
            )
        })
    }

    fn ord_internal(
        args: &[SteelVal],
        ordering_f: impl Fn(Option<Ordering>) -> bool,
    ) -> Result<SteelVal> {
        match args {
            [x] => {
                ensure_real(x)?;
                Ok(SteelVal::BoolV(true))
            }
            [x, rest @ ..] => {
                let mut left = ensure_real(x)?;
                for r in rest {
                    let right = ensure_real(r)?;
                    if !ordering_f(left.partial_cmp(right)) {
                        return Ok(SteelVal::BoolV(false));
                    }
                    left = right;
                }
                Ok(SteelVal::BoolV(true))
            }
            _ => stop!(ArityMismatch => "expected at least one argument"),
        }
    }

    /// Compares real numbers to check if any number is greater than the subsequent.
    ///
    /// (> x . rest) -> bool?
    ///
    /// * x : real? - The first real number to compare.
    /// * rest : real? - The rest of the numbers to compare.
    ///
    /// # Examples
    /// ```scheme
    /// > (> 1) ;; => #t
    /// > (> 3 2) ;; => #t
    /// > (> 1 1) ;; => #f
    /// > (> 3/2 1.5) ;; => #f
    /// > (> 3/2 1.4) ;; => #t
    /// > (> 3 4/2 1) ;; #t
    /// ```
    #[steel_derive::native(name = ">", arity = "AtLeast(1)")]
    fn greater_than(args: &[SteelVal]) -> Result<SteelVal> {
        ord_internal(args, |o| matches!(o, Some(Ordering::Greater)))
    }

    /// Compares real numbers to check if any number is greater than or equal than the subsequent.
    ///
    /// (>= x . rest) -> bool?
    ///
    /// * x : real? - The first real number to compare.
    /// * rest : real? - The rest of the numbers to compare.
    ///
    /// # Examples
    /// ```scheme
    /// > (>= 1) ;; => #t
    /// > (>= 3 2) ;; => #t
    /// > (>= 2 3) ;; => #f
    /// > (>= 3/2 1.5) ;; => #t
    /// > (>= 3/2 1.4) ;; => #t
    /// > (>= 2 4/2 1) ;; #t
    /// ```
    #[steel_derive::native(name = ">=", arity = "AtLeast(1)")]
    fn greater_than_equal(args: &[SteelVal]) -> Result<SteelVal> {
        ord_internal(args, |o| {
            matches!(o, Some(Ordering::Greater | Ordering::Equal))
        })
    }

    /// Compares real numbers to check if any number is less than the subsequent.
    ///
    /// (< x . rest) -> bool?
    ///
    /// * x : real? - The first real number to compare.
    /// * rest : real? - The rest of the numbers to compare.
    ///
    /// # Examples
    /// ```scheme
    /// > (< 1) ;; => #t
    /// > (< 3 2) ;; => #f
    /// > (< 2 3) ;; => #t
    /// > (< 3/2 1.5) ;; => #f
    /// > (< 2.5 3/2) ;; => #t
    /// > (< 2 5/2 3) ;; #t
    /// ```
    #[steel_derive::native(name = "<", arity = "AtLeast(1)")]
    fn less_than(args: &[SteelVal]) -> Result<SteelVal> {
        ord_internal(args, |o| matches!(o, Some(Ordering::Less)))
    }

    /// Compares real numbers to check if any number is less than or equal than the subsequent.
    ///
    /// (<= x . rest) -> bool?
    ///
    /// * x : real? - The first real number to compare.
    /// * rest : real? - The rest of the numbers to compare.
    ///
    /// # Examples
    /// ```scheme
    /// > (<= 1) ;; => #t
    /// > (<= 3 2) ;; => #f
    /// > (<= 2 3) ;; => #t
    /// > (<= 3/2 1.5) ;; => #t
    /// > (<= 2.5 3/2) ;; => #f
    /// > (<= 2 6/2 3) ;; #t
    /// ```
    #[steel_derive::native(name = "<=", arity = "AtLeast(1)")]
    fn less_than_equal(args: &[SteelVal]) -> Result<SteelVal> {
        ord_internal(args, |o| {
            matches!(o, Some(Ordering::Less | Ordering::Equal))
        })
    }

    module
        .register_native_fn_definition(GREATER_THAN_DEFINITION)
        .register_native_fn_definition(GREATER_THAN_EQUAL_DEFINITION)
        .register_native_fn_definition(LESS_THAN_DEFINITION)
        .register_native_fn_definition(LESS_THAN_EQUAL_DEFINITION);
    module
}

fn io_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/io");
    module
        .register_value("stdout-simple-displayln", IoFunctions::displayln())
        .register_value("read-to-string", IoFunctions::read_to_string());

    module
}

pub const VOID_DOC: MarkdownDoc = MarkdownDoc::from_str(
    "The void value, returned by many forms with side effects, such as `define`.",
);

/// Miscellaneous constants
#[steel_derive::define_module(name = "steel/constants")]
fn constants_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/constants");
    module.register_value("void", SteelVal::Void);
    module.register_doc("void", VOID_DOC);
    module
}

fn get_environment_variable(var: String) -> Result<SteelVal> {
    std::env::var(var)
        .map(|x| x.into_steelval().unwrap())
        .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
}

fn maybe_get_environment_variable(var: String) -> SteelResult<SteelVal, SteelErr> {
    get_environment_variable(var).into()
}

fn sandboxed_meta_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/meta");
    module
        // .register_value("assert!", MetaOperations::assert_truthy())
        .register_value("active-object-count", MetaOperations::active_objects())
        // .register_value("memory-address", MetaOperations::memory_address())
        // .register_value("async-exec", MetaOperations::exec_async())
        // .register_value("poll!", MetaOperations::poll_value())
        // .register_value("block-on", MetaOperations::block_on())
        // .register_value("join!", MetaOperations::join_futures())
        // .register_value("struct-ref", struct_ref())
        // .register_value("struct->list", struct_to_list())
        // .register_value("struct->vector", struct_to_vector())
        .register_fn("value->string", super::meta::value_to_string)
        // .register_value("expand!", SteelVal::FuncV(super::meta::expand_macros))
        // .register_value("read!", SteelVal::FuncV(super::meta::read))
        // .register_value("eval!", SteelVal::FuncV(super::meta::eval))
        // TODO: @Matt -> implement the traits for modules as well
        // .register_fn("Engine::new", super::meta::EngineWrapper::new)
        .register_fn("eval!", super::meta::eval)
        .register_fn("value->iterator", crate::rvals::value_into_iterator)
        .register_value("iter-next!", SteelVal::FuncV(crate::rvals::iterator_next));
    // .register_fn("run!", super::meta::EngineWrapper::call)
    // .register_fn("get-value", super::meta::EngineWrapper::get_value)
    // .register_fn("env-var", get_environment_variable);
    module
}

/// Returns the message of an error object.
///
/// (error-object-message error?) -> string?
#[steel_derive::function(name = "error-object-message")]
fn error_object_message(val: &SteelVal) -> Result<SteelVal> {
    let SteelVal::Custom(custom) = val else {
        stop!(TypeMismatch => "error-object-message: expected an error object");
    };

    let value = custom.read();

    let Some(error) = as_underlying_type::<SteelErr>(value.as_ref()) else {
        stop!(TypeMismatch => "error-object-message: expected an error object");
    };

    Ok(error.message().to_string().into())
}

pub fn lookup_function_name(value: SteelVal) -> Option<SteelVal> {
    match value {
        SteelVal::BoxedFunction(f) => f.name().map(|x| x.into_steelval().unwrap()),
        SteelVal::FuncV(f) => get_function_name(f).map(|x| x.name.into_steelval().unwrap()),
        SteelVal::BuiltIn(f) => {
            get_function_metadata(super::builtin::BuiltInFunctionType::Context(f as _))
                .map(|x| x.name().into_steelval().unwrap())
        }
        SteelVal::MutFunc(f) => {
            get_function_metadata(super::builtin::BuiltInFunctionType::Mutable(f as _))
                .map(|x| x.name().into_steelval().unwrap())
        }
        _ => None,
    }
}

#[steel_derive::context(name = "#%lookup-doc", arity = "Exact(1)")]
pub fn lookup_doc_ctx(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    // Attempt to find the docs for a given value.
    let compiler = ctx.thread.compiler.read();
    let value = args[0].clone();

    let doc = compiler.get_doc(value);

    match doc {
        Some(crate::compiler::compiler::StringOrSteelString::String(s)) => {
            Some(Ok(SteelVal::StringV(s.into())))
        }
        Some(crate::compiler::compiler::StringOrSteelString::SteelString(s)) => {
            Some(Ok(SteelVal::StringV(s)))
        }
        None => Some(Ok(SteelVal::BoolV(false))),
    }
}

fn lookup_doc(value: SteelVal) -> bool {
    match value {
        // SteelVal::BoxedFunction(f) => ,
        SteelVal::FuncV(f) => {
            let metadata = get_function_metadata(BuiltInFunctionType::Reference(f));

            if let Some(data) = metadata.and_then(|x| x.doc) {
                println!("{}", data);
                true
            } else {
                false
            }
        }
        SteelVal::MutFunc(f) => {
            let metadata = get_function_metadata(BuiltInFunctionType::Mutable(f));
            if let Some(data) = metadata.and_then(|x| x.doc) {
                println!("{}", data);
                true
            } else {
                false
            }
        }
        SteelVal::BuiltIn(f) => {
            let metadata = get_function_metadata(BuiltInFunctionType::Context(f));
            if let Some(data) = metadata.and_then(|x| x.doc) {
                println!("{}", data);
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

fn lookup_doc_value(value: SteelVal) -> Option<String> {
    match value {
        SteelVal::FuncV(f) => {
            let metadata = get_function_metadata(BuiltInFunctionType::Reference(f));
            metadata.and_then(|x| x.doc.map(|x| x.0.to_string()))
        }
        SteelVal::MutFunc(f) => {
            let metadata = get_function_metadata(BuiltInFunctionType::Mutable(f));
            metadata.and_then(|x| x.doc.map(|x| x.0.to_string()))
        }
        SteelVal::BuiltIn(f) => {
            let metadata = get_function_metadata(BuiltInFunctionType::Context(f));
            metadata.and_then(|x| x.doc.map(|x| x.0.to_string()))
        }
        _ => None,
    }
}

fn arity_to_list(arity: &Arity) -> SteelVal {
    match arity {
        Arity::Exact(e) => vec!["exact".into_steelval().unwrap(), e.into_steelval().unwrap()],
        Arity::AtLeast(e) => vec![
            "at-least".into_steelval().unwrap(),
            e.into_steelval().unwrap(),
        ],
        Arity::AtMost(e) => vec![
            "at-most".into_steelval().unwrap(),
            e.into_steelval().unwrap(),
        ],
        Arity::Range(l, h) => vec![
            "range".into_steelval().unwrap(),
            l.into_steelval().unwrap(),
            h.into_steelval().unwrap(),
        ],
    }
    .into_steelval()
    .unwrap()
}

// Only works with fixed size arity functions
fn arity(value: SteelVal) -> UnRecoverableResult {
    match value {
        SteelVal::Closure(c) => {
            // Ok(SteelVal::IntV(c.arity() as isize)).into()

            if let Some(SteelVal::CustomStruct(s)) = c.get_contract_information() {
                let guard = s;
                if guard.name().resolve() == "FunctionContract" {
                    if let SteelVal::ListV(l) = &guard.fields[0] {
                        Ok(SteelVal::IntV(l.len() as isize)).into()
                    } else {
                        steelerr!(TypeMismatch => "Unable to find the arity for the given function")
                            .into()
                    }
                } else if guard.name().resolve() == "FlatContract" {
                    Ok(SteelVal::IntV(1)).into()
                } else {
                    // This really shouldn't happen
                    Ok(SteelVal::IntV(c.arity() as isize)).into()
                }
            } else {
                Ok(SteelVal::IntV(c.arity() as isize)).into()
            }
        }
        SteelVal::BoxedFunction(f) => f
            .get_arity()
            .map(|x| SteelVal::IntV(x as isize))
            .ok_or(SteelErr::new(
                ErrorKind::TypeMismatch,
                "Unable to find the arity for the given function".to_string(),
            ))
            // .ok_or(steelerr!(TypeMismatch => "Unable to find the arity for the give function"))
            .into(),

        // Lookup the function signature metadata, return the arity payload
        SteelVal::FuncV(f) => {
            let metadata = get_function_name(f);

            metadata
                .map(|x| x.arity)
                .ok_or(SteelErr::new(
                    ErrorKind::TypeMismatch,
                    "Unable to find the arity for the given function".to_string(),
                ))
                .and_then(|x| x.into_steelval())
                .into()
        }

        // Ok(SteelVal::IntV(f.get_arity()))
        _ => steelerr!(TypeMismatch => "Unable to find the arity for the given function").into(),
    }
}

// Only works with fixed size arity functions
fn is_multi_arity(value: SteelVal) -> UnRecoverableResult {
    match value {
        SteelVal::Closure(c) => Ok(SteelVal::BoolV(c.is_multi_arity)).into(),
        _ => steelerr!(TypeMismatch => "Unable to find the arity for the given function").into(),
    }
}

struct MutableVector {
    vector: Vec<SteelVal>,
}

impl MutableVector {
    fn new() -> Self {
        Self { vector: Vec::new() }
    }

    fn vector_push(&mut self, value: SteelVal) {
        self.vector.push(value);
    }

    fn vector_pop(&mut self) -> Option<SteelVal> {
        self.vector.pop()
    }

    fn vector_set(&mut self, index: usize, value: SteelVal) {
        self.vector[index] = value;
    }

    fn vector_ref(&self, index: usize) -> SteelVal {
        self.vector[index].clone()
    }

    fn vector_len(&self) -> usize {
        self.vector.len()
    }

    fn vector_to_list(&self) -> SteelVal {
        SteelVal::ListV(self.vector.clone().into())
    }

    fn vector_is_empty(&self) -> bool {
        self.vector.is_empty()
    }

    fn vector_from_list(lst: List<SteelVal>) -> Self {
        Self {
            vector: lst.into_iter().collect(),
        }
    }
}

impl crate::rvals::Custom for MutableVector {
    fn gc_visit_children(&self, context: &mut crate::values::closed::MarkAndSweepContext) {
        for value in &self.vector {
            context.push_back(value.clone());
        }
    }

    fn visit_equality(&self, visitor: &mut crate::rvals::cycles::EqualityVisitor) {
        for value in &self.vector {
            visitor.push_back(value.clone());
        }
    }

    // Compare the two for equality otherwise
    fn equality_hint(&self, other: &dyn crate::rvals::CustomType) -> bool {
        if let Some(other) = as_underlying_type::<MutableVector>(other) {
            self.vector.len() == other.vector.len()
        } else {
            false
        }
    }
}

struct Reader {
    buffer: String,
    offset: usize,
}

impl crate::rvals::Custom for Reader {}

impl Reader {
    fn create_reader() -> Reader {
        Self {
            buffer: String::new(),
            offset: 0,
        }
    }

    fn push_string(&mut self, input: crate::rvals::SteelString) {
        self.buffer.push_str(input.as_str());
    }

    fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    fn read_one_impl(&mut self, finisher: fn(ExprKind) -> Result<SteelVal>) -> Result<SteelVal> {
        if let Some(buffer) = self.buffer.get(self.offset..) {
            let mut parser = crate::parser::parser::Parser::new_flat(buffer, SourceId::none());

            // TODO: This reparses everything while it hasn't yet finished the expression.
            // What we need to do is keep the parser around while we finish it.
            // For that, we should leave the parser around, and just leak buffer?
            // or swap in something else? I think the stack needs to be saved
            // in between runs. If we hit a situation where we EOF, we want to
            // resume where we were, and keep the stack in the proper spot. Otherwise,
            // we reparse everything up to that point since we're reading line by line?
            if let Some(raw) = parser.next() {
                let next = match raw {
                    Ok(next) => next,
                    // Err(steel_parser::parser::ParseError::UnexpectedEOF(_, _)) => {
                    //     return Ok(SteelVal::Void);
                    // }
                    _ => {
                        // dbg!(parser.offset());

                        return Ok(SteelVal::Void);
                    }
                };

                self.offset += parser.offset();

                let result = finisher(next);

                if let Some(remaining) = self.buffer.get(self.offset..) {
                    for _ in remaining.chars().take_while(|x| x.is_whitespace()) {
                        self.offset += 1;
                    }
                }

                if self.offset == self.buffer.len() {
                    self.buffer.clear();
                    self.offset = 0;
                }

                result
            } else {
                // No value, keep reading
                Ok(SteelVal::Void)
            }
        } else {
            // TODO: This needs to get fixed
            Ok(crate::primitives::ports::eof())
        }
    }

    fn read_one(&mut self) -> Result<SteelVal> {
        self.read_one_impl(TryFromExprKindForSteelVal::try_from_expr_kind_quoted)
    }

    fn read_one_syntax_object(&mut self) -> Result<SteelVal> {
        self.read_one_impl(
            crate::parser::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind,
        )
    }
}

#[steel_derive::context(name = "#%intern", arity = "Exact(1)")]
pub fn intern_symbol(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let mut guard = ctx.thread.compiler.write();
    let arg = args[0].clone();
    if let SteelVal::Void = arg {
        return Some(Ok(arg));
    }

    let interned_index = guard.constant_map.add_or_get(arg);
    let value = guard.constant_map.get(interned_index);
    Some(Ok(value))
}

fn reader_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("#%private/steel/reader");

    module
        .register_fn("new-reader", Reader::create_reader)
        .register_fn("reader-push-string", Reader::push_string)
        .register_fn("reader-read-one", Reader::read_one)
        .register_fn("reader-empty?", Reader::is_empty)
        .register_fn(
            "reader-read-one-syntax-object",
            Reader::read_one_syntax_object,
        )
        .register_native_fn_definition(INTERN_SYMBOL_DEFINITION);

    module
}

fn mutable_vector_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("#%private/steel/mvector");

    module
        .register_fn("make-mutable-vector", MutableVector::new)
        .register_fn("mutable-vector-ref", MutableVector::vector_ref)
        .register_fn("mutable-vector-set!", MutableVector::vector_set)
        .register_fn("mutable-vector-pop!", MutableVector::vector_pop)
        .register_fn("mutable-vector-push!", MutableVector::vector_push)
        .register_fn("mutable-vector-len", MutableVector::vector_len)
        .register_fn("mutable-vector->list", MutableVector::vector_to_list)
        .register_fn("mutable-vector-empty?", MutableVector::vector_is_empty)
        .register_fn("mutable-vector-from-list", MutableVector::vector_from_list);

    module
}

#[steel_derive::function(name = "#%unbox")]
pub fn unbox_mutable(value: &HeapRef<SteelVal>) -> SteelVal {
    value.get()
}

#[steel_derive::function(name = "#%set-box!")]
pub fn set_box_mutable(value: &HeapRef<SteelVal>, update: SteelVal) -> SteelVal {
    value.set_and_return(update)
}

#[steel_derive::function(name = "unbox")]
pub fn plain_unbox_mutable(value: &HeapRef<SteelVal>) -> SteelVal {
    value.get()
}

#[steel_derive::function(name = "set-box!")]
pub fn plain_set_box_mutable(value: &HeapRef<SteelVal>, update: SteelVal) -> SteelVal {
    value.set_and_return(update)
}

fn gc_collection(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if args.len() != 0 {
        return Some(Err(
            throw!(ArityMismatch => "gc-collect expects 0 arguments, found: {}", args.len())(),
        ));
    }

    ctx.gc_collect();

    Some(Ok(SteelVal::Void))
}

fn make_mutable_box(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if args.len() != 1 {
        return Some(Err(
            throw!(ArityMismatch => "box expects one argument, found: {}", args.len())(),
        ));
    }

    let allocated_var = ctx.thread.heap.lock().unwrap().allocate(
        args[0].clone(), // TODO: Could actually move off of the stack entirely
        &ctx.thread.stack,
        ctx.thread.stack_frames.iter().map(|x| x.function.as_ref()),
        ctx.thread.global_env.roots(),
        &ctx.thread.thread_local_storage,
        &mut ctx.thread.synchronizer,
    );

    Some(Ok(SteelVal::HeapAllocated(allocated_var)))
}

#[steel_derive::function(name = "unbox-strong")]
pub fn unbox(value: &GcMut<SteelVal>) -> SteelVal {
    value.read().clone()
}

#[steel_derive::function(name = "set-strong-box!")]
pub fn set_box(value: &GcMut<SteelVal>, update_to: SteelVal) {
    *value.write() = update_to;
}

pub fn black_box(_: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "struct->list")]
pub fn struct_to_list(value: &UserDefinedStruct) -> Result<SteelVal> {
    if value.is_transparent() {
        // Ok(SteelVal::ListV((*value.fields).clone().into()))
        Ok(SteelVal::ListV((*value.fields).iter().cloned().collect()))
    } else {
        Ok(SteelVal::BoolV(false))
    }
}

fn meta_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/meta");
    module
        .register_value("#%black-box", SteelVal::FuncV(black_box))
        .register_value(
            "#%function-ptr-table",
            LambdaMetadataTable::new().into_steelval().unwrap(),
        )
        .register_fn("#%function-ptr-table-add", LambdaMetadataTable::add)
        .register_fn("#%function-ptr-table-get", LambdaMetadataTable::get)
        .register_native_fn_definition(LOOKUP_DOC_CTX_DEFINITION)
        .register_fn("#%private-cycle-collector", SteelCycleCollector::from_root)
        .register_fn("#%private-cycle-collector-get", SteelCycleCollector::get)
        .register_fn(
            "#%private-cycle-collector-values",
            SteelCycleCollector::values,
        )
        .register_value("assert!", MetaOperations::assert_truthy())
        .register_value("active-object-count", MetaOperations::active_objects())
        .register_value("memory-address", MetaOperations::memory_address())
        // .register_value("async-exec", MetaOperations::exec_async())
        .register_value("poll!", MetaOperations::poll_value())
        .register_value("block-on", MetaOperations::block_on())
        .register_value(
            "local-executor/block-on",
            MetaOperations::block_on_with_local_executor(),
        )
        .register_value("join!", MetaOperations::join_futures())
        .register_value("futures-join-all", MetaOperations::join_futures())
        .register_fn(
            "#%struct-property-ref",
            |value: &UserDefinedStruct, key: SteelVal| UserDefinedStruct::get(value, &key),
        )
        .register_native_fn_definition(STRUCT_TO_LIST_DEFINITION)
        .register_value("expand!", SteelVal::FuncV(super::meta::expand_macros))
        .register_value("read!", SteelVal::FuncV(super::meta::read))
        .register_value(
            "current-function-span",
            SteelVal::BuiltIn(super::vm::current_function_span),
        )
        .register_value("error-with-span", error_with_src_loc())
        .register_value("raise-error-with-span", error_from_error_with_span())
        .register_value("raise-error", raise_error_from_error())
        .register_native_fn_definition(CALL_CC_DEFINITION)
        .register_native_fn_definition(EVAL_DEFINITION)
        .register_native_fn_definition(EVAL_FILE_DEFINITION)
        .register_native_fn_definition(EXPAND_SYNTAX_OBJECTS_DEFINITION)
        .register_native_fn_definition(MATCH_SYNTAX_CASE_DEFINITION)
        .register_native_fn_definition(EXPAND_SYNTAX_CASE_DEFINITION)
        .register_native_fn_definition(MACRO_CASE_BINDINGS_DEFINITION)
        .register_native_fn_definition(EVAL_STRING_DEFINITION)
        .register_native_fn_definition(CALL_WITH_EXCEPTION_HANDLER_DEFINITION)
        .register_value("breakpoint!", SteelVal::BuiltIn(super::vm::breakpoint))
        .register_native_fn_definition(INSPECT_DEFINITION)
        // TODO: Come back to this
        .register_native_fn_definition(super::vm::EMIT_EXPANDED_FILE_DEFINITION)
        .register_native_fn_definition(super::vm::LOAD_EXPANDED_FILE_DEFINITION)
        .register_value(
            "#%environment-length",
            SteelVal::BuiltIn(super::vm::environment_offset),
        )
        .register_value(
            "call-with-current-continuation",
            SteelVal::BuiltIn(super::vm::call_cc),
        )
        .register_fn("eval!", super::meta::eval)
        .register_fn("value->string", super::meta::value_to_string)
        // TODO: @Matt -> implement the traits for modules as well
        .register_fn("Engine::new", super::meta::EngineWrapper::new)
        .register_fn("Engine::clone", super::meta::EngineWrapper::deep_copy)
        .register_fn("Engine::add-module", super::meta::EngineWrapper::add_module)
        .register_fn("Engine::modules->list", super::meta::EngineWrapper::modules)
        .register_fn(
            "Engine::raise_error",
            super::meta::EngineWrapper::raise_error,
        )
        .register_value("set-test-mode!", SteelVal::BuiltIn(set_test_mode))
        .register_value("get-test-mode", SteelVal::BuiltIn(get_test_mode))
        .register_fn("run!", super::meta::EngineWrapper::call)
        // .register_fn("get-value", super::meta::EngineWrapper::get_value)
        .register_fn("value->iterator", crate::rvals::value_into_iterator)
        .register_value("iter-next!", SteelVal::FuncV(crate::rvals::iterator_next))
        // Check whether the iterator is done
        .register_value("#%iterator-finished", ITERATOR_FINISHED.with(|x| x.clone()))
        .register_value("%iterator?", gen_pred!(BoxedIterator))
        .register_fn("env-var", get_environment_variable)
        .register_fn("maybe-get-env-var", maybe_get_environment_variable)
        // TODO: Maybe just remove this, or provide a steel wrapper in place of this
        .register_fn("set-env-var!", |name, val| unsafe {
            std::env::set_var::<String, String>(name, val)
        })
        .register_fn("arity?", arity)
        .register_fn("function-arity", arity)
        .register_fn("arity-object->list", arity_to_list)
        .register_fn("function-name", lookup_function_name)
        .register_fn("#%native-fn-ptr-doc", lookup_doc)
        .register_fn("#%native-fn-ptr-doc->string", lookup_doc_value)
        .register_fn("multi-arity?", is_multi_arity)
        .register_value("make-struct-type", SteelVal::FuncV(make_struct_type))
        .register_value(
            "#%struct-update",
            SteelVal::MutFunc(struct_update_primitive),
        )
        .register_fn("box-strong", SteelVal::boxed)
        .register_native_fn_definition(UNBOX_DEFINITION)
        .register_native_fn_definition(SET_BOX_DEFINITION)
        .register_native_fn_definition(MAKE_WEAK_BOX_DEFINITION)
        .register_native_fn_definition(WEAK_BOX_VALUE_DEFINITION)
        .register_value("#%box", SteelVal::BuiltIn(make_mutable_box))
        .register_value("#%gc-collect", SteelVal::BuiltIn(gc_collection))
        .register_value("box", SteelVal::BuiltIn(make_mutable_box))
        .register_native_fn_definition(SET_BOX_MUTABLE_DEFINITION)
        .register_native_fn_definition(UNBOX_MUTABLE_DEFINITION)
        .register_native_fn_definition(PLAIN_UNBOX_MUTABLE_DEFINITION)
        .register_native_fn_definition(PLAIN_SET_BOX_MUTABLE_DEFINITION)
        .register_native_fn_definition(MAKE_CALLSTACK_PROFILER_DEFINITION)
        .register_native_fn_definition(CALLSTACK_HYDRATE_NAMES_DEFINITION)
        .register_native_fn_definition(SAMPLE_STACKS_DEFINITION)
        .register_native_fn_definition(DUMP_PROFILER_DEFINITION)
        .register_value(
            "attach-contract-struct!",
            SteelVal::FuncV(attach_contract_struct),
        )
        .register_value("get-contract-struct", SteelVal::FuncV(get_contract))
        .register_fn("current-os!", || std::env::consts::OS)
        .register_fn("target-arch!", || std::env::consts::ARCH)
        .register_fn("platform-dll-prefix!", || std::env::consts::DLL_PREFIX)
        .register_fn("path-separator", || std::path::MAIN_SEPARATOR_STR)
        .register_fn("platform-dll-extension!", || {
            std::env::consts::DLL_EXTENSION
        })
        .register_fn(
            "#%build-dylib",
            |_args: Vec<String>, _env_vars: Vec<(String, String)>| {
                #[cfg(feature = "dylib-build")]
                cargo_steel_lib::run(_args, _env_vars)
                    .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
            },
        )
        .register_fn("feature-dylib-build?", || cfg!(feature = "dylib-build"))
        .register_native_fn_definition(COMMAND_LINE_DEFINITION)
        .register_native_fn_definition(ERROR_OBJECT_MESSAGE_DEFINITION)
        .register_fn("steel-home-location", steel_home)
        .register_fn("%#interner-memory-usage", interned_current_memory_usage);

    module
        .register_native_fn_definition(WILL_EXECUTE_DEFINITION)
        .register_native_fn_definition(WILL_REGISTER_DEFINITION)
        .register_native_fn_definition(MAKE_WILL_EXECUTOR_DEFINITION);

    #[cfg(not(feature = "dylibs"))]
    module.register_native_fn_definition(super::engine::LOAD_MODULE_NOOP_DEFINITION);

    // TODO: Remove
    #[cfg(feature = "dylibs")]
    module.register_native_fn_definition(crate::steel_vm::dylib::LOAD_MODULE_DEFINITION);

    module
}

/// Returns the command line passed to this process,
/// including the command name as first argument.
#[steel_derive::function(name = "command-line")]
fn command_line() -> SteelList<String> {
    std::env::args().collect()
}

/// De/serialization from/to JSON.
#[steel_derive::define_module(name = "steel/json")]
fn json_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/json");
    module
        .register_native_fn_definition(crate::values::json_vals::STRING_TO_JSEXPR_DEFINITION)
        .register_native_fn_definition(
            crate::values::json_vals::SERIALIZE_VAL_TO_STRING_DEFINITION,
        );
    module
}

fn syntax_to_module_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    if let SteelVal::SyntaxObject(s) = &args[0] {
        let span = s.syntax_loc();
        let source = span.source_id();

        if let Some(source) = source {
            let path = ctx.thread.compiler.read().sources.get_path(&source);
            return path
                .map(|x| x.to_str().unwrap().to_string())
                .into_steelval();
        }
    }

    Ok(SteelVal::BoolV(false))
}

#[steel_derive::context(name = "syntax-originating-file", arity = "Exact(1)")]
fn syntax_to_module(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(syntax_to_module_impl(ctx, args))
}

fn syntax_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/syntax");
    module
        .register_fn("syntax->datum", crate::rvals::Syntax::syntax_datum)
        .register_fn("syntax-loc", crate::rvals::Syntax::syntax_loc)
        .register_fn("syntax/loc", crate::rvals::Syntax::new)
        .register_fn("syntax-span", crate::rvals::Syntax::syntax_loc)
        .register_fn("span-file-id", |span: Span| span.source_id.map(|x| x.0))
        .register_fn("#%syntax/raw", crate::rvals::Syntax::proto)
        .register_fn("syntax-e", crate::rvals::Syntax::syntax_e)
        .register_value("syntax?", gen_pred!(SyntaxObject))
        .register_fn("#%debug-syntax->exprkind", |value| {
            let expr = TryFromSteelValVisitorForExprKind::root(&value);

            match expr {
                Ok(v) => {
                    println!("{}", v.to_pretty(60));
                }
                Err(e) => {
                    println!("{}", e);
                }
            }
        })
        .register_native_fn_definition(SYNTAX_TO_MODULE_DEFINITION);
    module
}

// #[derive(Clone, Copy)]
// pub struct SourceLocation {
//     span: Span,
//     source: Option<usize>,
// }

// impl Custom for SourceLocation {}

// TODO: Add integration for native functions to just write something like:
// pub fn dummy(args: RestArgs) where RestArgs just derefs to &[SteelVal] and the arguments
// can be selected that way

pub fn error_with_src_loc() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        let mut error_message = String::new();

        if args.len() < 2 {
            stop!(ArityMismatch => "error-with-span expects at least 2 arguments - the span and the error message")
        }

        if let SteelVal::Void = &args[0] {
            for arg in &args[1..] {
                let error_val = arg.to_string();
                error_message.push(' ');
                error_message.push_str(error_val.trim_matches('\"'));
            }

            stop!(Generic => error_message);
        }

        let span = Span::from_steelval(&args[0])?;

        if !args[1..].is_empty() {
            for arg in &args[1..] {
                let error_val = arg.to_string();
                error_message.push(' ');
                error_message.push_str(error_val.trim_matches('\"'));
            }

            stop!(Generic => error_message; span);
        } else {
            stop!(ArityMismatch => "error-with-span takes at least one argument"; span);
        }
    })
}

pub fn error_from_error_with_span() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 2 {
            stop!(ArityMismatch => "raise-error-with-span expects at least 2 arguments - the error object and the span")
        }

        let mut steel_error = SteelErr::from_steelval(&args[0])?;

        // steel_error.span()

        if let Some(span) = steel_error.span() {
            steel_error.push_span_context_to_stack_trace_if_trace_exists(span);
        }

        let span = Span::from_steelval(&args[1])?;

        Err(steel_error.with_span(span))
    })
}

pub fn raise_error_from_error() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        let steel_error = SteelErr::from_steelval(&args[0])?;

        Err(steel_error)
    })
}

// Be able to introspect on the modules - probably just need to add a modules
// field on the vm, or use a wrapped type with modules to find things
// TODO: Add magic number for modules. - key to magic number, do pointer equality
fn _lookup_doc(_ctx: &mut VmCore, _args: &[SteelVal]) -> Result<SteelVal> {
    // for value in ctx.thread.global_env.bindings_vec.iter() {
    //     if let
    // }

    todo!()
}
