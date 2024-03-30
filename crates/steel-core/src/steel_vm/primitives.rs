use super::{
    builtin::BuiltInModule,
    cache::WeakMemoizationTable,
    engine::Engine,
    register_fn::RegisterFn,
    vm::{get_test_mode, list_modules, set_test_mode, VmCore},
};
use crate::{
    gc::Gc,
    parser::{
        ast::TryFromSteelValVisitorForExprKind, interner::InternedString, span::Span,
        tryfrom_visitor::TryFromExprKindForSteelVal,
    },
    primitives::{
        bytevectors::bytevector_module,
        fs_module,
        hashmaps::{hashmap_module, HM_CONSTRUCT, HM_GET, HM_INSERT},
        hashsets::hashset_module,
        lists::{list_module, UnRecoverableResult},
        numbers, port_module,
        process::process_module,
        random::random_module,
        string_module,
        time::time_module,
        vectors::{
            immutable_vectors_module, MUTABLE_VECTOR_CLEAR_DEFINITION,
            MUTABLE_VECTOR_POP_DEFINITION, MUTABLE_VECTOR_TO_LIST_DEFINITION,
        },
        ControlOperations, IoFunctions, MetaOperations, NumOperations, StreamOperations,
        SymbolOperations, VectorOperations,
    },
    rerrs::ErrorKind,
    rvals::{
        as_underlying_type,
        cycles::{BreadthFirstSearchSteelValVisitor, SteelCycleCollector},
        FromSteelVal, FunctionSignature, MutFunctionSignature, SteelString, ITERATOR_FINISHED,
        NUMBER_EQUALITY_DEFINITION,
    },
    steel_vm::{
        builtin::{get_function_metadata, get_function_name, Arity},
        vm::threads::threading_module,
    },
    values::{
        closed::HeapRef,
        functions::{attach_contract_struct, get_contract, LambdaMetadataTable},
        lists::List,
        structs::{
            build_type_id_module, make_struct_type, struct_update_primitive, SteelResult,
            UserDefinedStruct,
        },
    },
};
use crate::{
    rvals::IntoSteelVal,
    values::structs::{build_option_structs, build_result_structs},
};
use crate::{
    rvals::{Result, SteelVal},
    SteelErr,
};
use fxhash::{FxHashMap, FxHashSet};
use once_cell::sync::Lazy;
use std::{cell::RefCell, cmp::Ordering};

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

const LIST: &str = "list";
const PRIM_LIST: &str = "#%prim.list";
const CAR: &str = "car";
const CDR: &str = "cdr";
const CONS: &str = "cons";
const FIRST: &str = "first";
const REST: &str = "rest";
const APPEND: &str = "append";
const PUSH_BACK: &str = "push-back";
const LENGTH: &str = "length";
const REVERSE: &str = "reverse";
// const LIST_TO_VECTOR: &str = "list->vector";
const LIST_TO_STRING: &str = "list->string";
const NULL_HUH: &str = "null?";
const INT_HUH: &str = "int?";
const INTEGER_HUH: &str = "integer?";
const FLOAT_HUH: &str = "float?";
const NUMBER_HUH: &str = "number?";
const SYMBOL_HUH: &str = "symbol?";
const VECTOR_HUH: &str = "vector?";
const STRING_HUH: &str = "string?";
const LIST_HUH: &str = "list?";
const BOOLEAN_HUH: &str = "boolean?";
const FUNCTION_HUH: &str = "function?";

// TODO: Add the equivalent with prim in front
pub const CONSTANTS: &[&str] = &[
    "+",
    "#%prim.+",
    "i+",
    "#%prim.i+",
    "f+",
    "#%prim.f+",
    "*",
    "#%prim.*",
    "/",
    "#%prim./",
    "-",
    "#%prim.-",
    CAR,
    "#%prim.car",
    CDR,
    "#%prim.cdr",
    FIRST,
    "#%prim.first",
    REST,
    "#%prim.rest",
    // RANGE,
    // "#%prim.range",
    NULL_HUH,
    "#%prim.null?",
    INT_HUH,
    "#%prim.int?",
    FLOAT_HUH,
    "#%prim.float?",
    NUMBER_HUH,
    "#%prim.number?",
    STRING_HUH,
    "#%prim.string?",
    SYMBOL_HUH,
    "#%prim.symbol?",
    VECTOR_HUH,
    "#%prim.vector?",
    LIST_HUH,
    "#%prim.list?",
    INTEGER_HUH,
    "#%prim.integer?",
    BOOLEAN_HUH,
    "#%prim.boolean?",
    FUNCTION_HUH,
    "#%prim.function?",
    "=",
    "#%prim.=",
    "equal?",
    "#%prim.equal?",
    ">",
    "#%prim.>",
    ">=",
    "#%prim.>=",
    "<",
    "#%prim.<",
    "<=",
    "#%prim.<=",
    "string-append",
    "#%prim.string-append",
    "string->list",
    "#%prim.string->list",
    "string-upcase",
    "#%prim.string-upcase",
    "string-lowercase",
    "#%prim.string-lowercase",
    "trim",
    "#%prim.trim",
    "trim-start",
    "#%prim.trim-start",
    "trim-end",
    "#%prim.trim-end",
    "split-whitespace",
    "#%prim.split-whitespace",
    "void",
    "#%prim.void",
    "list->string",
    "#%prim.list->string",
    "concat-symbols",
    "#%prim.concat-symbols",
    "string->int",
    "#%prim.string->int",
    "even?",
    "#%prim.even?",
    "odd",
    CONS,
    "#%prim.cons",
    APPEND,
    "#%prim.append",
    PUSH_BACK,
    LENGTH,
    "#%prim.length",
    REVERSE,
    "#%prim.reverse",
    LIST_TO_STRING,
    "#%prim.list->string",
    LIST,
    PRIM_LIST,
    "#%prim.not",
    "not",
];

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
    pub static PORT_MODULE: BuiltInModule = port_module();
    pub static META_MODULE: BuiltInModule = meta_module();
    pub static JSON_MODULE: BuiltInModule = json_module();
    pub static CONSTANTS_MODULE: BuiltInModule = constants_module();
    pub static SYNTAX_MODULE: BuiltInModule = syntax_module();
    pub static SANDBOXED_META_MODULE: BuiltInModule = sandboxed_meta_module();
    pub static SANDBOXED_IO_MODULE: BuiltInModule = sandboxed_io_module();
    pub static PROCESS_MODULE: BuiltInModule = process_module();
    pub static RANDOM_MODULE: BuiltInModule = random_module();
    pub static RESULT_MODULE: BuiltInModule = build_result_structs();
    pub static TYPE_ID_MODULE: BuiltInModule = build_type_id_module();
    pub static OPTION_MODULE: BuiltInModule = build_option_structs();

    #[cfg(feature = "dylibs")]
    pub static FFI_MODULE: BuiltInModule = ffi_module();

    pub static PRELUDE_MODULE: BuiltInModule = prelude();

    pub(crate) static PRELUDE_INTERNED_STRINGS: FxHashSet<InternedString> = PRELUDE_MODULE.with(|x| x.names().into_iter().map(|x| x.into()).collect());


    pub static TIME_MODULE: BuiltInModule = time_module();
    pub static THREADING_MODULE: BuiltInModule = threading_module();

    pub static MUTABLE_VECTOR_MODULE: BuiltInModule = mutable_vector_module();
    pub static PRIVATE_READER_MODULE: BuiltInModule = reader_module();

}

pub fn prelude() -> BuiltInModule {
    BuiltInModule::new("steel/base")
        .with_module(MAP_MODULE.with(|x| x.clone()))
        .with_module(SET_MODULE.with(|x| x.clone()))
        .with_module(LIST_MODULE.with(|x| x.clone()))
        .with_module(STRING_MODULE.with(|x| x.clone()))
        .with_module(VECTOR_MODULE.with(|x| x.clone()))
        .with_module(STREAM_MODULE.with(|x| x.clone()))
        // .with_module(CONTRACT_MODULE.with(|x| x.clone()))
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

pub fn register_builtin_modules_without_io(engine: &mut Engine) {
    engine.register_fn("##__module-get", BuiltInModule::get);
    engine.register_fn("%module-get%", BuiltInModule::get);

    engine.register_fn("load-from-module!", BuiltInModule::get);

    engine.register_value("%proto-hash%", HM_CONSTRUCT);
    engine.register_value("%proto-hash-insert%", HM_INSERT);
    engine.register_value("%proto-hash-get%", HM_GET);
    engine.register_value("error!", ControlOperations::error());

    engine.register_value("error", ControlOperations::error());

    engine
        .register_module(MAP_MODULE.with(|x| x.clone()))
        .register_module(SET_MODULE.with(|x| x.clone()))
        .register_module(LIST_MODULE.with(|x| x.clone()))
        .register_module(STRING_MODULE.with(|x| x.clone()))
        .register_module(VECTOR_MODULE.with(|x| x.clone()))
        .register_module(STREAM_MODULE.with(|x| x.clone()))
        // .register_module(CONTRACT_MODULE.with(|x| x.clone()))
        .register_module(IDENTITY_MODULE.with(|x| x.clone()))
        .register_module(NUMBER_MODULE.with(|x| x.clone()))
        .register_module(EQUALITY_MODULE.with(|x| x.clone()))
        .register_module(ORD_MODULE.with(|x| x.clone()))
        .register_module(TRANSDUCER_MODULE.with(|x| x.clone()))
        .register_module(SYMBOL_MODULE.with(|x| x.clone()))
        .register_module(SANDBOXED_IO_MODULE.with(|x| x.clone()))
        // .register_module(FS_MODULE.with(|x| x.clone()))
        // .register_module(PORT_MODULE.with(|x| x.clone()))
        .register_module(SANDBOXED_META_MODULE.with(|x| x.clone()))
        .register_module(JSON_MODULE.with(|x| x.clone()))
        .register_module(CONSTANTS_MODULE.with(|x| x.clone()))
        .register_module(SYNTAX_MODULE.with(|x| x.clone()))
        .register_module(PRELUDE_MODULE.with(|x| x.clone()));
}

fn render_as_md(text: String) {
    #[cfg(feature = "markdown")]
    println!("{}", termimad::text(&text));

    #[cfg(not(feature = "markdown"))]
    println!("{}", text);
}

pub fn register_builtin_modules(engine: &mut Engine) {
    engine.register_value("std::env::args", SteelVal::ListV(List::new()));

    engine.register_fn("##__module-get", BuiltInModule::get);
    engine.register_fn("%module-get%", BuiltInModule::get);

    engine.register_fn("load-from-module!", BuiltInModule::get);

    // Registering values in modules
    engine.register_fn("#%module", BuiltInModule::new::<String>);
    engine.register_fn(
        "#%module-add",
        |module: &mut BuiltInModule, name: SteelString, value: SteelVal| {
            module.register_value(&name, value);
        },
    );

    engine.register_fn("%doc?", BuiltInModule::get_doc);
    // engine.register_fn("%module-docs", BuiltInModule::docs);
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

    engine.register_value(
        "%memo-table",
        WeakMemoizationTable::new().into_steelval().unwrap(),
    );
    engine.register_fn("%memo-table-ref", WeakMemoizationTable::get);
    engine.register_fn("%memo-table-set!", WeakMemoizationTable::insert);

    engine
        .register_module(MAP_MODULE.with(|x| x.clone()))
        .register_module(SET_MODULE.with(|x| x.clone()))
        .register_module(LIST_MODULE.with(|x| x.clone()))
        .register_module(STRING_MODULE.with(|x| x.clone()))
        .register_module(VECTOR_MODULE.with(|x| x.clone()))
        .register_module(STREAM_MODULE.with(|x| x.clone()))
        // .register_module(CONTRACT_MODULE.with(|x| x.clone()))
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

    #[cfg(feature = "dylibs")]
    engine.register_module(FFI_MODULE.with(|x| x.clone()));

    // Private module
    engine.register_module(MUTABLE_VECTOR_MODULE.with(|x| x.clone()));
    engine.register_module(PRIVATE_READER_MODULE.with(|x| x.clone()));

    engine.register_module(IMMUTABLE_VECTOR_MODULE.with(|x| x.clone()));
}

pub static MODULE_IDENTIFIERS: Lazy<fxhash::FxHashSet<InternedString>> = Lazy::new(|| {
    let mut set = fxhash::FxHashSet::default();

    // TODO: Consolidate the prefixes and module names into one spot
    set.insert("%-builtin-module-steel/hash".into());
    set.insert("%-builtin-module-steel/sets".into());
    set.insert("%-builtin-module-steel/lists".into());
    set.insert("%-builtin-module-steel/strings".into());
    set.insert("%-builtin-module-steel/vectors".into());
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
                        ("#%prim.".to_string() + x.resolve()).into(),
                    )
                })
                .collect()
        })
    });

pub fn builtin_to_reserved(ident: &str) -> InternedString {
    if let Some(value) = PRELUDE_TO_RESERVED_MAP.get(ident) {
        *value
    } else {
        ("#%prim.".to_string() + ident).into()
    }
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
        .register_value("mutable-vector", VectorOperations::mut_vec_construct())
        .register_value("make-vector", VectorOperations::make_vector())
        .register_value("mutable-vector->list", VectorOperations::mut_vec_to_list())
        .register_value("vector-push!", VectorOperations::mut_vec_push())
        .register_value("mut-vec-len", VectorOperations::mut_vec_length())
        .register_value("vector-length", VectorOperations::vec_length())
        .register_value("vector-append!", VectorOperations::mut_vec_append())
        .register_value("mut-vector-ref", VectorOperations::mut_vec_get())
        .register_value("vector-set!", VectorOperations::mut_vec_set())
        // Immutable vector operations
        .register_value("vector", VectorOperations::vec_construct())
        .register_value("push-front", VectorOperations::vec_cons())
        .register_value("pop-front", VectorOperations::vec_car())
        .register_value("vec-rest", VectorOperations::vec_cdr())
        .register_value("null?", VectorOperations::list_vec_null())
        .register_value("push", VectorOperations::vec_push())
        .register_value("range-vec", VectorOperations::vec_range())
        .register_value("vec-append", VectorOperations::vec_append())
        .register_value("vector-ref", VectorOperations::vec_ref())
        .register_native_fn_definition(MUTABLE_VECTOR_CLEAR_DEFINITION)
        .register_native_fn_definition(MUTABLE_VECTOR_TO_LIST_DEFINITION)
        .register_native_fn_definition(MUTABLE_VECTOR_POP_DEFINITION);
    module
}

#[steel_derive::function(name = "not", constant = true)]
fn not(value: &SteelVal) -> bool {
    matches!(value, SteelVal::BoolV(false))
}

#[steel_derive::function(name = "string?", constant = true)]
fn stringp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::StringV(_))
}

#[steel_derive::function(name = "list?", constant = true)]
fn listp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::ListV(_))
}

#[steel_derive::function(name = "vector?", constant = true)]
fn vectorp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::VectorV(_))
}

#[steel_derive::function(name = "symbol?", constant = true)]
fn symbolp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::SymbolV(_))
}

#[steel_derive::function(name = "hash?", constant = true)]
fn hashp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::HashMapV(_))
}

#[steel_derive::function(name = "set?", constant = true)]
fn hashsetp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::HashSetV(_))
}

#[steel_derive::function(name = "continuation?", constant = true)]
fn continuationp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::ContinuationFunction(_))
}

#[steel_derive::function(name = "boolean?", constant = true)]
fn booleanp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::BoolV(_))
}

#[steel_derive::function(name = "bool?", constant = true)]
fn boolp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::BoolV(_))
}

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

#[steel_derive::function(name = "#%private-struct?", constant = true)]
fn private_structp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::CustomStruct(_))
}

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
        .register_native_fn_definition(PRIVATE_STRUCTP_DEFINITION)
        .register_value("mutable-vector?", gen_pred!(MutableVector))
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
        .register_value("f+", NumOperations::float_add())
        .register_native_fn_definition(numbers::MULTIPLY_PRIMITIVE_DEFINITION)
        .register_native_fn_definition(numbers::DIVIDE_PRIMITIVE_DEFINITION)
        .register_native_fn_definition(numbers::SUBTRACT_PRIMITIVE_DEFINITION)
        .register_value("even?", NumOperations::even())
        .register_value("odd?", NumOperations::odd())
        .register_value("arithmetic-shift", NumOperations::arithmetic_shift())
        .register_native_fn_definition(numbers::ABS_DEFINITION)
        .register_native_fn_definition(numbers::NANP_DEFINITION)
        .register_native_fn_definition(numbers::ZEROP_DEFINITION)
        .register_native_fn_definition(numbers::POSITIVEP_DEFINITION)
        .register_native_fn_definition(numbers::NEGATIVEP_DEFINITION)
        .register_native_fn_definition(numbers::CEILING_DEFINITION)
        .register_native_fn_definition(numbers::DENOMINATOR_DEFINITION)
        .register_native_fn_definition(numbers::EXACTP_DEFINITION)
        .register_native_fn_definition(numbers::EXACT_TO_INEXACT_DEFINITION)
        .register_native_fn_definition(numbers::EXACT_INTEGER_SQRT_DEFINITION)
        .register_native_fn_definition(numbers::EXPT_DEFINITION)
        .register_native_fn_definition(numbers::EXP_DEFINITION)
        .register_native_fn_definition(numbers::FINITEP_DEFINITION)
        .register_native_fn_definition(numbers::FLOOR_DEFINITION)
        .register_native_fn_definition(numbers::INEXACTP_DEFINITION)
        .register_native_fn_definition(numbers::INEXACT_TO_EXACT_DEFINITION)
        .register_native_fn_definition(numbers::INFINITEP_DEFINITION)
        .register_native_fn_definition(numbers::LOG_DEFINITION)
        .register_native_fn_definition(numbers::MAGNITUDE_DEFINITION)
        .register_native_fn_definition(numbers::NUMERATOR_DEFINITION)
        .register_native_fn_definition(numbers::QUOTIENT_DEFINITION)
        .register_native_fn_definition(numbers::ROUND_DEFINITION)
        .register_native_fn_definition(numbers::SQUARE_DEFINITION)
        .register_native_fn_definition(numbers::SQRT_DEFINITION);

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
        .register_value("eqv?", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a == b)))
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

fn ord_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ord");
    module
        .register_value(">", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a > b)))
        .register_value(">=", SteelVal::FuncV(gte_primitive))
        .register_value("<", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a < b)))
        .register_value("<=", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a <= b)));
    module
}

pub fn transducer_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/transducers");

    use crate::primitives::transducers::*;

    module
        .register_native_fn("compose", compose, Arity::AtLeast(0))
        .register_native_fn("mapping", map, Arity::Exact(1))
        .register_native_fn("flattening", flatten, Arity::Exact(0))
        .register_native_fn("flat-mapping", flat_map, Arity::Exact(1))
        .register_native_fn("filtering", filter, Arity::Exact(1))
        .register_native_fn("taking", take, Arity::Exact(1))
        .register_native_fn("dropping", dropping, Arity::Exact(1))
        .register_native_fn("extending", extending, Arity::Exact(1))
        .register_native_fn("enumerating", enumerating, Arity::Exact(0))
        .register_native_fn("zipping", zipping, Arity::Exact(1))
        .register_native_fn("interleaving", interleaving, Arity::Exact(1))
        .register_value("into-sum", crate::values::transducers::INTO_SUM)
        .register_value("into-product", crate::values::transducers::INTO_PRODUCT)
        .register_value("into-max", crate::values::transducers::INTO_MAX)
        .register_value("into-min", crate::values::transducers::INTO_MIN)
        .register_value("into-count", crate::values::transducers::INTO_COUNT)
        .register_value("into-list", crate::values::transducers::INTO_LIST)
        .register_value("into-vector", crate::values::transducers::INTO_VECTOR)
        .register_value("into-hashmap", crate::values::transducers::INTO_HASHMAP)
        .register_value("into-hashset", crate::values::transducers::INTO_HASHSET)
        .register_value("into-string", crate::values::transducers::INTO_STRING)
        .register_value("into-last", crate::values::transducers::INTO_LAST)
        .register_value("into-for-each", crate::values::transducers::FOR_EACH)
        .register_value("into-nth", crate::values::transducers::NTH)
        .register_value("into-reducer", crate::values::transducers::REDUCER);
    module
}

fn symbol_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/symbols");
    module
        .register_value("concat-symbols", SymbolOperations::concat_symbols())
        .register_value("symbol->string", SymbolOperations::symbol_to_string());
    module
}

fn io_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/io");
    module
        // .register_value("display", IoFunctions::display())
        // .register_value("displayln", IoFunctions::displayln())
        // .register_value("simple-display", IoFunctions::display())
        .register_value("stdout-simple-displayln", IoFunctions::displayln())
        // .register_value("newline", IoFunctions::newline())
        .register_value("read-to-string", IoFunctions::read_to_string());

    // #[cfg(feature = "colors")]
    // module.register_value("display-color", IoFunctions::display_color());

    module
}

fn sandboxed_io_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/io");
    module
        .register_value("stdout-simple-displayln", IoFunctions::displayln())
        // .register_value("newline", IoFunctions::newline())
        .register_value("read-to-string", IoFunctions::read_to_string());
    // .register_value("display", IoFunctions::sandboxed_display())
    // .register_value("display-color", IoFunctions::display_color())
    // .register_value("newline", IoFunctions::sandboxed_newline());
    // .register_value("read-to-string", IoFunctions::read_to_string());
    module
}

fn constants_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/constants");
    module.register_value("void", SteelVal::Void);
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
        .register_value("inspect-bytecode", MetaOperations::inspect_bytecode())
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

fn lookup_function_name(value: SteelVal) -> Option<SteelVal> {
    match value {
        SteelVal::BoxedFunction(f) => f.name().map(|x| x.into_steelval().unwrap()),
        SteelVal::FuncV(f) => get_function_name(f).map(|x| x.name.into_steelval().unwrap()),
        _ => None,
    }
}

fn lookup_doc(value: SteelVal) -> bool {
    match value {
        // SteelVal::BoxedFunction(f) => ,
        SteelVal::FuncV(f) => {
            let metadata = get_function_metadata(
                crate::steel_vm::builtin::BuiltInFunctionTypePointer::Reference(
                    f as *const FunctionSignature,
                ),
            );

            if let Some(data) = metadata.and_then(|x| x.doc) {
                println!("{}", data);
                true
            } else {
                false
            }
        }
        SteelVal::MutFunc(f) => {
            let metadata = get_function_metadata(
                crate::steel_vm::builtin::BuiltInFunctionTypePointer::Mutable(
                    f as *const MutFunctionSignature,
                ),
            );
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

    fn read_one(&mut self) -> Result<SteelVal> {
        if let Some(buffer) = self.buffer.get(self.offset..) {
            // println!("Reading range: {}", buffer);

            let mut parser = crate::parser::parser::Parser::new_flat(buffer, None);

            if let Some(raw) = parser.next() {
                // self.offset += parser.offset();

                let next = if let Ok(next) = raw {
                    next
                } else {
                    return Ok(SteelVal::Void);
                };

                self.offset += parser.offset();

                let result = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(next);

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
            Ok(SteelVal::SymbolV(
                crate::primitives::ports::EOF_OBJECT.with(|x| x.clone()),
            ))
        }
    }
}

fn reader_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("#%private/steel/reader");

    module
        .register_fn("new-reader", Reader::create_reader)
        .register_fn("reader-push-string", Reader::push_string)
        .register_fn("reader-read-one", Reader::read_one)
        .register_fn("reader-empty?", Reader::is_empty);

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

// TODO: Handle arity issues!!!
fn make_mutable_box(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let allocated_var = ctx.thread.heap.allocate(
        args[0].clone(), // TODO: Could actually move off of the stack entirely
        ctx.thread.stack.iter(),
        ctx.thread.stack_frames.iter().map(|x| x.function.as_ref()),
        ctx.thread.global_env.roots(),
    );

    Some(Ok(SteelVal::HeapAllocated(allocated_var)))
}

#[steel_derive::function(name = "unbox-strong")]
pub fn unbox(value: &Gc<RefCell<SteelVal>>) -> SteelVal {
    value.borrow().clone()
}

#[steel_derive::function(name = "set-strong-box!")]
pub fn set_box(value: &Gc<RefCell<SteelVal>>, update_to: SteelVal) {
    *value.borrow_mut() = update_to;
}

pub fn black_box(_: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "struct->list")]
pub fn struct_to_list(value: &UserDefinedStruct) -> Result<SteelVal> {
    if value.is_transparent() {
        Ok(SteelVal::ListV(value.fields.clone().into()))
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
        .register_fn("#%private-cycle-collector", SteelCycleCollector::from_root)
        .register_fn("#%private-cycle-collector-get", SteelCycleCollector::get)
        .register_fn(
            "#%private-cycle-collector-values",
            SteelCycleCollector::values,
        )
        .register_value("assert!", MetaOperations::assert_truthy())
        .register_value("active-object-count", MetaOperations::active_objects())
        .register_value("inspect-bytecode", MetaOperations::inspect_bytecode())
        .register_value("memory-address", MetaOperations::memory_address())
        // .register_value("async-exec", MetaOperations::exec_async())
        .register_value("poll!", MetaOperations::poll_value())
        .register_value("block-on", MetaOperations::block_on())
        .register_value(
            "local-executor/block-on",
            MetaOperations::block_on_with_local_executor(),
        )
        .register_value("join!", MetaOperations::join_futures())
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
        .register_value("call/cc", SteelVal::BuiltIn(super::vm::call_cc))
        .register_value(
            "call-with-exception-handler",
            SteelVal::BuiltIn(super::vm::call_with_exception_handler),
        )
        .register_value("breakpoint!", SteelVal::BuiltIn(super::vm::breakpoint))
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
        .register_fn("set-env-var!", std::env::set_var::<String, String>)
        .register_fn("arity?", arity)
        .register_fn("function-name", lookup_function_name)
        .register_fn("#%native-fn-ptr-doc", lookup_doc)
        .register_fn("multi-arity?", is_multi_arity)
        .register_value("make-struct-type", SteelVal::FuncV(make_struct_type))
        .register_value(
            "#%struct-update",
            SteelVal::MutFunc(struct_update_primitive),
        )
        .register_fn("box-strong", SteelVal::boxed)
        .register_native_fn_definition(UNBOX_DEFINITION)
        .register_native_fn_definition(SET_BOX_DEFINITION)
        .register_value("#%box", SteelVal::BuiltIn(make_mutable_box))
        .register_value("box", SteelVal::BuiltIn(make_mutable_box))
        .register_native_fn_definition(SET_BOX_MUTABLE_DEFINITION)
        .register_native_fn_definition(UNBOX_MUTABLE_DEFINITION)
        .register_native_fn_definition(PLAIN_UNBOX_MUTABLE_DEFINITION)
        .register_native_fn_definition(PLAIN_SET_BOX_MUTABLE_DEFINITION)
        .register_value(
            "attach-contract-struct!",
            SteelVal::FuncV(attach_contract_struct),
        )
        .register_value("get-contract-struct", SteelVal::FuncV(get_contract))
        .register_fn("current-os!", || std::env::consts::OS)
        .register_fn("#%build-dylib", || {
            #[cfg(feature = "dylib-build")]
            cargo_steel_lib::run().ok()
        });

    #[cfg(not(feature = "dylibs"))]
    module.register_native_fn_definition(super::engine::LOAD_MODULE_NOOP_DEFINITION);

    // TODO: Remove
    #[cfg(feature = "dylibs")]
    module.register_native_fn_definition(crate::steel_vm::dylib::LOAD_MODULE_DEFINITION);

    module
}

fn json_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/json");
    module
        .register_value(
            "string->jsexpr",
            crate::values::json_vals::string_to_jsexpr(),
        )
        .register_value(
            "value->jsexpr-string",
            crate::values::json_vals::serialize_val_to_string(),
        );
    module
}

fn syntax_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/syntax");
    module
        .register_fn("syntax->datum", crate::rvals::Syntax::syntax_datum)
        .register_fn("syntax-loc", crate::rvals::Syntax::syntax_loc)
        .register_fn("syntax/loc", crate::rvals::Syntax::new)
        .register_fn("syntax-span", crate::rvals::Syntax::syntax_loc)
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
        });
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
