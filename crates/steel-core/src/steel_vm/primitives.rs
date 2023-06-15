use std::cmp::Ordering;

use super::{
    builtin::BuiltInModule,
    cache::WeakMemoizationTable,
    engine::Engine,
    register_fn::RegisterFn,
    vm::{apply, get_test_mode, list_modules, set_test_mode, VmCore, APPLY_DOC},
};
use crate::{
    parser::span::Span,
    primitives::{
        colors::string_coloring_module,
        contracts,
        hashmaps::hashmap_module,
        hashmaps::{HM_CONSTRUCT, HM_GET, HM_INSERT},
        hashsets::hashset_module,
        lists::{
            UnRecoverableResult, APPEND_DOC, CAR_DOC, CDR_DOC, CONS_DOC, FIRST_DOC, IS_EMPTY_DOC,
            LAST_DOC, LENGTH_DOC, LIST_DOC, LIST_REF_DOC, RANGE_DOC, REST_DOC, REVERSE_DOC,
            SECOND_DOC, THIRD_DOC,
        },
        nums::quotient,
        process::process_module,
        random::random_module,
        string_module,
        time::time_module,
        ControlOperations, FsFunctions, IoFunctions, MetaOperations, NumOperations, PortOperations,
        StreamOperations, StringOperations, SymbolOperations, VectorOperations,
    },
    rerrs::ErrorKind,
    rvals::{Custom, FromSteelVal, SerializableSteelVal},
    steel_vm::builtin::Arity,
    values::{
        closed::HeapRef,
        functions::{attach_contract_struct, get_contract},
        structs::make_struct_type,
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

#[cfg(feature = "web")]
use crate::primitives::web::{requests::requests_module, websockets::websockets_module};

use itertools::Itertools;

macro_rules! ensure_tonicity_two {
    ($check_fn:expr) => {{
        |args: &[SteelVal]| -> Result<SteelVal> {

            if args.is_empty() {
                stop!(ArityMismatch => "expected at least one argument");
            }

            for (left, right) in args.iter().tuple_windows() {
                if !$check_fn(left, right) {
                    return Ok(SteelVal::BoolV(false))
                }
            }

            Ok(SteelVal::BoolV(true))

        }
    }};
}

fn is_void() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        Ok(if let Some(SteelVal::Void) = args.first() {
            SteelVal::BoolV(true)
        } else {
            SteelVal::BoolV(false)
        })
    })
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
const CAR: &str = "car";
const CDR: &str = "cdr";
const CONS: &str = "cons";
const FIRST: &str = "first";
const REST: &str = "rest";
const APPEND: &str = "append";
const PUSH_BACK: &str = "push-back";
const RANGE: &str = "range";
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

pub const CONSTANTS: &[&str] = &[
    "+",
    "i+",
    "f+",
    "*",
    "/",
    "-",
    CAR,
    CDR,
    FIRST,
    REST,
    RANGE,
    NULL_HUH,
    INT_HUH,
    FLOAT_HUH,
    NUMBER_HUH,
    STRING_HUH,
    SYMBOL_HUH,
    VECTOR_HUH,
    LIST_HUH,
    INTEGER_HUH,
    BOOLEAN_HUH,
    FUNCTION_HUH,
    "=",
    "equal?",
    ">",
    ">=",
    "<",
    "<=",
    "string-append",
    "string->list",
    "string-upcase",
    "string-lowercase",
    "trim",
    "trim-start",
    "trim-end",
    "split-whitespace",
    "void",
    "list->string",
    "concat-symbols",
    "string->int",
    "even?",
    "odd",
    CONS,
    APPEND,
    PUSH_BACK,
    LENGTH,
    REVERSE,
    LIST_TO_STRING,
    LIST,
];

thread_local! {
    pub static MAP_MODULE: BuiltInModule = hashmap_module();
    pub static SET_MODULE: BuiltInModule = hashset_module();
    pub static LIST_MODULE: BuiltInModule = list_module();
    pub static STRING_MODULE: BuiltInModule = string_module();
    pub static VECTOR_MODULE: BuiltInModule = vector_module();
    pub static STREAM_MODULE: BuiltInModule = stream_module();
    pub static CONTRACT_MODULE: BuiltInModule = contract_module();
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
    pub static OPTION_MODULE: BuiltInModule = build_option_structs();
    pub static PRELUDE_MODULE: BuiltInModule = prelude();
    pub static TIME_MODULE: BuiltInModule = time_module();

    #[cfg(feature = "web")]
    pub static WEBSOCKETS_MODULE: BuiltInModule = websockets_module();

    #[cfg(feature = "web")]
    pub static REQUESTS_MODULE: BuiltInModule = requests_module();

    #[cfg(feature = "blocking_requests")]
    pub static BLOCKING_REQUESTS_MODULE: BuiltInModule = crate::primitives::blocking_requests::blocking_requests_module();

    pub static STRING_COLORS_MODULE: BuiltInModule = string_coloring_module();

    #[cfg(feature = "sqlite")]
    pub static SQLITE_MODULE: BuiltInModule = crate::primitives::sqlite::sqlite_module();
}

pub fn prelude() -> BuiltInModule {
    BuiltInModule::new("steel/base")
        .with_module(MAP_MODULE.with(|x| x.clone()))
        .with_module(SET_MODULE.with(|x| x.clone()))
        .with_module(LIST_MODULE.with(|x| x.clone()))
        .with_module(STRING_MODULE.with(|x| x.clone()))
        .with_module(VECTOR_MODULE.with(|x| x.clone()))
        .with_module(STREAM_MODULE.with(|x| x.clone()))
        .with_module(CONTRACT_MODULE.with(|x| x.clone()))
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
        .with_module(TIME_MODULE.with(|x| x.clone()))
}

pub fn register_builtin_modules_without_io(engine: &mut Engine) {
    engine.register_fn("##__module-get", BuiltInModule::get);
    engine.register_fn("%module-get%", BuiltInModule::get);

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
        .register_module(CONTRACT_MODULE.with(|x| x.clone()))
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
    println!("{}", termimad::text(&text))
}

pub fn register_builtin_modules(engine: &mut Engine) {
    engine.register_fn("##__module-get", BuiltInModule::get);
    engine.register_fn("%module-get%", BuiltInModule::get);
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
        .register_module(CONTRACT_MODULE.with(|x| x.clone()))
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
        .register_module(PRELUDE_MODULE.with(|x| x.clone()))
        .register_module(TIME_MODULE.with(|x| x.clone()))
        .register_module(STRING_COLORS_MODULE.with(|x| x.clone()))
        .register_module(RANDOM_MODULE.with(|x| x.clone()));

    #[cfg(feature = "web")]
    engine
        .register_module(WEBSOCKETS_MODULE.with(|x| x.clone()))
        .register_module(REQUESTS_MODULE.with(|x| x.clone()));

    #[cfg(feature = "sqlite")]
    engine.register_module(SQLITE_MODULE.with(|x| x.clone()));

    #[cfg(feature = "blocking_requests")]
    engine.register_module(BLOCKING_REQUESTS_MODULE.with(|x| x.clone()));
}

pub static ALL_MODULES: &str = r#"
    (require-builtin steel/hash)
    (require-builtin steel/sets)
    (require-builtin steel/lists)
    (require-builtin steel/strings)
    (require-builtin steel/symbols)
    (require-builtin steel/vectors)
    (require-builtin steel/streams)
    (require-builtin steel/contracts)
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
"#;

pub static SANDBOXED_MODULES: &str = r#"
    (require-builtin steel/hash)
    (require-builtin steel/sets)
    (require-builtin steel/lists)
    (require-builtin steel/strings)
    (require-builtin steel/symbols)
    (require-builtin steel/vectors)
    (require-builtin steel/streams)
    (require-builtin steel/contracts)
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

// static MAP_MODULE: Lazy<BuiltInModule> = Lazy::new(hashmap);
// static SET_MODULE: Lazy<BuiltInModule> = Lazy::new(hashset);

pub(crate) const TEST_APPLY: SteelVal = SteelVal::BuiltIn(apply);

fn list_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/lists");

    // Register the doc for the module
    module.register_doc("steel/lists", crate::primitives::lists::LIST_MODULE_DOC);

    // Grab a raw handle to a list
    module.register_native_fn(
        "%raw-list",
        crate::primitives::lists::new,
        Arity::AtLeast(0),
    );

    module
        .register_value_with_doc(LIST, crate::primitives::lists::LIST, LIST_DOC)
        .register_value_with_doc(CONS, crate::primitives::lists::CONS, CONS_DOC)
        .register_value_with_doc(RANGE, crate::primitives::lists::RANGE, RANGE_DOC)
        .register_value_with_doc(LENGTH, crate::primitives::lists::LENGTH, LENGTH_DOC)
        .register_value_with_doc("last", crate::primitives::lists::LAST, LAST_DOC)
        .register_value_with_doc("empty?", crate::primitives::lists::IS_EMPTY, IS_EMPTY_DOC)
        .register_value_with_doc(CAR, crate::primitives::lists::CAR, CAR_DOC)
        .register_value_with_doc(FIRST, crate::primitives::lists::CAR, FIRST_DOC)
        .register_value_with_doc(CDR, crate::primitives::lists::CDR, CDR_DOC)
        .register_value_with_doc(REST, crate::primitives::lists::REST, REST_DOC)
        .register_value_with_doc(APPEND, crate::primitives::lists::APPEND, APPEND_DOC)
        .register_value_with_doc(REVERSE, crate::primitives::lists::REVERSE, REVERSE_DOC)
        .register_value_with_doc("list-ref", crate::primitives::lists::LIST_REF, LIST_REF_DOC)
        .register_value("try-list-ref", crate::primitives::lists::TRY_LIST_REF)
        .register_value("list->string", crate::primitives::lists::LIST_TO_STRING)
        .register_value("push-back", crate::primitives::lists::PUSH_BACK)
        .register_value("pair?", crate::primitives::lists::PAIR)
        // .register_value("test-push-back", crate::primitives::alternative_list::PU)
        // .register_value("test-map", crate::primitives::lists::TEST_MAP)
        // TODO move this to somewhere better than here
        .register_value_with_doc("apply", TEST_APPLY, APPLY_DOC)
        // .register_value("transduce", crate::steel_vm::transducers::TRANSDUCE)
        // .register_value("execute", crate::steel_vm::transducers::EXECUTE)
        .register_value("transduce", crate::steel_vm::transducers::TRANSDUCE)
        .register_fn("second", crate::primitives::lists::second)
        .register_fn("third", crate::primitives::lists::third);

    module.register_doc("second", SECOND_DOC);
    module.register_doc("third", THIRD_DOC);

    module
}

fn vector_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/vectors");
    module
        .register_value("mutable-vector", VectorOperations::mut_vec_construct())
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
        .register_value("vector-ref", VectorOperations::vec_ref());
    module
}

fn identity_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/identity");
    module
        .register_value("int?", gen_pred!(IntV))
        .register_value("float?", gen_pred!(NumV))
        .register_value("number?", gen_pred!(NumV, IntV))
        .register_value("string?", gen_pred!(StringV))
        .register_value("symbol?", gen_pred!(SymbolV))
        .register_value("vector?", gen_pred!(VectorV))
        // .register_value("struct?", gen_pred!(StructV))
        .register_value("list?", gen_pred!(ListV))
        .register_value("hash?", gen_pred!(HashMapV))
        .register_value("mutable-vector?", gen_pred!(MutableVector))
        // .register_value("pair?", gen_pred!(ListV))
        .register_value("integer?", gen_pred!(IntV))
        .register_value("boolean?", gen_pred!(BoolV))
        .register_value("char?", gen_pred!(CharV))
        .register_value("void?", is_void())
        .register_value("continuation?", gen_pred!(ContinuationFunction))
        .register_value("future?", gen_pred!(FutureV))
        .register_value(
            "function?",
            gen_pred!(
                Closure,
                FuncV,
                ContractedFunction,
                BoxedFunction,
                ContinuationFunction,
                MutFunc,
                BuiltIn
            ),
        )
        .register_value(
            "procedure?",
            gen_pred!(
                Closure,
                FuncV,
                ContractedFunction,
                BoxedFunction,
                ContinuationFunction,
                MutFunc,
                BuiltIn
            ),
        )
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
        .register_value("stream-cdr'", StreamOperations::stream_cdr());
    module
}

fn contract_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/contracts");
    module
        .register_value("bind/c", contracts::BIND_CONTRACT_TO_FUNCTION)
        .register_value("make-flat/c", contracts::MAKE_FLAT_CONTRACT)
        .register_value(
            "make-dependent-function/c",
            contracts::MAKE_DEPENDENT_CONTRACT,
        )
        .register_value("make-function/c", contracts::MAKE_FUNCTION_CONTRACT)
        .register_value("make/c", contracts::MAKE_C);

    module
}

fn number_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/numbers");
    module
        .register_value("+", NumOperations::adder())
        .register_value("f+", NumOperations::float_add())
        .register_value("*", NumOperations::multiply())
        .register_value("/", NumOperations::divide())
        .register_value("-", NumOperations::subtract())
        .register_value("even?", NumOperations::even())
        .register_value("odd?", NumOperations::odd())
        .register_fn("quotient", quotient)
        .register_value("arithmetic-shift", NumOperations::arithmetic_shift());
    module
}

#[inline(always)]
pub fn equality_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    // if args.is_empty() {
    //     stop!(ArityMismatch => "expected at least one argument");
    // }

    // for (left, right) in args.iter().tuple_windows() {
    //     if left != right {
    //         return Ok(SteelVal::BoolV(false));
    //     }
    // }

    Ok(SteelVal::BoolV(args.windows(2).all(|x| x[0] == x[1])))

    // Ok(SteelVal::BoolV(true))
}

// TODO: Align with the above using windows
pub fn gte_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        stop!(ArityMismatch => "expected at least one argument");
    }

    for (left, right) in args.iter().tuple_windows() {
        match left.partial_cmp(right) {
            None | Some(Ordering::Less) => return Ok(SteelVal::BoolV(false)),
            _ => continue,
        }
    }

    Ok(SteelVal::BoolV(true))
}

#[inline(always)]
pub fn lte_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        stop!(ArityMismatch => "expected at least one argument");
    }

    // Ok(SteelVal::BoolV(args.is_sorted_by()))

    // self.as_slice().windows(2).all(|w| {
    //     compare(&&w[0], &&w[1]).map(|o| o != Ordering::Greater).unwrap_or(false)
    // })

    // for &[left, right] in args.windows(2) {
    //     match left.partial_cmp(&right) {
    //         None | Some(Ordering::Greater) => return Ok(SteelVal::BoolV(false)),
    //         _ => continue,
    //     }
    // }

    Ok(SteelVal::BoolV(args.windows(2).all(|x| {
        x[0].partial_cmp(&x[1])
            .map(|x| x != Ordering::Greater)
            .unwrap_or(false)
    })))

    // Ok(SteelVal::BoolV(true))
}

fn equality_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/equality");
    module
        .register_value(
            "equal?",
            SteelVal::FuncV(ensure_tonicity_two!(|a, b| a == b)),
        )
        .register_value(
            "eq?",
            SteelVal::FuncV(ensure_tonicity_two!(
                |a: &SteelVal, b: &SteelVal| a.ptr_eq(b)
            )),
        )
        .register_value("=", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a == b)));
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
        .register_native_fn("flattening", flatten, Arity::Exact(1))
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
        .register_value("display", IoFunctions::display())
        .register_value("displayln", IoFunctions::displayln())
        .register_value("display-color", IoFunctions::display_color())
        .register_value("newline", IoFunctions::newline())
        .register_value("read-to-string", IoFunctions::read_to_string());
    module
}

fn sandboxed_io_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/io");
    module
        .register_value("display", IoFunctions::sandboxed_display())
        // .register_value("display-color", IoFunctions::display_color())
        .register_value("newline", IoFunctions::sandboxed_newline());
    // .register_value("read-to-string", IoFunctions::read_to_string());
    module
}

fn constants_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/constants");
    module.register_value("void", SteelVal::Void);
    module
}

fn fs_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/filesystem");
    module
        .register_value("is-dir?", FsFunctions::is_dir())
        .register_value("is-file?", FsFunctions::is_file())
        .register_value("read-dir", FsFunctions::read_dir())
        .register_value("path-exists?", FsFunctions::path_exists())
        .register_value(
            "copy-directory-recursively!",
            FsFunctions::copy_directory_recursively(),
        )
        .register_value("delete-directory!", FsFunctions::delete_directory())
        .register_value("create-directory!", FsFunctions::create_dir_all())
        .register_value("file-name", FsFunctions::file_name())
        .register_value("current-directory", FsFunctions::current_dir())
        .register_value(
            "path->extension",
            SteelVal::FuncV(FsFunctions::get_extension),
        );
    module
}

fn port_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ports");
    module
        .register_value("open-input-file", PortOperations::open_input_file())
        .register_value("open-output-file", PortOperations::open_output_file())
        .register_value("write-line!", PortOperations::write_line())
        .register_value("read-port-to-string", PortOperations::read_port_to_string())
        .register_value("read-line-from-port", PortOperations::read_line_to_string())
        .register_value("stdin", SteelVal::FuncV(PortOperations::open_stdin));
    module
}

fn get_environment_variable(var: String) -> Result<SteelVal> {
    std::env::var(var)
        .map(|x| x.into_steelval().unwrap())
        .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
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

// Only works with fixed size arity functions
fn arity(value: SteelVal) -> UnRecoverableResult {
    match value {
        SteelVal::Closure(c) => Ok(SteelVal::IntV(c.arity() as isize)).into(),
        SteelVal::BoxedFunction(f) => f
            .get_arity()
            .map(|x| SteelVal::IntV(x as isize))
            .ok_or(SteelErr::new(
                ErrorKind::TypeMismatch,
                "Unable to find the arity for the given function".to_string(),
            ))
            // .ok_or(steelerr!(TypeMismatch => "Unable to find the arity for the give function"))
            .into(),

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

// Move values back and forth across threads!
impl Custom for std::sync::mpsc::Sender<SerializableSteelVal> {
    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        Some(SerializableSteelVal::Custom(Box::new(self.clone())))
    }
}

struct SReceiver {
    receiver: Option<std::sync::mpsc::Receiver<SerializableSteelVal>>,
}

impl Custom for SReceiver {
    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        let inner = self.receiver.take();

        let new_channel = SReceiver { receiver: inner };

        Some(SerializableSteelVal::Custom(Box::new(new_channel)))
    }
}

// impl Custom for std::sync::mpsc::Receiver<SerializableSteelVal> {
//     fn into_serializable_steelval(&self) -> Option<SerializableSteelVal> {
//         Some(SerializableSteelVal::Custom(Box::new(self.clone())))
//     }
// }

impl Custom for std::thread::ThreadId {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("#<{:?}>", self)))
    }
}

fn meta_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/meta");
    module
        .register_value("assert!", MetaOperations::assert_truthy())
        .register_value("active-object-count", MetaOperations::active_objects())
        .register_value("inspect-bytecode", MetaOperations::inspect_bytecode())
        .register_value("memory-address", MetaOperations::memory_address())
        // .register_value("async-exec", MetaOperations::exec_async())
        .register_value("poll!", MetaOperations::poll_value())
        .register_value("block-on", MetaOperations::block_on())
        .register_value("join!", MetaOperations::join_futures())
        // .register_value("struct-ref", struct_ref())
        // .register_value("struct->list", struct_to_list())
        // .register_value("struct->vector", struct_to_vector())
        .register_value("expand!", SteelVal::FuncV(super::meta::expand_macros))
        .register_value("read!", SteelVal::FuncV(super::meta::read))
        .register_value(
            "current-function-span",
            SteelVal::BuiltIn(super::vm::current_function_span),
        )
        .register_value("error-with-span", error_with_src_loc())
        .register_value("raise-error-with-span", error_from_error_with_span())
        .register_value("call/cc", SteelVal::BuiltIn(super::vm::call_cc))
        .register_value(
            "call-with-exception-handler",
            SteelVal::BuiltIn(super::vm::call_with_exception_handler),
        )
        .register_value(
            "call-with-current-continuation",
            SteelVal::BuiltIn(super::vm::call_cc),
        )
        .register_fn("eval!", super::meta::eval)
        .register_fn("value->string", super::meta::value_to_string)
        // TODO: @Matt -> implement the traits for modules as well
        .register_fn("Engine::new", super::meta::EngineWrapper::new)
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
        .register_value("%iterator?", gen_pred!(BoxedIterator))
        .register_fn("env-var", get_environment_variable)
        .register_fn("set-env-var!", std::env::set_var::<String, String>)
        .register_fn("arity?", arity)
        .register_fn("multi-arity?", is_multi_arity)
        .register_value("make-struct-type", SteelVal::FuncV(make_struct_type))
        // .register_fn("struct-properties", UserDefinedStruct::properties)
        .register_value(
            "box",
            SteelVal::BuiltIn(crate::primitives::meta_ops::steel_box),
        )
        .register_fn("unbox", HeapRef::get)
        .register_fn("set-box!", HeapRef::set_interior_mut)
        .register_value(
            "attach-contract-struct!",
            SteelVal::FuncV(attach_contract_struct),
        )
        .register_value("get-contract-struct", SteelVal::FuncV(get_contract))
        .register_value("spawn-thread!", SteelVal::BuiltIn(super::vm::spawn_thread))
        .register_fn("thread-join!", super::vm::thread_join)
        .register_fn("make-channels", || {
            let (left, right) = std::sync::mpsc::channel::<SerializableSteelVal>();

            crate::list![
                left,
                SReceiver {
                    receiver: Some(right)
                }
            ]
        })
        .register_fn(
            "channel->send",
            |channel: &std::sync::mpsc::Sender<SerializableSteelVal>,
             val: SteelVal|
             -> Result<()> {
                let serializable = crate::rvals::into_serializable_value(val)?;

                channel
                    .send(serializable)
                    .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))

                // todo!()
            },
        )
        .register_fn("channel->recv", |channel: &SReceiver| -> Result<SteelVal> {
            // let serializable = crate::rvals::into_serializable_value(val)?;

            let receiver = channel
                .receiver
                .as_ref()
                .expect("Channel should not be dropped here!");

            let value = receiver
                .recv()
                .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))?;

            let value = crate::rvals::from_serializable_value(value);

            Ok(value)
        })
        .register_fn(
            "channel->try-recv",
            |channel: &SReceiver| -> Result<Option<SteelVal>> {
                // let serializable = crate::rvals::into_serializable_value(val)?;

                let receiver = channel
                    .receiver
                    .as_ref()
                    .expect("Channel should not be dropped here!");

                let value = receiver.try_recv();

                match value {
                    Ok(v) => return Ok(Some(crate::rvals::from_serializable_value(v))),
                    Err(std::sync::mpsc::TryRecvError::Empty) => return Ok(None),
                    Err(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
                }
            },
        )
        .register_fn("thread::current/id", || std::thread::current().id())
        .register_fn("current-os!", || std::env::consts::OS);
    module
}

// TODO: Document these
fn threading_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/threads");

    module
        .register_value("spawn-thread!", SteelVal::BuiltIn(super::vm::spawn_thread))
        .register_fn("thread-join!", super::vm::thread_join)
        .register_fn("make-channels", || {
            let (left, right) = std::sync::mpsc::channel::<SerializableSteelVal>();

            crate::list![
                left,
                SReceiver {
                    receiver: Some(right)
                }
            ]
        })
        .register_fn(
            "channel->send",
            |channel: &std::sync::mpsc::Sender<SerializableSteelVal>,
             val: SteelVal|
             -> Result<()> {
                let serializable = crate::rvals::into_serializable_value(val)?;

                channel
                    .send(serializable)
                    .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))

                // todo!()
            },
        )
        .register_fn("channel->recv", |channel: &SReceiver| -> Result<SteelVal> {
            // let serializable = crate::rvals::into_serializable_value(val)?;

            let receiver = channel
                .receiver
                .as_ref()
                .expect("Channel should not be dropped here!");

            let value = receiver
                .recv()
                .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))?;

            let value = crate::rvals::from_serializable_value(value);

            Ok(value)
        });

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
        .register_value("syntax?", gen_pred!(SyntaxObject));
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

        let steel_error = SteelErr::from_steelval(&args[0])?;

        let span = Span::from_steelval(&args[1])?;

        Err(steel_error.with_span(span))
    })
}

// Be able to introspect on the modules - probably just need to add a modules
// field on the vm, or use a wrapped type with modules to find things
// TODO: Add magic number for modules. - key to magic number, do pointer equality
fn lookup_doc(_ctx: &mut VmCore, _args: &[SteelVal]) -> Result<SteelVal> {
    // for value in ctx.thread.global_env.bindings_vec.iter() {
    //     if let
    // }

    todo!()
}
