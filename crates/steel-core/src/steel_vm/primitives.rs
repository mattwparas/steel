use std::cmp::Ordering;

use super::{
    builtin::BuiltInModule,
    cache::WeakMemoizationTable,
    engine::Engine,
    register_fn::RegisterFn,
    vm::{get_test_mode, list_modules, set_test_mode, VmCore},
};
use crate::{
    parser::span::Span,
    primitives::{
        colors::string_coloring_module,
        contracts,
        hashmaps::hashmap_module,
        hashmaps::{HM_CONSTRUCT, HM_GET, HM_INSERT},
        hashsets::hashset_module,
        lists::{list_module, UnRecoverableResult},
        nums::quotient,
        port_module,
        process::process_module,
        random::random_module,
        string_module,
        time::time_module,
        ControlOperations, FsFunctions, IoFunctions, MetaOperations, NumOperations,
        StreamOperations, SymbolOperations, VectorOperations,
    },
    rerrs::ErrorKind,
    rvals::FromSteelVal,
    steel_vm::{
        builtin::{get_function_name, Arity},
        vm::threads::threading_module,
    },
    values::{
        closed::HeapRef,
        functions::{attach_contract_struct, get_contract},
        structs::{build_type_id_module, make_struct_type},
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
    pub static TYPE_ID_MODULE: BuiltInModule = build_type_id_module();
    pub static OPTION_MODULE: BuiltInModule = build_option_structs();
    pub static PRELUDE_MODULE: BuiltInModule = prelude();
    pub static TIME_MODULE: BuiltInModule = time_module();
    pub static THREADING_MODULE: BuiltInModule = threading_module();

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
        .with_module(TYPE_ID_MODULE.with(|x| x.clone()))
        .with_module(TIME_MODULE.with(|x| x.clone()))
        .with_module(THREADING_MODULE.with(|x| x.clone()))
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
    #[cfg(feature = "markdown")]
    println!("{}", termimad::text(&text));

    #[cfg(not(feature = "markdown"))]
    println!("{}", text);
}

pub fn register_builtin_modules(engine: &mut Engine) {
    engine.register_fn("##__module-get", BuiltInModule::get);
    engine.register_fn("%module-get%", BuiltInModule::get);
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
        .register_module(TYPE_ID_MODULE.with(|x| x.clone()))
        .register_module(PRELUDE_MODULE.with(|x| x.clone()))
        .register_module(TIME_MODULE.with(|x| x.clone()))
        .register_module(STRING_COLORS_MODULE.with(|x| x.clone()))
        .register_module(RANDOM_MODULE.with(|x| x.clone()))
        .register_module(THREADING_MODULE.with(|x| x.clone()));

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
    (require-builtin steel/core/types)
    (require-builtin steel/threads)
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

#[steel_derive::function(name = "int?", constant = true)]
fn intp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::IntV(_))
}

#[steel_derive::function(name = "integer?", constant = true)]
fn integerp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::IntV(_))
}

#[steel_derive::function(name = "float?", constant = true)]
fn floatp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::NumV(_))
}

#[steel_derive::function(name = "number?", constant = true)]
fn numberp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::NumV(_) | SteelVal::IntV(_))
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

#[steel_derive::function(name = "function?", constant = true)]
fn functionp(value: &SteelVal) -> bool {
    matches!(
        value,
        SteelVal::Closure(_)
            | SteelVal::FuncV(_)
            | SteelVal::ContractedFunction(_)
            | SteelVal::BoxedFunction(_)
            | SteelVal::ContinuationFunction(_)
            | SteelVal::MutFunc(_)
            | SteelVal::BuiltIn(_)
    )
}

#[steel_derive::function(name = "procedure?", constant = true)]
fn procedurep(value: &SteelVal) -> bool {
    matches!(
        value,
        SteelVal::Closure(_)
            | SteelVal::FuncV(_)
            | SteelVal::ContractedFunction(_)
            | SteelVal::BoxedFunction(_)
            | SteelVal::ContinuationFunction(_)
            | SteelVal::MutFunc(_)
            | SteelVal::BuiltIn(_)
    )
}
fn identity_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/identity");
    module
        // .register_value("int?", gen_pred!(IntV))
        .register_native_fn_definition(INTEGERP_DEFINITION)
        .register_native_fn_definition(INTP_DEFINITION)
        .register_native_fn_definition(FLOATP_DEFINITION)
        .register_native_fn_definition(NUMBERP_DEFINITION)
        .register_native_fn_definition(NUMBERP_DEFINITION)
        .register_native_fn_definition(STRINGP_DEFINITION)
        .register_native_fn_definition(LISTP_DEFINITION)
        .register_native_fn_definition(VECTORP_DEFINITION)
        .register_native_fn_definition(SYMBOLP_DEFINITION)
        .register_native_fn_definition(HASHP_DEFINITION)
        .register_native_fn_definition(CONTINUATIONP_DEFINITION)
        .register_native_fn_definition(BOOLEANP_DEFINITION)
        .register_native_fn_definition(BOOLP_DEFINITION)
        .register_native_fn_definition(VOIDP_DEFINITION)
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

fn lookup_function_name(value: SteelVal) -> Option<SteelVal> {
    match value {
        SteelVal::BoxedFunction(f) => f.name().map(|x| x.into_steelval().unwrap()),
        SteelVal::FuncV(f) => get_function_name(f).map(|x| x.name.into_steelval().unwrap()),
        _ => None,
    }
}

// Only works with fixed size arity functions
fn arity(value: SteelVal) -> UnRecoverableResult {
    match value {
        SteelVal::Closure(c) => {
            // Ok(SteelVal::IntV(c.arity() as isize)).into()

            if let Some(SteelVal::CustomStruct(s)) = c.get_contract_information() {
                let guard = s.borrow();
                if guard.name.resolve() == "FunctionContract" {
                    if let SteelVal::ListV(l) = &guard.fields[0] {
                        Ok(SteelVal::IntV(l.len() as isize)).into()
                    } else {
                        steelerr!(TypeMismatch => "Unable to find the arity for the given function")
                            .into()
                    }
                } else if guard.name.resolve() == "FlatContract" {
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
        .register_fn("function-name", lookup_function_name)
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
        .register_fn("current-os!", || std::env::consts::OS);
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
fn _lookup_doc(_ctx: &mut VmCore, _args: &[SteelVal]) -> Result<SteelVal> {
    // for value in ctx.thread.global_env.bindings_vec.iter() {
    //     if let
    // }

    todo!()
}
