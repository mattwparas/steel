use super::{builtin::BuiltInModule, engine::Engine, register_fn::RegisterFn};
use crate::rvals::{Result, SteelVal};
use crate::values::structs::{struct_ref, struct_to_list, struct_to_vector};
use crate::{
    primitives::{
        ContractOperations, ControlOperations, FsFunctions, HashMapOperations, HashSetOperations,
        IoFunctions, MetaOperations, NumOperations, PortOperations, StreamOperations,
        StringOperations, SymbolOperations, TransducerOperations, VectorOperations,
    },
    values::structs::is_custom_struct,
};

use itertools::Itertools;
use once_cell::unsync::Lazy;

macro_rules! ensure_tonicity_two {
    ($check_fn:expr) => {{
        |args: &[SteelVal]| -> Result<SteelVal> {

            if args.len() == 0 {
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
        if let Some(first) = args.first() {
            if let SteelVal::Void = first {
                return Ok(SteelVal::BoolV(true));
            }
        }
        Ok(SteelVal::BoolV(false))
    })
}

macro_rules! gen_pred {
    ($variant:ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some(first) = args.first() {
                if let SteelVal::$variant(..) = first {
                    return Ok(SteelVal::BoolV(true));
                }
            }
            Ok(SteelVal::BoolV(false))
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
}

pub fn register_builtin_modules(engine: &mut Engine) {
    engine.register_fn("##__module-get", BuiltInModule::get);
    engine.register_value("error!", ControlOperations::error());

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
        .register_module(SYNTAX_MODULE.with(|x| x.clone()));
}

pub static ALL_MODULES: &str = r#"
    (require-builtin "hash")
    (require-builtin "sets")
    (require-builtin "lists")
    (require-builtin "strings")
    (require-builtin "symbols")
    (require-builtin "vectors")
    (require-builtin "streams")
    (require-builtin "contracts")
    (require-builtin "identity")
    (require-builtin "numbers")
    (require-builtin "equality")
    (require-builtin "ord")
    (require-builtin "transducers")
    (require-builtin "io")
    (require-builtin "filesystem")
    (require-builtin "ports")
    (require-builtin "meta")
    (require-builtin "json")
    (require-builtin "constants")
    (require-builtin "syntax")
"#;

// static MAP_MODULE: Lazy<BuiltInModule> = Lazy::new(hashmap);
// static SET_MODULE: Lazy<BuiltInModule> = Lazy::new(hashset);

fn list_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("lists".to_string());
    module
        .register_value(LIST, crate::primitives::lists::LIST)
        .register_value(CONS, crate::primitives::lists::CONS)
        .register_value(RANGE, crate::primitives::lists::RANGE)
        .register_value(LENGTH, crate::primitives::lists::LENGTH)
        .register_value("empty?", crate::primitives::lists::IS_EMPTY)
        .register_value(CAR, crate::primitives::lists::CAR)
        .register_value(FIRST, crate::primitives::lists::CAR)
        .register_value(CDR, crate::primitives::lists::CDR)
        .register_value(REST, crate::primitives::lists::REST)
        .register_value(APPEND, crate::primitives::lists::APPEND)
        .register_value(REVERSE, crate::primitives::lists::REVERSE)
        .register_value("list-ref", crate::primitives::lists::LIST_REF)
        .register_value("try-list-ref", crate::primitives::lists::TRY_LIST_REF)
        .register_value("list->string", crate::primitives::lists::LIST_TO_STRING)
        .register_value("push-back", crate::primitives::lists::PUSH_BACK)
        .register_value("pair?", crate::primitives::lists::PAIR)
        // .register_value("test-push-back", crate::primitives::alternative_list::PU)
        .register_value("test-map", crate::primitives::lists::TEST_MAP)
        // TODO move this to somewhere better than here
        .register_value("apply", crate::primitives::lists::TEST_APPLY)
        // .register_value("transduce", crate::steel_vm::transducers::TRANSDUCE)
        // .register_value("execute", crate::steel_vm::transducers::EXECUTE)
        .register_value("transduce", crate::steel_vm::transducers::TRANSDUCE);
    module
}

fn vector_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("vectors".to_string());
    module
        .register_value("mutable-vector", VectorOperations::mut_vec_construct())
        .register_value("vector-push!", VectorOperations::mut_vec_push())
        .register_value("mut-vec-len", VectorOperations::mut_vec_length())
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

fn string_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("strings".to_string());
    module
        .register_value("string-append", StringOperations::string_append())
        .register_value("string->list", StringOperations::string_to_list())
        .register_value("string-upcase", StringOperations::string_to_upper())
        .register_value("string-lowercase", StringOperations::string_to_lower())
        .register_value("string-length", StringOperations::string_length())
        .register_value("trim", StringOperations::trim())
        .register_value("trim-start", StringOperations::trim_start())
        .register_value("trim-end", StringOperations::trim_end())
        .register_value("split-whitespace", StringOperations::split_whitespace())
        .register_value("string->int", StringOperations::string_to_int())
        .register_value("int->string", StringOperations::int_to_string())
        .register_value("string->symbol", StringOperations::string_to_symbol())
        .register_value("starts-with?", StringOperations::starts_with())
        .register_value("ends-with?", StringOperations::ends_with());
    module
}

// TODO: Make this lazy
pub(crate) fn hashmap_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("hash".to_string());
    module
        .register_value("hash", HashMapOperations::hm_construct())
        .register_value("hash-insert", HashMapOperations::hm_insert())
        .register_value("hash-get", HashMapOperations::hm_get())
        .register_value("hash-try-get", HashMapOperations::hm_try_get())
        .register_value("hash-length", HashMapOperations::hm_length())
        .register_value("hash-contains?", HashMapOperations::hm_contains())
        .register_value("hash-keys->list", HashMapOperations::keys_to_list())
        .register_value("hash-keys->vector", HashMapOperations::keys_to_vector())
        .register_value("hash-values->list", HashMapOperations::values_to_list())
        .register_value("hash-values->vector", HashMapOperations::values_to_vector())
        .register_value("hash-clear", HashMapOperations::clear())
        .register_value("hash-empty?", HashMapOperations::hm_empty());
    module
}

pub(crate) fn hashset_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("sets".to_string());
    module
        .register_value("hashset", HashSetOperations::hs_construct())
        .register_value("hashset-length", HashSetOperations::hs_length())
        .register_value("hashset-contains?", HashSetOperations::hs_contains())
        .register_value("hashset-insert", HashSetOperations::hs_insert())
        .register_value("hashset->list", HashSetOperations::keys_to_list())
        .register_value("hashset->vector", HashSetOperations::keys_to_vector())
        .register_value("hashset-clear", HashSetOperations::clear())
        .register_value("hashset-subset?", HashSetOperations::is_subset())
        .register_value("list->hashset", HashSetOperations::list_to_hashset());
    module
}

fn identity_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("identity".to_string());
    module
        .register_value("int?", gen_pred!(IntV))
        .register_value("float?", gen_pred!(NumV))
        .register_value("number?", gen_pred!(NumV, IntV))
        .register_value("string?", gen_pred!(StringV))
        .register_value("symbol?", gen_pred!(SymbolV))
        .register_value("vector?", gen_pred!(VectorV))
        .register_value("struct?", gen_pred!(StructV))
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
    let mut module = BuiltInModule::new("streams".to_string());
    module
        .register_value("stream-cons", StreamOperations::stream_cons())
        .register_value("empty-stream", StreamOperations::empty_stream())
        .register_value("stream-empty?", StreamOperations::stream_empty_huh())
        .register_value("stream-car", StreamOperations::stream_car())
        .register_value("stream-cdr'", StreamOperations::stream_cdr());
    module
}

fn contract_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("contracts".to_string());
    module
        .register_value("bind/c", ContractOperations::bind_contract_to_function())
        .register_value("make-flat/c", ContractOperations::make_flat_contract())
        .register_value(
            "make-dependent-function/c",
            ContractOperations::make_dependent_contract(),
        )
        .register_value(
            "make-function/c",
            ContractOperations::make_function_contract(),
        )
        .register_value("make/c", ContractOperations::make_c());

    module
}

fn number_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("numbers".to_string());
    module
        .register_value("+", NumOperations::adder())
        .register_value("f+", NumOperations::float_add())
        .register_value("*", NumOperations::multiply())
        .register_value("/", NumOperations::divide())
        .register_value("-", NumOperations::subtract())
        .register_value("even?", NumOperations::even())
        .register_value("odd?", NumOperations::odd())
        .register_value("arithmetic-shift", NumOperations::arithmetic_shift());
    module
}

fn equality_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("equality".to_string());
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
    let mut module = BuiltInModule::new("ord".to_string());
    module
        .register_value(">", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a > b)))
        .register_value(">=", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a >= b)))
        .register_value("<", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a < b)))
        .register_value("<=", SteelVal::FuncV(ensure_tonicity_two!(|a, b| a <= b)));
    module
}

pub fn transducer_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("transducers".to_string());
    module
        .register_value("compose", TransducerOperations::compose())
        .register_value("mapping", TransducerOperations::map())
        .register_value("flattening", TransducerOperations::flatten())
        .register_value("flat-mapping", TransducerOperations::flat_map())
        .register_value("filtering", TransducerOperations::filter())
        .register_value("taking", TransducerOperations::take())
        .register_value("dropping", TransducerOperations::dropping())
        .register_value("extending", TransducerOperations::extending())
        .register_value("enumerating", TransducerOperations::enumerating())
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
    let mut module = BuiltInModule::new("symbols".to_string());
    module
        .register_value("concat-symbols", SymbolOperations::concat_symbols())
        .register_value("symbol->string", SymbolOperations::symbol_to_string());
    module
}

fn io_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("io".to_string());
    module
        .register_value("display", IoFunctions::display())
        .register_value("display-color", IoFunctions::display_color())
        .register_value("newline", IoFunctions::newline())
        .register_value("read-to-string", IoFunctions::read_to_string());
    module
}

fn constants_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("constants".to_string());
    module.register_value("void", SteelVal::Void);
    module
}

fn fs_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("filesystem".to_string());
    module
        .register_value("is-dir?", FsFunctions::is_dir())
        .register_value("is-file?", FsFunctions::is_file())
        .register_value("read-dir", FsFunctions::read_dir())
        .register_value("path-exists?", FsFunctions::path_exists())
        .register_value("file-name", FsFunctions::file_name())
        .register_value("current-directory", FsFunctions::current_dir());
    module
}

fn port_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("ports".to_string());
    module
        .register_value("open-input-file", PortOperations::open_input_file())
        .register_value("open-output-file", PortOperations::open_output_file())
        .register_value("write-line!", PortOperations::write_line())
        .register_value("read-port-to-string", PortOperations::read_port_to_string())
        .register_value("read-line-from-port", PortOperations::read_line_to_string());
    module
}

fn meta_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("meta".to_string());
    module
        .register_value("assert!", MetaOperations::assert_truthy())
        .register_value("box", MetaOperations::new_box())
        .register_value("unbox", MetaOperations::unbox())
        .register_value("set-box!", MetaOperations::set_box())
        .register_value("active-object-count", MetaOperations::active_objects())
        .register_value("inspect-bytecode", MetaOperations::inspect_bytecode())
        .register_value("memory-address", MetaOperations::memory_address())
        .register_value("async-exec", MetaOperations::exec_async())
        .register_value("poll!", MetaOperations::poll_value())
        .register_value("block-on", MetaOperations::block_on())
        .register_value("join!", MetaOperations::join_futures())
        .register_value("struct-ref", struct_ref())
        .register_value("struct->list", struct_to_list())
        .register_value("struct->vector", struct_to_vector())
        .register_value("expand!", SteelVal::FuncV(super::meta::expand_macros))
        .register_value("read!", SteelVal::FuncV(super::meta::read))
        .register_value("eval!", SteelVal::FuncV(super::meta::eval))
        // TODO: @Matt -> implement the traits for modules as well
        .register_fn("Engine::new", super::meta::EngineWrapper::new)
        // .register_fn("run!", super::meta::EngineWrapper::call)
        // .register_fn("get-value", super::meta::EngineWrapper::get_value)
        .register_value(
            "___magic_struct_symbol___",
            crate::rvals::MAGIC_STRUCT_SYMBOL.with(|x| x.clone()),
        )
        .register_value("custom-struct?", is_custom_struct());
    module
}

fn json_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("json".to_string());
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
    let mut module = BuiltInModule::new("syntax".to_string());
    module
        .register_fn("syntax->datum", crate::rvals::Syntax::syntax_datum)
        .register_fn("syntax-loc", crate::rvals::Syntax::syntax_loc)
        .register_fn("syntax/loc", crate::rvals::Syntax::new);
    module
}
