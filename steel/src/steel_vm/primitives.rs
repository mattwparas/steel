use super::engine::Engine;
use crate::primitives::{
    ContractOperations, ControlOperations, FsFunctions, HashMapOperations, HashSetOperations,
    IoFunctions, ListOperations, MetaOperations, NumOperations, PortOperations, StreamOperations,
    StringOperations, SymbolOperations, TransducerOperations, VectorOperations,
};
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::values::structs::{struct_ref, struct_to_list, struct_to_vector};

#[macro_use]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[SteelVal]| -> Result<SteelVal> {
            let mut args_iter = args.iter();
            let first = args_iter.next().ok_or(SteelErr::new(
                ErrorKind::ArityMismatch,
                "expected at least one argument".to_string(),
            ))?;
            fn f<'a>(prev: &SteelVal, mut xs: impl Iterator<Item = &'a SteelVal>) -> bool {
                match xs.next() {
                    Some(x) => $check_fn(prev, x) && f(x, xs),
                    None => true,
                }
            }
            let res = f(&first, args_iter);
            Ok(SteelVal::BoolV(res))
        }
    }};
}

// #[macro_use]
// macro_rules! ensure_tonicity_pointer_equality {
//     ($check_fn:expr) => {{
//         |args: &[SteelVal]| -> Result<SteelVal> {
//             // let args_iter: Vec<Rc<SteelVal>> = args.into_iter();
//             let mut args_iter = args.into_iter();
//             let first = args_iter.next().ok_or(SteelErr::ArityMismatch(
//                 "expected at least one argument".to_string(),
//                 None,
//             ))?;
//             fn f<'a>(prev: &SteelVal, mut xs: impl Iterator<Item = &'a SteelVal>) -> bool {
//                 match xs.next() {
//                     Some(x) => $check_fn(prev, x) && f(x, xs),
//                     None => true,
//                 }
//             };
//             let res = f(&first, args_iter);
//             Ok(SteelVal::BoolV(res))
//         }
//     }};
// }

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

#[macro_use]
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

    ($variant1:ident, $variant2:ident, $variant3:ident, $variant4:ident, $variant5:ident) => {{
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if let Some(first) = args.first() {
                match first {
                    SteelVal::$variant1(..)
                    | SteelVal::$variant2(..)
                    | SteelVal::$variant3(..)
                    | SteelVal::$variant4(..)
                    | SteelVal::$variant5(..) => {
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
const LIST_TO_VECTOR: &str = "list->vector";
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

#[inline(always)]
pub(crate) fn register_list_functions(engine: &mut Engine) {
    engine
        .register_value(LIST, ListOperations::list())
        .register_value(CAR, ListOperations::car())
        .register_value(CDR, ListOperations::cdr())
        .register_value(FIRST, ListOperations::car())
        .register_value(REST, ListOperations::cdr())
        .register_value(CONS, ListOperations::cons())
        .register_value(APPEND, ListOperations::append())
        .register_value(PUSH_BACK, ListOperations::push_back())
        .register_value(RANGE, ListOperations::range())
        .register_value(LENGTH, ListOperations::list_length())
        .register_value(REVERSE, ListOperations::reverse())
        .register_value(LIST_TO_VECTOR, ListOperations::list_to_vec())
        .register_value(LIST_TO_STRING, ListOperations::list_to_string());
}

#[inline(always)]
pub(crate) fn register_vector_functions(engine: &mut Engine) {
    engine
        .register_value("vector", VectorOperations::vec_construct())
        .register_value("push-front", VectorOperations::vec_cons())
        .register_value("pop-front", VectorOperations::vec_car())
        .register_value("vec-rest", VectorOperations::vec_cdr())
        .register_value("null?", VectorOperations::list_vec_null())
        .register_value("push", VectorOperations::vec_push())
        .register_value("range-vec", VectorOperations::vec_range())
        .register_value("vec-append", VectorOperations::vec_append())
        .register_value("vector-ref", VectorOperations::vec_ref());
}

#[inline(always)]
pub(crate) fn register_string_functions(engine: &mut Engine) {
    engine
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
}

#[inline(always)]
pub(crate) fn register_hashmap_functions(engine: &mut Engine) {
    engine
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
}

#[inline(always)]
pub(crate) fn register_hashset_functions(engine: &mut Engine) {
    engine
        .register_value("hashset", HashSetOperations::hs_construct())
        .register_value("hashset-contains?", HashSetOperations::hs_contains())
        .register_value("hashset-insert", HashSetOperations::hs_insert())
        .register_value("hashset->list", HashSetOperations::keys_to_list())
        .register_value("hashset->vector", HashSetOperations::keys_to_vector())
        .register_value("hashset-clear", HashSetOperations::clear())
        .register_value("list->hashset", HashSetOperations::list_to_hashset());
}

#[inline(always)]
pub(crate) fn register_identity_predicates(engine: &mut Engine) {
    engine
        .register_value("int?", gen_pred!(IntV))
        .register_value("float?", gen_pred!(NumV))
        .register_value("number?", gen_pred!(NumV, IntV))
        .register_value("string?", gen_pred!(StringV))
        .register_value("symbol?", gen_pred!(SymbolV))
        .register_value("vector?", gen_pred!(VectorV))
        .register_value("struct?", gen_pred!(StructV))
        .register_value("list?", gen_pred!(Pair))
        .register_value("pair?", gen_pred!(Pair))
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
                ContinuationFunction
            ),
        )
        .register_value(
            "procedure?",
            gen_pred!(
                Closure,
                FuncV,
                ContractedFunction,
                BoxedFunction,
                ContinuationFunction
            ),
        )
        .register_value(
            "atom?",
            gen_pred!(NumV, IntV, StringV, SymbolV, BoolV, CharV),
        );
}

#[inline(always)]
pub(crate) fn register_stream_functions(engine: &mut Engine) {
    engine
        .register_value("stream-cons", StreamOperations::stream_cons())
        .register_value("empty-stream", StreamOperations::empty_stream())
        .register_value("stream-empty?", StreamOperations::stream_empty_huh())
        .register_value("stream-car", StreamOperations::stream_car())
        .register_value("stream-cdr'", StreamOperations::stream_cdr());
}

#[inline(always)]
pub(crate) fn register_contract_functions(engine: &mut Engine) {
    engine
        .register_value("bind/c", ContractOperations::bind_contract_to_function())
        .register_value("make-flat/c", ContractOperations::make_flat_contract())
        .register_value(
            "make-function/c",
            ContractOperations::make_function_contract(),
        )
        .register_value("make/c", ContractOperations::make_c());
}

#[inline(always)]
pub(crate) fn register_number_functions(engine: &mut Engine) {
    engine
        .register_value("+", NumOperations::adder())
        .register_value("f+", NumOperations::float_add())
        .register_value("*", NumOperations::multiply())
        .register_value("/", NumOperations::divide())
        .register_value("-", NumOperations::subtract())
        .register_value("even?", NumOperations::even())
        .register_value("odd?", NumOperations::odd())
        .register_value("arithmetic-shift", NumOperations::arithmetic_shift());
}

#[inline(always)]
pub(crate) fn register_equality_functions(engine: &mut Engine) {
    engine
        .register_value("equal?", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b)))
        .register_value("=", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b)));
}

#[inline(always)]
pub(crate) fn register_ord_functions(engine: &mut Engine) {
    engine
        .register_value(">", SteelVal::FuncV(ensure_tonicity!(|a, b| a > b)))
        .register_value(">=", SteelVal::FuncV(ensure_tonicity!(|a, b| a >= b)))
        .register_value("<", SteelVal::FuncV(ensure_tonicity!(|a, b| a < b)))
        .register_value("<=", SteelVal::FuncV(ensure_tonicity!(|a, b| a <= b)));
}

#[inline(always)]
pub(crate) fn register_transducer_functions(engine: &mut Engine) {
    engine
        .register_value("compose", TransducerOperations::compose())
        .register_value("mapping", TransducerOperations::map())
        .register_value("filtering", TransducerOperations::filter())
        .register_value("taking", TransducerOperations::take())
        .register_value("dropping", TransducerOperations::dropping());
}

#[inline(always)]
pub(crate) fn register_symbol_functions(engine: &mut Engine) {
    engine
        .register_value("concat-symbols", SymbolOperations::concat_symbols())
        .register_value("symbol->string", SymbolOperations::symbol_to_string());
}

#[inline(always)]
pub(crate) fn register_io_functions(engine: &mut Engine) {
    engine
        .register_value("display", IoFunctions::display())
        .register_value("display-color", IoFunctions::display_color())
        .register_value("newline", IoFunctions::newline())
        .register_value("read-to-string", IoFunctions::read_to_string());
}

#[inline(always)]
pub(crate) fn register_constants(engine: &mut Engine) {
    engine.register_value("void", SteelVal::Void);
}

#[inline(always)]
pub(crate) fn register_fs_functions(engine: &mut Engine) {
    engine
        .register_value("is-dir?", FsFunctions::is_dir())
        .register_value("is-file?", FsFunctions::is_file())
        .register_value("read-dir", FsFunctions::read_dir())
        .register_value("path-exists?", FsFunctions::path_exists())
        .register_value("file-name", FsFunctions::file_name())
        .register_value("current-directory", FsFunctions::current_dir());
}

#[inline(always)]
pub(crate) fn register_port_functions(engine: &mut Engine) {
    engine
        .register_value("open-input-file", PortOperations::open_input_file())
        .register_value("read-port-to-string", PortOperations::read_port_to_string())
        .register_value("read-line-from-port", PortOperations::read_line_to_string());
}

#[inline(always)]
pub(crate) fn register_meta_functions(engine: &mut Engine) {
    engine
        .register_value("assert!", MetaOperations::assert_truthy())
        .register_value("box", MetaOperations::new_box())
        .register_value("unbox", MetaOperations::unbox())
        .register_value("set-box!", MetaOperations::set_box())
        .register_value("active-object-count", MetaOperations::active_objects())
        .register_value("inspect-bytecode", MetaOperations::inspect_bytecode())
        .register_value("memory-address", MetaOperations::memory_address())
        .register_value("async-exec", MetaOperations::exec_async())
        .register_value("poll!", MetaOperations::poll_value())
        .register_value("join!", MetaOperations::join_futures())
        .register_value("struct-ref", struct_ref())
        .register_value("struct->list", struct_to_list())
        .register_value("struct->vector", struct_to_vector());
}

#[inline(always)]
pub(crate) fn register_json_functions(engine: &mut Engine) {
    engine
        .register_value(
            "string->jsexpr",
            crate::values::json_vals::string_to_jsexpr(),
        )
        .register_value(
            "value->jsexpr-string",
            crate::values::json_vals::serialize_val_to_string(),
        );
}

#[inline(always)]
pub(crate) fn embed_primitives(engine: &mut Engine) {
    register_constants(engine);

    register_equality_functions(engine);
    register_ord_functions(engine);

    register_number_functions(engine);
    register_list_functions(engine);
    register_vector_functions(engine);
    register_string_functions(engine);
    register_hashmap_functions(engine);
    register_hashset_functions(engine);
    register_identity_predicates(engine);
    register_stream_functions(engine);
    register_contract_functions(engine);
    register_transducer_functions(engine);
    register_symbol_functions(engine);

    register_io_functions(engine);
    register_fs_functions(engine);
    register_port_functions(engine);

    register_meta_functions(engine);
    register_json_functions(engine);

    engine.register_value("error!", ControlOperations::error());
}

#[inline(always)]
pub(crate) fn embed_primitives_without_io(engine: &mut Engine) {
    register_constants(engine);

    register_equality_functions(engine);
    register_ord_functions(engine);

    register_number_functions(engine);
    register_list_functions(engine);
    register_vector_functions(engine);
    register_string_functions(engine);
    register_hashmap_functions(engine);
    register_hashset_functions(engine);
    register_identity_predicates(engine);
    register_stream_functions(engine);
    register_contract_functions(engine);
    register_transducer_functions(engine);
    register_symbol_functions(engine);

    register_meta_functions(engine);
    register_json_functions(engine);

    engine.register_value("error!", ControlOperations::error());
}
