use crate::rvals::{PrimitiveAsRef, RestArgsIter, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::stop;
use alloc::format;
use alloc::string::String;

pub fn symbol_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/symbols");
    module
        .register_native_fn_definition(CONCAT_SYMBOLS_DEFINITION)
        .register_native_fn_definition(SYMBOL_TO_STRING_DEFINITION)
        .register_native_fn_definition(SYMBOL_EQUALS_DEFINITION);
    module
}

pub struct SteelSymbol<'a>(&'a SteelString);

impl<'a> PrimitiveAsRef<'a> for SteelSymbol<'a> {
    fn primitive_as_ref(val: &'a SteelVal) -> Result<Self> {
        if let SteelVal::SymbolV(sym) = val {
            Ok(SteelSymbol(sym))
        } else {
            stop!(ConversionError => format!("Cannot convert steel value: {} to steel symbol", val))
        }
    }

    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self> {
        if let SteelVal::SymbolV(sym) = val {
            Some(SteelSymbol(sym))
        } else {
            None
        }
    }
}

/// Compares one or more symbols for pointer‐identity equality.
///
/// (symbol=? sym1 sym2 …) -> bool?
///
/// * `sym1` : symbol? — the first symbol to compare
/// * `sym2` : symbol? — the next symbol to compare, and so on
///
/// Returns `#t` if all provided symbols share the same memory pointer,
/// `#f` otherwise. At least one argument is required.
///
/// # Examples
/// ```scheme
/// > (define a 'foo)
/// > (define b 'foo)
/// > (symbol=? a b)
/// => #t
/// > (symbol=? 'a 'b)
/// => #f
/// > (symbol=? 'x 'x 'x)
/// => #t
/// ```
#[steel_derive::function(name = "symbol=?", constant = true)]
pub fn symbol_equals(mut iter: RestArgsIter<SteelSymbol<'_>>) -> Result<SteelVal> {
    let Some(mut prev) = iter.next().transpose()? else {
        stop!(ArityMismatch => "expected at least one argument");
    };

    for item in iter {
        let item = item?;
        if crate::gc::Shared::ptr_eq(&**item.0, &**prev.0) {
            prev = item;
        } else {
            return Ok(SteelVal::BoolV(false));
        }
    }

    Ok(SteelVal::BoolV(true))
}

/// Concatenates zero or more symbols into a new symbol.
///
/// (concat-symbols sym1 sym2 …) -> symbol?
///
/// * `sym1` : symbol? — the first symbol to append
/// * `sym2` : symbol? — the next symbol to append, and so on
///
/// # Examples
/// ```scheme
/// > (concat-symbols 'he 'llo)
/// => 'hello
/// > (concat-symbols)
/// => '
/// ```
#[steel_derive::native(name = "concat-symbols", arity = "AtLeast(0)")]
fn concat_symbols(args: &[SteelVal]) -> Result<SteelVal> {
    let mut new_symbol = String::new();

    for arg in args {
        if let SteelVal::SymbolV(quoted_value) = arg {
            new_symbol.push_str(quoted_value.as_ref());
        } else {
            let error_message = format!("concat-symbol expected only symbols, found {args:?}");
            stop!(TypeMismatch => error_message);
        }
    }

    Ok(SteelVal::SymbolV(new_symbol.into()))
}

/// Converts a symbol or quoted list into its string representation.
///
/// (symbol->string sym) -> string?
///
/// * `sym` : symbol? | list? — a symbol or quoted list to convert
///
/// # Examples
/// ```scheme
/// > (symbol->string 'foo)
/// "foo"
///
/// > (symbol->string '(a b c))
/// "(a b c)"
///
/// > (symbol->string 123)
/// Error: symbol->string expected a symbol, found 123
/// ```
#[steel_derive::native(name = "symbol->string", arity = "Exact(1)")]
fn symbol_to_string(args: &[SteelVal]) -> Result<SteelVal> {
    match &args[0] {
        SteelVal::SymbolV(quoted_value) => Ok(SteelVal::StringV(quoted_value.clone())),
        SteelVal::ListV(_) => Ok(SteelVal::StringV(
            format!("{:?}", &args[0]).trim_start_matches('\'').into(),
        )),
        _ => {
            let error_message = format!("symbol->string expected a symbol, found {}", &args[0]);
            stop!(TypeMismatch => error_message)
        }
    }
}

#[cfg(test)]
mod symbol_tests {
    use super::*;
    use crate::rvals::SteelVal::*;
    use alloc::vec;

    #[test]
    fn concat_symbols_normal() {
        let args = vec![
            SymbolV("foo".into()),
            SymbolV("bar".into()),
            SymbolV("baz".into()),
        ];
        let result = concat_symbols(&args);
        let expected = SymbolV("foobarbaz".into());
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn symbol_to_string_normal() {
        let args = vec![SymbolV("foo".into())];
        let result = symbol_to_string(&args);
        let expected = StringV("foo".into());
        assert_eq!(result.unwrap(), expected);
    }
}
