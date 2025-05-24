use crate::rvals::{PrimitiveAsRef, RestArgsIter, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::stop;

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
