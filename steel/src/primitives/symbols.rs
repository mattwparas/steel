use crate::gc::Gc;
use crate::rvals::{Result, SteelVal};
use crate::stop;

pub struct SymbolOperations {}
impl SymbolOperations {
    pub fn concat_symbols() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            let mut new_symbol = String::new();

            for arg in args {
                if let SteelVal::SymbolV(quoted_value) = arg {
                    new_symbol.push_str(quoted_value.as_ref());
                } else {
                    let error_message =
                        format!("concat-symbol expected only symbols, found {}", arg);
                    stop!(TypeMismatch => error_message);
                }
            }

            return Ok(SteelVal::SymbolV(new_symbol.into()));
        })
    }

    pub fn symbol_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::SymbolV(quoted_value) = &args[0] {
                    return Ok(SteelVal::StringV(std::rc::Rc::clone(&quoted_value)));
                } else {
                    let error_message =
                        format!("symbol->string expected a symbol, found {}", &args[0]);
                    stop!(TypeMismatch => error_message)
                }
            } else {
                stop!(ArityMismatch => "symbol->string expects only one argument")
            }
        })
    }
}

#[cfg(test)]
mod symbol_tests {
    use super::*;
    use crate::throw;

    use crate::rvals::SteelVal::*;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        func.func_or_else(throw!(BadSyntax => "hash tests"))
            .unwrap()(&args)
    }

    #[test]
    fn concat_symbols_normal() {
        let args = vec![
            SymbolV("foo".into()),
            SymbolV("bar".into()),
            SymbolV("baz".into()),
        ];
        let result = apply_function(SymbolOperations::concat_symbols(), args);
        let expected = SymbolV("foobarbaz".into());
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn symbol_to_string_normal() {
        let args = vec![SymbolV("foo".into())];
        let result = apply_function(SymbolOperations::symbol_to_string(), args);
        let expected = StringV("foo".into());
        assert_eq!(result.unwrap(), expected);
    }
}
