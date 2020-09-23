use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;

pub struct SymbolOperations {}
impl SymbolOperations {
    pub fn concat_symbols() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut new_symbol = String::new();

            for arg in args {
                if let SteelVal::SymbolV(quoted_value) = arg.as_ref() {
                    new_symbol.push_str(quoted_value);
                } else {
                    let error_message =
                        format!("concat-symbol expected only symbols, found {}", arg);
                    stop!(TypeMismatch => error_message);
                }
            }

            return Ok(Gc::new(SteelVal::SymbolV(new_symbol)));
        })
    }

    pub fn symbol_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::SymbolV(quoted_value) = args[0].as_ref() {
                    return Ok(Gc::new(SteelVal::StringV(quoted_value.clone())));
                } else {
                    let error_message = format!(
                        "symbol->string expected a symbol, found {}",
                        args[0].as_ref()
                    );
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

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "hash tests"))
            .unwrap()(&args)
    }

    #[test]
    fn concat_symbols_normal() {
        let args = vec![
            SymbolV("foo".to_string()),
            SymbolV("bar".to_string()),
            SymbolV("baz".to_string()),
        ];
        let result = apply_function(SymbolOperations::concat_symbols(), args);
        let expected = Gc::new(SymbolV("foobarbaz".to_string()));
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn symbol_to_string_normal() {
        let args = vec![SymbolV("foo".to_string())];
        let result = apply_function(SymbolOperations::symbol_to_string(), args);
        let expected = Gc::new(StringV("foo".to_string()));
        assert_eq!(result.unwrap(), expected);
    }
}
