//! Signatures of host functions written as data, for the language server.
//!
//! An embedding application tells the language server about its functions with
//! files in `STEEL_LSP_HOME`, which [`Engine::emit_lsp_definitions`] can write:
//!
//! ```scheme
//! (define demo (#%module "host/demo"))
//! (#%module-add demo "host-add" (#%signature (args Int Int) (returns Int)))
//! (#%module-add demo "host-shout" (#%signature (args (or String Symbol)) (returns String)))
//! (#%module-add demo "host-log" (#%signature (args String (rest Any)) (returns Void) (raises BadArgs)))
//! ```
//!
//! The vocabulary is the one the function macros use: `crate::value_type::ValueType`
//! variants, unions written `(or List Pair)`, the type of every argument past
//! the fixed ones as `(rest T)`, and a `crate::value_type::Raises` variant.
//! A host type is `(custom "app::Counter")`, by its full Rust type name.
//! Arity follows from `args`.
//!
//! [`Engine::emit_lsp_definitions`]: crate::steel_vm::engine::Engine::emit_lsp_definitions

use crate::rvals::{Result, SteelVal};
use crate::steel_vm::builtin::{get_function_metadata, BuiltInFunctionType, BuiltInModule};
use crate::value_type::{ArgType, ArgTypes, CustomTypeName, Raises, ValueType};
use crate::values::functions::{BoxedDynFunction, HostSignature};
use crate::SteelErr;
use alloc::sync::Arc;

/// Defines `#%signature`, which quotes its clauses for `#%signature-from-spec`.
const SIGNATURE_MACRO: &str = "(define-syntax #%signature
  (syntax-rules ()
    [(_ clause ...) (#%signature-from-spec (quote (clause ...)))]))";

/// Makes `#%signature` available to programs run on `engine`.
pub fn register(engine: &mut crate::steel_vm::engine::Engine) -> Result<()> {
    use crate::steel_vm::register_fn::RegisterFn;
    engine.register_fn("#%signature-from-spec", signature_value);
    engine.compile_and_run_raw_program(SIGNATURE_MACRO)?;
    Ok(())
}

/// A stand-in for a function the language server only knows the signature of.
/// Calling it raises.
fn signature_value(clauses: SteelVal) -> Result<SteelVal> {
    let signature = parse(&clauses)?;
    let fixed = signature.arg_types.fixed.len() as u32;
    let arity = signature.arg_types.rest.is_none().then_some(fixed);
    let function = Arc::new(|_: &[SteelVal]| -> Result<SteelVal> {
        stop!(Generic => "a function known only by its signature cannot be called")
    });
    let mut boxed = BoxedDynFunction::new(function, None, arity);
    boxed.signature = Some(signature);
    Ok(SteelVal::BoxedFunction(crate::gc::Gc::new(boxed)))
}

/// Parses the clauses of a `#%signature`.
pub fn parse(clauses: &SteelVal) -> Result<HostSignature> {
    let mut signature = HostSignature {
        arg_types: ArgTypes {
            fixed: &[],
            rest: None,
        },
        return_type: ValueType::Any,
        raises: Raises::Maybe,
    };
    for clause in list(clauses, "a #%signature")? {
        let items = list(&clause, "a #%signature clause")?;
        let (head, rest) = items
            .split_first()
            .ok_or_else(|| error("an empty #%signature clause"))?;
        match (symbol(head)?, rest) {
            ("args", types) => {
                let mut fixed = Vec::new();
                for (i, ty) in types.iter().enumerate() {
                    match rest_clause(ty)? {
                        Some(rest) if i == types.len() - 1 => {
                            signature.arg_types.rest = Some(arg_type(rest)?)
                        }
                        Some(_) => stop!(Generic => "`(rest T)` has to be the last of `args`"),
                        None => fixed.push(arg_type(ty)?),
                    }
                }
                signature.arg_types.fixed = Box::leak(fixed.into_boxed_slice());
            }
            ("returns", [ty]) => signature.return_type = value_type(ty)?,
            ("raises", [raises]) => {
                signature.raises = match symbol(raises)? {
                    "Never" => Raises::Never,
                    "BadArgs" => Raises::BadArgs,
                    "Maybe" => Raises::Maybe,
                    other => {
                        stop!(Generic => "unknown `raises` {}: expected Never, BadArgs or Maybe", other)
                    }
                }
            }
            (other, _) => {
                stop!(Generic => "unknown #%signature clause `{}`: expected args, returns or raises", other)
            }
        }
    }
    Ok(signature)
}

/// `signature` as the clauses of a `#%signature`.
pub fn render(signature: &HostSignature) -> String {
    let arg = |ty: &ArgType| match ty.0 {
        [single] => render_type(single),
        members => {
            let members: Vec<String> = members.iter().map(render_type).collect();
            format!("(or {})", members.join(" "))
        }
    };
    let mut args: Vec<String> = signature.arg_types.fixed.iter().map(arg).collect();
    if let Some(rest) = &signature.arg_types.rest {
        args.push(format!("(rest {})", arg(rest)));
    }
    let mut out = format!(
        "(#%signature (args{}{})",
        if args.is_empty() { "" } else { " " },
        args.join(" ")
    );
    if signature.return_type != ValueType::Any {
        out.push_str(&format!(
            " (returns {})",
            render_type(&signature.return_type)
        ));
    }
    if signature.raises != Raises::Maybe {
        out.push_str(&format!(" (raises {:?})", signature.raises));
    }
    out.push(')');
    out
}

/// `Int`, or `(custom "app::Counter")` for a host type.
fn render_type(ty: &ValueType) -> String {
    match ty {
        ValueType::Custom(name) => format!("(custom {:?})", name.name()),
        other => format!("{other:?}"),
    }
}

/// The signature a function value declares: one `register_fn` read off its Rust
/// types, or a native function's annotations.
pub fn declared(value: &SteelVal) -> Option<HostSignature> {
    let metadata = get_function_metadata(match value {
        SteelVal::BoxedFunction(f) => return f.signature,
        SteelVal::FuncV(f) => BuiltInFunctionType::Reference(*f),
        SteelVal::MutFunc(f) => BuiltInFunctionType::Mutable(*f),
        SteelVal::BuiltIn(f) => BuiltInFunctionType::Context(*f),
        _ => return None,
    })?;
    Some(HostSignature {
        arg_types: metadata.arg_types?,
        return_type: metadata.return_type,
        raises: metadata.raises,
    })
}

/// The signature of the function `module` binds to `name`, as programs call
/// it. A context function's first argument is the context, which its wrapper
/// passes, so callers supply the rest.
pub fn declared_in(module: &BuiltInModule, name: &str) -> Option<HostSignature> {
    let Some(function) = module.context_function(name) else {
        return declared(&module.try_get_ref(name)?);
    };
    let signature = declared(&function)?;
    let (_, fixed) = signature.arg_types.fixed.split_first()?;
    Some(HostSignature {
        arg_types: ArgTypes {
            fixed,
            rest: signature.arg_types.rest,
        },
        ..signature
    })
}

fn error(what: &str) -> SteelErr {
    SteelErr::new(
        crate::rerrs::ErrorKind::Generic,
        format!("malformed {what}"),
    )
}

fn list(value: &SteelVal, what: &str) -> Result<Vec<SteelVal>> {
    match value {
        SteelVal::ListV(items) => Ok(items.iter().cloned().collect()),
        _ => Err(error(what)),
    }
}

fn symbol(value: &SteelVal) -> Result<&str> {
    match value {
        SteelVal::SymbolV(s) => Ok(s.as_str()),
        other => stop!(Generic => "expected a name in a #%signature, found {}", other),
    }
}

/// `(rest T)`: `T`.
fn rest_clause(value: &SteelVal) -> Result<Option<&SteelVal>> {
    if let SteelVal::ListV(items) = value {
        if let (Some(head), 2) = (items.first(), items.len()) {
            if symbol(head)? == "rest" {
                return Ok(items.get(1));
            }
        }
    }
    Ok(None)
}

/// `Int`, or `(or List Pair)`.
fn arg_type(value: &SteelVal) -> Result<ArgType> {
    let members = match value {
        SteelVal::SymbolV(_) => vec![value_type(value)?],
        SteelVal::ListV(items) if items.first().map(symbol).transpose()? != Some("or") => {
            vec![value_type(value)?]
        }
        SteelVal::ListV(items) => items
            .iter()
            .skip(1)
            .map(value_type)
            .collect::<Result<Vec<_>>>()?,
        other => stop!(Generic => "expected a type or (or ...) in a #%signature, found {}", other),
    };
    Ok(ArgType(Box::leak(members.into_boxed_slice())))
}

fn value_type(value: &SteelVal) -> Result<ValueType> {
    if let SteelVal::ListV(items) = value {
        if let [head, SteelVal::StringV(name)] = items.iter().collect::<Vec<_>>().as_slice() {
            if symbol(head)? == "custom" {
                return Ok(ValueType::Custom(CustomTypeName::named(name.as_str())));
            }
        }
        stop!(Generic => "expected a type or (custom \"path::Type\") in a #%signature, found {}", value)
    }
    let name = symbol(value)?;
    ValueType::ALL
        .iter()
        .copied()
        .find(|ty| format!("{ty:?}") == name)
        .ok_or_else(|| {
            SteelErr::new(
                crate::rerrs::ErrorKind::Generic,
                format!("unknown type `{name}` in a #%signature"),
            )
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterFn;

    fn parse_text(text: &str) -> Result<HostSignature> {
        let mut engine = Engine::new();
        let clauses = engine
            .compile_and_run_raw_program(format!("(quote {text})"))?
            .pop()
            .unwrap();
        parse(&clauses)
    }

    #[test]
    fn signatures_round_trip() {
        for text in [
            "(#%signature (args Int Int) (returns Int))",
            "(#%signature (args (or String Symbol)) (returns String))",
            "(#%signature (args String (rest Any)) (returns Void) (raises BadArgs))",
            "(#%signature (args))",
            "(#%signature (args (rest (or List Pair))) (raises Never))",
            r#"(#%signature (args (custom "app::Counter") (or Int (custom "app::Gauge"))) (returns (custom "app::Counter")))"#,
        ] {
            let clauses = text
                .strip_prefix("(#%signature ")
                .and_then(|t| t.strip_suffix(')'))
                .unwrap();
            let signature = parse_text(&format!("({clauses})")).unwrap();
            assert_eq!(render(&signature), text);
        }
    }

    #[test]
    fn malformed_signatures_are_errors() {
        for clauses in [
            "((args Lisst))",
            "((args (rest Int) Int))",
            "((raises Sometimes))",
            "((returns (or Int String)))",
            "((takes Int))",
            "((args (custom Counter)))",
        ] {
            assert!(parse_text(clauses).is_err(), "{clauses} parsed");
        }
    }

    #[test]
    fn generated_definitions_describe_registered_functions() {
        let mut engine = Engine::new();
        let mut module = BuiltInModule::new("host/demo");
        module.register_fn("host-add", |a: isize, b: isize| a + b);
        module.register_fn("host-shout", |s: String| s.to_uppercase());
        module.register_value("host-version", SteelVal::IntV(1));
        #[derive(Clone)]
        struct Counter(isize);
        impl crate::rvals::Custom for Counter {}
        module.register_fn("counter-value", |c: &Counter| c.0);
        engine.register_value("app-context", SteelVal::IntV(0));
        module.register_fn_with_ctx("app-context", "ctx-scale", |ctx: isize, x: f64| {
            x * ctx as f64
        });
        engine.register_module(module);
        let text = engine.emit_lsp_definitions();
        assert!(
            text.contains(r#"(define module-0 (#%module "host/demo"))"#),
            "{text}"
        );
        assert!(text.contains(
            r#"(#%module-add module-0 "host-add" (#%signature (args Int Int) (returns Int)))"#
        ));
        assert!(text.contains(
            r#"(#%module-add module-0 "host-shout" (#%signature (args (or String Symbol)) (returns String)))"#
        ));
        assert!(text.contains(r#"(#%module-add module-0 "host-version" void)"#));
        // Without the context, which the wrapper passes.
        assert!(text.contains(
            r#"(#%module-add module-0 "ctx-scale" (#%signature (args Float) (returns Float)))"#
        ));
        // Only what the host added.
        assert!(!text.contains("steel/time"), "{text}");

        // The language server reads the file back into a module with the same
        // signatures.
        let mut reader = Engine::new();
        register(&mut reader).unwrap();
        reader.compile_and_run_raw_program(text.clone()).unwrap();
        let read = reader.extract::<BuiltInModule>("module-0").unwrap();
        let add = read.try_get_ref("host-add").unwrap();
        assert_eq!(
            render(&declared(&add).unwrap()),
            "(#%signature (args Int Int) (returns Int))"
        );
        // A host type keeps its full name, which is what its values report.
        let counter = format!(
            "(#%signature (args (custom {:?})) (returns Int))",
            core::any::type_name::<Counter>()
        );
        assert!(text.contains(&counter), "{text}");
        let value = read.try_get_ref("counter-value").unwrap();
        assert_eq!(render(&declared(&value).unwrap()), counter);
    }
}
