use crate::rvals::{Result, SteelVal};
use crate::steel_vm::builtin::{get_function_metadata, BuiltInFunctionType, BuiltInModule};
use crate::steel_vm::vm::VmCore;
use crate::value_type::{ArgType, ArgTypes, CustomTypeName, Raises, ValueType};
use crate::values::functions::{BoxedDynFunction, HostSignature};
use crate::SteelErr;
use alloc::sync::Arc;
use once_cell::sync::Lazy;
use rustc_hash::FxHashMap;
use std::sync::Mutex;

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
    let arg = render_arg_type;
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

/// `Int`, or `(or List Pair)`.
fn render_arg_type(ty: &ArgType) -> String {
    match ty.0 {
        [single] => render_type(single),
        members => {
            let members: Vec<String> = members.iter().map(render_type).collect();
            format!("(or {})", members.join(" "))
        }
    }
}

/// `Int`, `(listof Int)`, or `(custom "app::Counter")` for a host type.
fn render_type(ty: &ValueType) -> String {
    match ty {
        ValueType::Custom(name) => format!("(custom {:?})", name.name()),
        ValueType::ListOf(elements) => format!("(listof {})", render_arg_type(elements)),
        ValueType::HashOf(keys, values) => format!(
            "(hashof {} {})",
            render_arg_type(keys),
            render_arg_type(values)
        ),
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

#[steel_derive::context(name = "function-signature", arity = Exact(1), args(Any), raises = Never)]
pub fn function_signature(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(ctx.intern_symbols(signature_clauses(args[0].clone()))))
}

/// What a native or host function declares about its arguments, result and
/// errors, as the clauses of a `#%signature`: `((args T ...) (returns T) (raises R))`.
/// There is no `args` clause when its argument types weren't declared. False for
/// any other value.
pub fn signature_clauses(value: SteelVal) -> SteelVal {
    let metadata =
        |function| get_function_metadata(function).map(|m| (m.arg_types, m.return_type, m.raises));
    let declared = match &value {
        SteelVal::BoxedFunction(f) => f
            .signature
            .map(|s| (Some(s.arg_types), s.return_type, s.raises)),
        SteelVal::FuncV(f) => metadata(BuiltInFunctionType::Reference(*f)),
        SteelVal::MutFunc(f) => metadata(BuiltInFunctionType::Mutable(*f)),
        SteelVal::BuiltIn(f) => metadata(BuiltInFunctionType::Context(*f)),
        SteelVal::Closure(c) => CONTEXT_WRAPPERS
            .lock()
            .unwrap()
            .get(&c.id)
            .map(|s| (Some(s.arg_types), s.return_type, s.raises)),
        _ => None,
    };
    let Some((arg_types, return_type, raises)) = declared else {
        return SteelVal::BoolV(false);
    };

    let mut clauses = Vec::new();
    if let Some(arg_types) = arg_types {
        let mut args = vec![name("args")];
        args.extend(arg_types.fixed.iter().map(arg_type_value));
        if let Some(rest) = &arg_types.rest {
            args.push(data(vec![name("rest"), arg_type_value(rest)]));
        }
        clauses.push(data(args));
    }
    clauses.push(data(vec![name("returns"), type_value(&return_type)]));
    clauses.push(data(vec![name("raises"), name(&format!("{raises:?}"))]));
    data(clauses)
}

fn name(name: &str) -> SteelVal {
    SteelVal::SymbolV(name.into())
}

fn data(items: Vec<SteelVal>) -> SteelVal {
    SteelVal::ListV(items.into_iter().collect())
}

/// `Int`, or `(or List Pair)`.
fn arg_type_value(ty: &ArgType) -> SteelVal {
    match ty.0 {
        [single] => type_value(single),
        members => {
            let mut items = vec![name("or")];
            items.extend(members.iter().map(type_value));
            data(items)
        }
    }
}

/// `Int`, `(listof Int)`, or `(custom "app::Counter")` for a host type.
fn type_value(ty: &ValueType) -> SteelVal {
    match ty {
        ValueType::Custom(custom) => data(vec![
            name("custom"),
            SteelVal::StringV(custom.name().into()),
        ]),
        ValueType::ListOf(elements) => data(vec![name("listof"), arg_type_value(elements)]),
        ValueType::HashOf(keys, values) => data(vec![
            name("hashof"),
            arg_type_value(keys),
            arg_type_value(values),
        ]),
        other => name(&format!("{other:?}")),
    }
}

/// The signature of the function `module` binds to `name`, as programs call
/// it. A context function's first argument is the context, which its wrapper
/// passes, so callers supply the rest.
pub fn declared_in(module: &BuiltInModule, name: &str) -> Option<HostSignature> {
    let Some(function) = module.context_function(name) else {
        return declared(&module.try_get_ref(name)?);
    };
    without_context(declared(&function)?)
}

/// A context function's signature as its wrapper is called, without the context.
fn without_context(signature: HostSignature) -> Option<HostSignature> {
    let (_, fixed) = signature.arg_types.fixed.split_first()?;
    Some(HostSignature {
        arg_types: ArgTypes {
            fixed,
            rest: signature.arg_types.rest,
        },
        ..signature
    })
}

// The signatures of the closures that pass a context function its context, by
// function id
static CONTEXT_WRAPPERS: Lazy<Mutex<FxHashMap<u32, HostSignature>>> =
    Lazy::new(|| Mutex::new(FxHashMap::default()));

/// Records that the closure with id `wrapper` calls `function` with the context.
pub(crate) fn note_context_wrapper(wrapper: u32, function: &BoxedDynFunction) {
    if let Some(signature) = function.signature.and_then(without_context) {
        CONTEXT_WRAPPERS.lock().unwrap().insert(wrapper, signature);
    }
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

/// The `T` of a `(rest T)` clause.
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
        match items.iter().collect::<Vec<_>>().as_slice() {
            [head, SteelVal::StringV(name)] if symbol(head)? == "custom" => {
                return Ok(ValueType::Custom(CustomTypeName::named(name.as_str())));
            }
            [head, elements] if symbol(head)? == "listof" => {
                return Ok(ValueType::ListOf(arg_type(elements)?));
            }
            [head, keys, values] if symbol(head)? == "hashof" => {
                return Ok(ValueType::HashOf(arg_type(keys)?, arg_type(values)?));
            }
            _ => {}
        }
        stop!(Generic => "expected a type, (listof T), (hashof K V) or (custom \"path::Type\") in a #%signature, found {}", value)
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

    #[test]
    fn programs_can_read_signatures() {
        let mut engine = Engine::new();
        engine.register_fn("host-add", |a: isize, b: isize| a + b);
        #[derive(Clone)]
        struct Counter(isize);
        impl crate::rvals::Custom for Counter {}
        engine.register_fn("counter-value", |c: &Counter| c.0);
        engine.register_value("app-context", SteelVal::IntV(2));
        engine.register_fn_with_ctx("app-context", "engine-scale", |ctx: isize, x: f64| {
            x * ctx as f64
        });
        let mut module = BuiltInModule::new("host/ctx-signatures");
        module.register_fn_with_ctx("app-context", "module-scale", |ctx: isize, x: isize| {
            x * ctx
        });
        engine.register_module(module);

        let program = r#"
            (require-builtin host/ctx-signatures)

            (list (function-signature string-length)
                  (function-signature string-append)
                  (function-signature car)
                  (function-signature host-add)
                  (function-signature counter-value)
                  (function-signature engine-scale)
                  (function-signature module-scale)
                  (engine-scale 1.5)
                  (module-scale 4)
                  (function-signature (lambda (x) x))
                  (function-signature 10))
        "#;
        let result = engine.compile_and_run_raw_program(program).unwrap().pop();
        let expected = format!(
            r#"(list '((args String) (returns Int) (raises BadArgs))
                     '((args (rest String)) (returns String) (raises BadArgs))
                     '((args (or List Pair)) (returns Any) (raises Maybe))
                     '((args Int Int) (returns Int) (raises Maybe))
                     '((args (custom {:?})) (returns Int) (raises Maybe))
                     '((args Float) (returns Float) (raises Maybe))
                     '((args Int) (returns Int) (raises Maybe))
                     3.0
                     8
                     #f
                     #f)"#,
            core::any::type_name::<Counter>()
        );
        let expected = engine.compile_and_run_raw_program(expected).unwrap().pop();
        assert_eq!(result, expected);

        // The clauses are what `#%signature` reads.
        let add = engine.extract_value("host-add").unwrap();
        let clauses = signature_clauses(add.clone());
        assert_eq!(
            render(&parse(&clauses).unwrap()),
            render(&declared(&add).unwrap())
        );
    }

    // The contract `signature->contract` makes for each type accepts exactly
    // the values of that type.
    #[test]
    fn type_contracts_accept_exactly_their_types() {
        let mut engine = Engine::new();
        #[derive(Clone)]
        struct Counter(isize);
        impl crate::rvals::Custom for Counter {}
        engine.register_fn("make-counter", || Counter(0));
        let counter = format!("(custom {:?})", core::any::type_name::<Counter>());

        let mut types: Vec<String> = ValueType::ALL.iter().map(|ty| format!("{ty:?}")).collect();
        types.extend([
            counter.clone(),
            "(listof Int)".to_string(),
            "(listof (or String Char))".to_string(),
            "(hashof Symbol Int)".to_string(),
            "(or Int String)".to_string(),
        ]);

        let samples = r#"
            (struct callable (f) #:prop:procedure 0)
            (struct plain (x))
            (require "steel/signatures")
            (define samples
              (list 1 -5 (expt 2 70) 1.5 1/2 1+1i #\a "s" 'sym '() '(1 2) '("a" #\b) '(1 "a")
                    (cons 1 2) #(1 2) (vector 1) (flat-vector 1 2) (hash 'a 1) (hash 'a "x")
                    (hash "a" 1) (hashset 1) (bytes 1) void (lambda (x) x) car (callable car)
                    (plain 1) #t (make-counter) (call/cc (lambda (k) k))))
        "#;
        engine.compile_and_run_raw_program(samples).unwrap();
        let values = engine.extract_value("samples").unwrap();
        let values: Vec<SteelVal> = list(&values, "samples").unwrap();

        for text in types {
            let accepted = engine
                .compile_and_run_raw_program(format!(
                    "(let ([contract (car (FunctionContract-pre-conditions
                                            (signature->contract '((args {text}) (returns Any)))))])
                       (map (lambda (value) (if (contract value) #t #f)) samples))"
                ))
                .unwrap()
                .pop()
                .unwrap();
            let accepted = list(&accepted, "results").unwrap();
            let clauses = engine
                .compile_and_run_raw_program(format!("(quote ((args {text})))"))
                .unwrap()
                .pop()
                .unwrap();
            let ty = parse(&clauses).unwrap().arg_types.fixed[0];
            for (value, accepted) in values.iter().zip(accepted) {
                assert_eq!(
                    accepted,
                    SteelVal::BoolV(ty.matches(value)),
                    "{text} on {value}"
                );
            }
        }
    }
}
