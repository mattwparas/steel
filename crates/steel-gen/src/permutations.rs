use codegen::Impl;

// Awful, ugly back to generate all of the inputs that I need across different params

// It might be worth flattening this vector, but that is quite a bit of contiguous memory to allocate.
// lets just deal with the fact that this might be broken up and take the cache hits. Alternatively, it would probably
// make a lot of sense to do this with an iterator.

pub const SUPPORTED_FUNCTION_ARITY: u32 = 5;

fn generate_permutations<T: Copy>(input: &[T]) -> Vec<Vec<Vec<T>>> {
    let mut buffers = Vec::with_capacity(SUPPORTED_FUNCTION_ARITY as usize);

    for i in 0..SUPPORTED_FUNCTION_ARITY {
        buffers.push(Vec::with_capacity(input.len().pow(i)));
    }

    for a in input {
        buffers[0].push(vec![*a]);

        for b in input {
            buffers[1].push(vec![*a, *b]);

            for c in input {
                buffers[2].push(vec![*a, *b, *c]);

                for d in input {
                    buffers[3].push(vec![*a, *b, *c, *d]);

                    for e in input {
                        buffers[4].push(vec![*a, *b, *c, *d, *e]);

                        //     // for f in input {
                        //     //     buffers[5].push(vec![*a, *b, *c, *d, *e, *f]);

                        //     //     // TODO: If this can be done, we want to get up to like 8 parameters if possible?

                        //     //     // for g in input {
                        //     //     //     buffers[6].push(vec![*a, *b, *c, *d, *e, *f, *g]);

                        //     //     //     for h in input {
                        //     //     //         buffers[7].push(vec![*a, *b, *c, *d, *e, *f, *g, *h])
                        //     //     //     }
                        //     //     // }
                        // }
                    }
                }
            }
        }
    }

    buffers
}

#[derive(Copy, Clone)]
enum ValueKind {
    Value,
    Reference,
    MutReference,
}

impl ValueKind {
    fn format_array_to_tuple(values: &[ValueKind]) -> String {
        let mut output = "(".to_string();

        for i in 0..values.len() {
            output.push('T');
            output.push_str(&i.to_string());
            output.push(',');
        }

        output.push(')');

        output
    }

    // Fn(&SELF, AREA, &CTX)
    fn format_array_to_function_args(values: &[ValueKind]) -> String {
        let mut output = "(".to_string();

        for (i, value) in values.iter().enumerate() {
            if let Some(reference_modifier) = value.as_reference_type_repr() {
                output.push_str(reference_modifier);
                output.push(' ');
            }
            output.push('T');
            output.push_str(&i.to_string());
            output.push(',');
        }

        output.push(')');

        output
    }

    // Convert this to a reference type for the use of code gen
    fn as_reference_type_repr(&self) -> Option<&'static str> {
        match self {
            Self::Value => None,
            Self::Reference => Some("&"),
            Self::MutReference => Some("&mut"),
        }
    }

    // In theory, we might have more, but I need to iron this out from the
    // actual implementation side of things.
    fn as_from_trait_name(&self) -> &'static str {
        match self {
            Self::Value => "FromSteelVal",
            Self::Reference => "AsRefSteelVal",
            Self::MutReference => "AsRefMutSteelVal",
        }
    }
}

const FN: &str = "FN";
const RET: &str = "RET";

fn code_gen_permutation(values: &[ValueKind], id: usize, row: usize) -> String {
    // RegisterFn<FN, MarkerWrapper8<(SELF, AREA, CTX)>, RET>
    let mut trait_to_impl_type = codegen::Type::new("RegisterFn");
    trait_to_impl_type.generic(FN);
    let mut wrapper = codegen::Type::new(format!("Marker{row}{id}"));
    wrapper.generic(ValueKind::format_array_to_tuple(values));
    trait_to_impl_type.generic(wrapper);
    trait_to_impl_type.generic(RET);

    // Target type to impl the trait for
    // let mut impl_trait = Impl::new("BuiltInModule");

    let mut impl_trait = Impl::new("FakeEngine");

    // We're implementing the above RegisterFn trait
    impl_trait.impl_trait(trait_to_impl_type);

    let ret_type = codegen::Type::new(RET);

    impl_trait.generic(RET);
    impl_trait.bound(RET, "IntoSteelVal"); // TODO: Figure out if these bounds are respected or not
    for (i, value) in values.iter().enumerate() {
        let type_name = format!("T{i}");

        impl_trait.generic(&type_name);
        impl_trait.bound(&type_name, value.as_from_trait_name()); // TODO: This is where we're gonna need to match to pick the right kind
    }

    impl_trait.generic(FN);
    // impl_trait.bound()

    let function_string = "Fn".to_string()
        + &ValueKind::format_array_to_function_args(values)
        + " -> "
        + "RET + SendSyncStatic";

    impl_trait.bound(FN, &function_string);

    let function_def = impl_trait.new_fn("register_fn");

    // Function signature
    function_def
        .arg_mut_self()
        .arg("name", "&'static str")
        .arg("func", FN)
        .ret("&mut Self");

    function_def.line("let f = move |args: &[SteelVal]| -> Result<SteelVal> {");
    function_def.line(format!("if args.len() != {} {{", values.len()));
    function_def.line(format!(
        "stop!(ArityMismatch => \"{{}} expected {} arguments, got {{}}\", name, args.len());",
        values.len(),
    ));
    function_def.line("}");

    for (i, value) in values.iter().enumerate() {
        match value {
            ValueKind::Value => {
                function_def.line(format!(
                    "let mut v{i} = <T{i}>::from_steelval(&args[{i}])?;"
                ));
            }
            ValueKind::Reference => {
                function_def.line(format!(
                    "let mut nursery{i} = <T{i} as AsRefSteelVal>::Nursery::default();"
                ));
                function_def.line(format!(
                    "let mut v{i} = <T{i}>::as_ref(&args[{i}], &mut nursery{i})?;"
                ));
            }
            ValueKind::MutReference => {
                function_def.line(format!(
                    "let mut v{i} = <T{i}>::as_mut_ref(&args[{i}])?;"
                ));
            }
        }
    }

    {
        function_def.line("let res = func(");

        for (i, value) in values.iter().enumerate() {
            // function_def.line

            let mut var = String::new();
            if let Some(modifier) = value.as_reference_type_repr() {
                var.push_str(modifier);
            }

            var.push_str(&format!(" v{i},"));

            function_def.line(var);
        }

        function_def.line(");");

        function_def.line("res.into_steelval()");
    }

    function_def.line("};");

    function_def.line(format!(
        r#"

        self.register_value(
            name,
            SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
                Arc::new(f), 
                Some(name), 
                Some({})
            ))),
        )
            
        "#,
        values.len()
    ));

    let mut scope = codegen::Scope::new();

    let mut marker = scope.new_struct(&format!("Marker{row}{id}"));

    marker.generic("ARGS");
    marker.tuple_field("PhantomData<ARGS>");

    scope.push_impl(impl_trait);

    scope.to_string()
}

pub fn code_gen() -> String {
    let values = &[
        ValueKind::Value,
        ValueKind::Reference,
        ValueKind::MutReference,
    ];

    let permutations = generate_permutations(values);

    let mut out = Vec::new();

    for (r, row) in permutations.into_iter().enumerate() {
        for (index, column) in row.into_iter().enumerate() {
            out.push(code_gen_permutation(&column, index, r));
        }
    }

    out.join("\n")
}

#[test]
fn simple_code_gen() {
    let values = &[
        ValueKind::Value,
        ValueKind::Reference,
        ValueKind::MutReference,
    ];

    // println!("{}", code_gen_permutation(values));

    let permutations = generate_permutations(values);

    for (r, row) in permutations.into_iter().enumerate() {
        for (index, column) in row.into_iter().enumerate() {
            println!("{}", code_gen_permutation(&column, index, r));
        }
    }
}
