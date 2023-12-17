use std::{path::PathBuf, sync::Arc};

use dashmap::{DashMap, DashSet};

use steel::{
    parser::interner::InternedString,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};
use steel_language_server::backend::{Backend, ExternalModuleResolver, ENGINE};

use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    // Use this to resolve the module configuration files
    let mut resolver_engine = Engine::new();

    let globals_set = Arc::new(DashSet::new());

    globals_set.insert("#%ignore-unused-identifier".into());
    globals_set.insert("#%register-global".into());

    let cloned_set = globals_set.clone();
    resolver_engine.register_fn("#%register-global", move |global: String| {
        cloned_set.insert(InternedString::from(global))
    });

    let ignore_unused_set = Arc::new(DashSet::new());
    let cloned_ignore_set = ignore_unused_set.clone();
    resolver_engine.register_fn("#%ignore-unused-identifier", move |global: String| {
        cloned_ignore_set.insert(InternedString::from(global));
    });

    ENGINE.with_borrow_mut(|x| {
        x.register_module_resolver(
            ExternalModuleResolver::new(
                &mut resolver_engine,
                PathBuf::from("/home/matt/.config/steel-lsp/"),
            )
            .unwrap(),
        )
    });

    let defined_globals = DashSet::new();

    ENGINE.with_borrow(|engine| {
        for global in engine.globals() {
            let resolved = global.resolve();
            if !resolved.starts_with("#")
                && !resolved.starts_with("%")
                && !resolved.starts_with("mangler#%")
            {
                defined_globals.insert(resolved.to_string());
            }
        }
    });

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        _macro_map: DashMap::new(),
        globals_set,
        ignore_set: ignore_unused_set,
        defined_globals,
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
