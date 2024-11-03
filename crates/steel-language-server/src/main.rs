use std::{path::PathBuf, sync::Arc};

use dashmap::{DashMap, DashSet};

use steel::{
    parser::interner::InternedString,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};
use steel_language_server::backend::{lsp_home, Backend, ExternalModuleResolver, ENGINE};

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
    globals_set.insert("#%register-additional-search-path".into());

    let cloned_set = globals_set.clone();
    resolver_engine.register_fn("#%register-global", move |global: String| {
        cloned_set.insert(InternedString::from(global))
    });

    let ignore_unused_set = Arc::new(DashSet::new());
    let cloned_ignore_set = ignore_unused_set.clone();
    resolver_engine.register_fn("#%ignore-unused-identifier", move |global: String| {
        cloned_ignore_set.insert(InternedString::from(global));
    });

    let additional_search_paths = Arc::new(DashSet::new());
    let cloned_additional_search_paths = additional_search_paths.clone();
    resolver_engine.register_fn("#%register-additional-search-path", move |path: String| {
        cloned_additional_search_paths.insert(path);
    });

    let home_directory = lsp_home();

    ENGINE.write().unwrap().register_module_resolver(
        ExternalModuleResolver::new(&mut resolver_engine, PathBuf::from(home_directory)).unwrap(),
    );

    {
        let mut guard = ENGINE.write().unwrap();

        for dir in additional_search_paths.iter() {
            guard.add_search_directory(PathBuf::from(dir.to_string()));
        }
    };

    let defined_globals = DashSet::new();

    for global in ENGINE.read().unwrap().globals().iter() {
        let resolved = global.resolve();
        if !resolved.starts_with("#")
            && !resolved.starts_with("%")
            && !resolved.starts_with("mangler#%")
            && !resolved.starts_with("mangler")
            && !resolved.starts_with("__module")
        {
            defined_globals.insert(resolved.to_string());
        }
    }

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
