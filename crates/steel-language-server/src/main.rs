use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    sync::Arc,
};

use dashmap::{DashMap, DashSet};

use steel::{
    compiler::modules::MANGLER_PREFIX,
    parser::{expander::SteelMacro, interner::InternedString},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};
use steel_language_server::backend::{
    lsp_home, Backend, Config, ExternalModuleResolver, FileState, ENGINE,
};

use tower_lsp::{lsp_types::Url, LspService, Server};

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
            && !resolved.starts_with(MANGLER_PREFIX)
            && !resolved.starts_with("__module")
        {
            defined_globals.insert(resolved.to_string());
        }
    }

    let vfs = DashMap::new();

    // Walk all of the files that end with .scm, and require them:
    for result in ignore::Walk::new("./") {
        match result {
            Ok(entry) => {
                entry.path();

                let path = entry.path();

                if path.extension().and_then(|x| x.to_str()) != Some("scm") {
                    continue;
                }

                let url = std::fs::canonicalize(path)
                    .map_err(|_| ())
                    .and_then(|path| Url::from_file_path(path));

                // Only show things that are actually present within the
                // context of this file
                if let Ok(url) = url {
                    vfs.insert(url, FileState { opened: false });
                }

                let mut guard = ENGINE.write().unwrap();

                eprintln!("Visiting: {:?}", path);

                // guard.add_module(path.to_str().unwrap().to_string()).ok();

                // Require the path to warm the compiler, but don't bring this into the global scope.
                // We're going to fake this into a different file, by creating another module
                // that will require this, and then require that explicitly.

                let macro_env_before: HashSet<InternedString> =
                    guard.in_scope_macros().keys().copied().collect();

                // TODO: Add span to the macro definition!
                let mut introduced_macros: HashMap<InternedString, SteelMacro> = HashMap::new();

                // TODO: This is still causing issues.
                // Somehow it seems there is something wrong with the modules as they get loaded,
                // and the environments between ASTs is not correct
                let _ = guard.emit_expanded_ast(&format!(r"(require {:?})", path), None);

                guard.in_scope_macros_mut().retain(|key, value| {
                    if macro_env_before.contains(key) {
                        return true;
                    } else {
                        // FIXME: Try to avoid this clone!
                        introduced_macros.insert(*key, value.clone());
                        false
                    }
                });

                eprintln!("Successfully loaded: {:?}", path);
            }
            _ => {}
        }
    }

    eprintln!("Finished indexing workspace");

    let root = std::env::current_dir().unwrap();

    let (service, socket) = LspService::build(|client| Backend {
        config: Config::new(),
        client,
        vfs,
        root,
        ast_map: DashMap::new(),
        raw_ast_map: DashMap::new(),
        lowered_ast_map: DashMap::new(),
        document_map: DashMap::new(),
        _macro_map: DashMap::new(),
        globals_set,
        ignore_set: ignore_unused_set,
        defined_globals,
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
