use std::{
    path::PathBuf,
    rc::Rc,
    sync::{Arc, Mutex},
};

use dlopen::wrapper::{Container, WrapperApi};
use dlopen_derive::WrapperApi;
use once_cell::sync::Lazy;

use super::builtin::BuiltInModule;

static LOADED_DYLIBS: Lazy<Arc<Mutex<Vec<(String, Container<ModuleApi>)>>>> =
    Lazy::new(|| Arc::new(Mutex::new(Vec::new())));

// #[derive(WrapperApi, Clone)]
// struct ModuleApi {
// generate_module: fn() -> Rc<BuiltInModule>,
// }

#[derive(WrapperApi, Clone)]
struct ModuleApi {
    generate_module: fn() -> *mut BuiltInModule,
    build_module: fn(module: &mut BuiltInModule),
    free_module: fn(ptr: *mut BuiltInModule),
}

#[derive(Clone)]
pub(crate) struct DylibContainers {
    // containers: Arc<Mutex<Vec<(String, Container<ModuleApi>)>>>,
}

impl DylibContainers {
    pub fn new() -> Self {
        Self {
            // containers: Arc::new(Mutex::new(Vec::new())),
        }
    }

    // TODO: @Matt - make these load modules lazily. Loading all modules right at the start
    // could be fairly burdensome from a startup time standpoint, and also requires modules to be separated from the standard ones.
    pub fn load_modules_from_directory(&mut self, home: Option<String>) {
        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        // let home = std::env::var("STEEL_HOME");

        if let Some(home) = home {
            let mut guard = LOADED_DYLIBS.lock().unwrap();

            let mut home = PathBuf::from(home);
            home.push("native");

            if home.exists() {
                let paths = std::fs::read_dir(home).unwrap();

                for path in paths {
                    // println!("{:?}", path);

                    let path = path.unwrap().path();

                    if path.extension().unwrap() != "so" && path.extension().unwrap() != "dylib" {
                        continue;
                    }

                    let path_name = path.file_name().and_then(|x| x.to_str()).unwrap();
                    log::info!(target: "dylibs", "Loading dylib: {}", path_name);

                    let module_name = path_name.to_string();

                    if guard.iter().find(|x| x.0 == path_name).is_some() {
                        // println!("Module already exists, skipping");

                        continue;
                    }

                    // Load in the dylib
                    let cont: Container<ModuleApi> = unsafe { Container::load(path) }
                        .expect("Could not open library or load symbols");

                    // Keep the container alive for the duration of the program
                    // This should probably just get wrapped up with the engine as well, when registering modules, directly
                    // register an external dylib
                    // self.containers.push(Rc::new(cont));

                    guard.push((module_name, cont));
                }
            } else {
                log::warn!(target: "dylibs", "$STEEL_HOME/native directory does not exist")
            }
        } else {
            log::warn!(target: "dylibs", "STEEL_HOME variable missing - unable to read shared dylibs")
        }

        // self.containers = Arc::clone(&LOADED_DYLIBS);

        #[cfg(feature = "profiling")]
        if log::log_enabled!(target: "pipeline_time", log::Level::Debug) {
            log::debug!(target: "pipeline_time", "Dylib loading time: {:?}", now.elapsed());
        }
    }

    // TODO: This should be lazily loaded on the first require-builtin
    // For now, we can just load everything at the start when the interpreter boots up
    pub fn load_modules(&mut self) {
        self.load_modules_from_directory(std::env::var("STEEL_HOME").ok())
    }

    // pub fn modules(&self) -> Vec<*const BuiltInModule> {
    //     LOADED_DYLIBS
    //         .lock()
    //         .unwrap()
    //         .iter()
    //         .map(|x| x.1.generate_module())
    //         .collect()
    // }

    pub fn modules(&self) -> Vec<*mut BuiltInModule> {
        LOADED_DYLIBS
            .lock()
            .unwrap()
            .iter()
            .map(|x| {
                // let mut module = BuiltInModule::raw();
                x.1.generate_module()
                // module
            })
            .collect()
    }
}
