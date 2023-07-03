#![allow(non_camel_case_types, unused)]
use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use abi_stable::{
    library::{LibraryError, RootModule},
    package_version_strings,
    sabi_types::VersionStrings,
    std_types::RBox,
    StableAbi,
};
use dlopen::wrapper::{Container, WrapperApi};
use dlopen_derive::WrapperApi;
use once_cell::sync::Lazy;

use super::ffi::FFIModule;

static LOADED_DYLIBS: Lazy<Arc<Mutex<Vec<(String, Container<ModuleApi>)>>>> =
    Lazy::new(|| Arc::new(Mutex::new(Vec::new())));

// The new and improved loading of modules
static LOADED_MODULES: Lazy<Arc<Mutex<Vec<(String, GenerateModule_Ref)>>>> =
    Lazy::new(|| Arc::new(Mutex::new(Vec::new())));

#[repr(C)]
#[derive(StableAbi)]
#[sabi(kind(Prefix(prefix_ref = GenerateModule_Ref)))]
#[sabi(missing_field(panic))]
pub struct GenerateModule {
    pub generate_module: extern "C" fn() -> RBox<FFIModule>,
}

/// The RootModule trait defines how to load the root module of a library.
impl RootModule for GenerateModule_Ref {
    abi_stable::declare_root_module_statics! {GenerateModule_Ref}

    const BASE_NAME: &'static str = "generate_module";
    const NAME: &'static str = "generate_module";
    const VERSION_STRINGS: VersionStrings = package_version_strings!();
}

// Load from the directory
pub fn load_root_module_in_directory(file: &Path) -> Result<GenerateModule_Ref, LibraryError> {
    GenerateModule_Ref::load_from_file(file)
}

#[derive(WrapperApi, Clone)]
struct ModuleApi {
    generate_module: extern "C" fn() -> RBox<FFIModule>,
    // build_module: fn(module: &mut BuiltInModule),
    // free_module: fn(ptr: *mut BuiltInModule),
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
            let guard = LOADED_DYLIBS.lock().unwrap();
            let mut module_guard = LOADED_MODULES.lock().unwrap();

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

                    // Load the module in
                    if true {
                        let container = load_root_module_in_directory(&path).unwrap();

                        module_guard.push((module_name, container));
                    } else {
                        // // Load in the dylib
                        // let cont: Container<ModuleApi> = unsafe { Container::load(path) }
                        //     .expect("Could not open library or load symbols");

                        // // Keep the container alive for the duration of the program
                        // // This should probably just get wrapped up with the engine as well, when registering modules, directly
                        // // register an external dylib
                        // // self.containers.push(Rc::new(cont));

                        // guard.push((module_name, cont));
                    }
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

    pub fn modules(&self) -> Vec<RBox<FFIModule>> {
        LOADED_MODULES
            .lock()
            .unwrap()
            .iter()
            .map(|x| {
                // let mut module = BuiltInModule::raw();
                x.1.generate_module()()
                // module
            })
            .collect()
    }
}
