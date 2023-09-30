#![allow(non_camel_case_types)]
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
    sync::{Arc, Mutex},
};

use abi_stable::{
    library::{LibraryError, RootModule},
    package_version_strings,
    sabi_types::VersionStrings,
    std_types::RBox,
    StableAbi,
};
use once_cell::sync::Lazy;

use crate::rvals::{IntoSteelVal, SteelString, SteelVal};

use super::{builtin::BuiltInModule, ffi::FFIModule};

// The new and improved loading of modules
static LOADED_MODULES: Lazy<Arc<Mutex<Vec<(String, GenerateModule_Ref)>>>> =
    Lazy::new(|| Arc::new(Mutex::new(Vec::new())));

thread_local! {
    static BUILT_DYLIBS: Rc<RefCell<HashMap<String, BuiltInModule>>> = Rc::new(RefCell::new(HashMap::new()));
}

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

#[derive(Clone)]
pub(crate) struct DylibContainers {}

#[steel_derive::function(name = "#%get-dylib")]
pub fn load_module(target: &SteelString) -> crate::rvals::Result<SteelVal> {
    match DylibContainers::load_module(target.clone()) {
        Some(container) => container.into_steelval(),
        None => {
            stop!(Generic => format!("dylib not found: {} or dylibs are not enabled for this instance of the steel runtime", target))
        }
    }
}

impl DylibContainers {
    pub fn new() -> Self {
        Self {
            // containers: Arc::new(Mutex::new(Vec::new())),
        }
    }

    // home should... probably just be $STEEL_HOME?
    pub fn load_module(target: SteelString) -> Option<BuiltInModule> {
        #[cfg(not(feature = "dylibs"))]
        {
            None // TODO: This _should_ just error instead!
        }

        #[cfg(feature = "dylibs")]
        {
            if let Some(module) = BUILT_DYLIBS.with(|x| x.borrow().get(target.as_str()).cloned()) {
                return Some(module);
            }

            let home = std::env::var("STEEL_HOME").ok();

            if let Some(home) = home {
                // let guard = LOADED_DYLIBS.lock().unwrap();
                let mut module_guard = LOADED_MODULES.lock().unwrap();

                let mut home = PathBuf::from(home);
                home.push("native");

                if home.exists() {
                    let paths = std::fs::read_dir(home).unwrap();

                    for path in paths {
                        // println!("{:?}", path);

                        let path = path.unwrap().path();

                        if path.extension().unwrap() != "so" && path.extension().unwrap() != "dylib"
                        {
                            continue;
                        }

                        let path_name = path
                            .file_stem()
                            // .file_name()
                            .and_then(|x| x.to_str())
                            .unwrap();
                        log::info!(target: "dylibs", "Loading dylib: {}", path_name);

                        // Didn't match! skip it
                        if path_name != target.as_str() {
                            continue;
                        }

                        let module_name = path_name.to_string();

                        if module_guard.iter().find(|x| x.0 == path_name).is_some() {
                            continue;
                        }

                        // Load the module in
                        let container = load_root_module_in_directory(&path).unwrap();

                        let dylib_module = container.generate_module()();

                        module_guard.push((module_name, container));

                        let external_module =
                            crate::steel_vm::ffi::FFIWrappedModule::new(dylib_module)
                                .expect("dylib failed to load!")
                                .build();

                        BUILT_DYLIBS.with(|x| {
                            x.borrow_mut()
                                .insert(target.to_string(), external_module.clone())
                        });

                        log::info!(target: "dylibs", "Registering dylib: {} - {}", path_name, target);

                        return Some(external_module);
                    }
                } else {
                    log::warn!(target: "dylibs", "$STEEL_HOME/native directory does not exist")
                }
            } else {
                log::warn!(target: "dylibs", "STEEL_HOME variable missing - unable to read shared dylibs")
            }

            None
        }
    }
}
