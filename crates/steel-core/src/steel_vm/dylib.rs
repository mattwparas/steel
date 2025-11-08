#![allow(non_camel_case_types)]
use crate::{path::PathBuf as SteelPath, sync::Mutex};
use alloc::format;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use std::{cell::RefCell, collections::HashMap, path::Path as StdPath, rc::Rc};

use abi_stable::{
    library::{LibraryError, RootModule},
    package_version_strings,
    sabi_types::VersionStrings,
    std_types::{RBox, RBoxError},
    StableAbi,
};
use once_cell::sync::Lazy;

use crate::{
    compiler::modules::steel_home,
    rvals::{IntoSteelVal, SteelString, SteelVal},
};

use super::{builtin::BuiltInModule, ffi::FFIModule};

// The new and improved loading of modules
static LOADED_MODULES: Lazy<Arc<Mutex<Vec<(String, GenerateModule_Ref)>>>> =
    Lazy::new(|| Arc::new(Mutex::new(Vec::new())));

thread_local! {
    static BUILT_DYLIBS: Rc<RefCell<HashMap<String, BuiltInModule>>> = Rc::new(RefCell::new(HashMap::new()));
}

#[cfg(feature = "sync")]
static STATIC_BUILT_DYLIBS: Lazy<Mutex<HashMap<String, BuiltInModule>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

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
pub fn load_root_module_in_directory(file: &StdPath) -> Result<GenerateModule_Ref, LibraryError> {
    abi_stable::library::lib_header_from_path(file)
        .and_then(|x| x.init_root_module::<GenerateModule_Ref>())
}

pub fn load_root_module_in_directory_manual(
    file: &StdPath,
) -> Result<(GenerateModule_Ref, Option<usize>), LibraryError> {
    let header = abi_stable::library::lib_header_from_path(file)
        .expect("plugin library header must be loadable");

    let mut max_enum: Option<usize> = None;

    if let abi_stable::library::IsLayoutChecked::Yes(layout) = header.root_mod_consts().layout() {
        // Note the full path here, the abi_checking module is hidden from documentation
        // Also note the arguments have been reversed, passing the plugin's layout first
        if let Err(errs) = abi_stable::abi_stability::abi_checking::check_layout_compatibility(
            GenerateModule_Ref::LAYOUT,
            layout,
        ) {
            for err in &errs.errors {
                for e in &err.errs {
                    match e {
                        abi_stable::abi_stability::abi_checking::AI::TooManyVariants(e)
                        | abi_stable::abi_stability::abi_checking::AI::FieldCountMismatch(e) => {
                            for trace in &err.stack_trace {
                                match trace.expected {
                                    abi_stable::type_layout::TLFieldOrFunction::Field(tlfield) => {
                                        if tlfield.full_type().name() == "FFIArg"
                                            || tlfield.full_type().name() == "FFIValue"
                                        {
                                            // This is an older plugin. Assuming the FFIArg layout
                                            // hasn't changed and this is the only issue, then we
                                            // should be okay to continue.
                                            if e.found < e.expected {
                                                // This is going to be the maximum enum variant that we'll
                                                // allow
                                                max_enum = Some(e.found - 1);
                                            }
                                        }
                                    }
                                    abi_stable::type_layout::TLFieldOrFunction::Function(_) => {}
                                }
                            }

                            if max_enum.is_none() {
                                return Err(LibraryError::AbiInstability(RBoxError::new(errs)));
                            }
                        }
                        // IF this isn't one of our known issues, we're just going to bail immediately
                        _ => return Err(LibraryError::AbiInstability(RBoxError::new(errs))),
                    }
                }
            }
        }
    };

    unsafe { header.init_root_module_with_unchecked_layout::<GenerateModule_Ref>() }
        .map(|x| (x, max_enum))

    // If we want to include version checking, use this instead:
    // let lib = unsafe {
    //     header
    //         .unchecked_layout::<GenerateModule_Ref>()
    //         .expect("plugin broke while loading")
    // };
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
            #[cfg(feature = "sync")]
            {
                if let Some(module) = STATIC_BUILT_DYLIBS
                    .lock()
                    .unwrap()
                    .get(target.as_str())
                    .cloned()
                {
                    return Some(module);
                }
            }

            #[cfg(not(feature = "sync"))]
            {
                if let Some(module) =
                    BUILT_DYLIBS.with(|x| x.borrow().get(target.as_str()).cloned())
                {
                    return Some(module);
                }
            }

            let home = steel_home();

            if let Some(home) = home {
                let mut module_guard = LOADED_MODULES.lock().unwrap();

                let mut home = SteelPath::from(home);
                home.push("native");

                if home.exists() {
                    let paths = std::fs::read_dir(home).unwrap();

                    for path in paths {
                        let path = path.unwrap().path();

                        if path.extension().unwrap() != std::env::consts::DLL_EXTENSION {
                            continue;
                        }

                        let path_name = path.file_stem().and_then(|x| x.to_str()).unwrap();

                        // Didn't match! skip it
                        if path_name != target.as_str() {
                            continue;
                        }

                        let module_name = path_name.to_string();

                        if module_guard.iter().find(|x| x.0 == path_name).is_some() {
                            continue;
                        }

                        log::info!(target: "dylibs", "Loading dylib: {:?}", path);

                        // Load the module in
                        let (container, max_enum) =
                            load_root_module_in_directory_manual(&path).unwrap();

                        let dylib_module = container.generate_module()();

                        module_guard.push((module_name, container));

                        let external_module =
                            crate::steel_vm::ffi::FFIWrappedModule::new(dylib_module, max_enum)
                                .expect("dylib failed to load!")
                                .build();

                        #[cfg(feature = "sync")]
                        {
                            STATIC_BUILT_DYLIBS
                                .lock()
                                .unwrap()
                                .insert(target.to_string(), external_module.clone());
                        }

                        #[cfg(not(feature = "sync"))]
                        {
                            BUILT_DYLIBS.with(|x| {
                                x.borrow_mut()
                                    .insert(target.to_string(), external_module.clone())
                            });
                        }

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
