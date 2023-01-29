use std::{path::PathBuf, rc::Rc};

use dlopen::wrapper::{Container, WrapperApi};
use dlopen_derive::WrapperApi;

use super::builtin::BuiltInModule;

#[derive(WrapperApi, Clone)]
struct ModuleApi {
    generate_module: fn() -> BuiltInModule,
}

#[derive(Clone)]
pub(crate) struct DylibContainers {
    containers: Vec<Rc<Container<ModuleApi>>>,
}

impl DylibContainers {
    pub fn new() -> Self {
        Self {
            containers: Vec::new(),
        }
    }

    // TODO: This should be lazily loaded on the first require-builtin
    // For now, we can just load everything at the start when the interpreter boots up
    pub fn load_modules(&mut self) {
        let home = std::env::var("STEEL_HOME");

        if let Ok(home) = home {
            let mut home = PathBuf::from(home);
            home.push("native");

            let paths = std::fs::read_dir(home).unwrap();

            for path in paths {
                // println!("{:?}", path);

                let path = path.unwrap().path();

                if path.extension().unwrap() != "so" && path.extension().unwrap() != "dylib" {
                    continue;
                }

                let path_name = path.file_name().and_then(|x| x.to_str()).unwrap();
                log::info!(target: "dylibs", "Loading dylib: {}", path_name);
                // Load in the dylib
                let cont: Container<ModuleApi> = unsafe { Container::load(path) }
                    .expect("Could not open library or load symbols");

                // Keep the container alive for the duration of the program
                // This should probably just get wrapped up with the engine as well, when registering modules, directly
                // register an external dylib
                self.containers.push(Rc::new(cont));
            }
        } else {
            log::warn!(target: "dylibs", "STEEL_HOME variable missing - unable to read shared dylibs")
        }
    }

    pub fn modules(&self) -> impl Iterator<Item = BuiltInModule> + '_ {
        self.containers.iter().map(|x| x.generate_module())
    }
}
