use std::{error::Error, path::PathBuf, process::Command};

use cargo_metadata::{Message, MetadataCommand, Package};
use std::process::Stdio;

fn package_contains_dependency_on_steel(packages: &[Package]) -> Option<&Package> {
    packages.iter().find(|x| x.name == "steel-core")
}

/*
TODO:
- Desired output directory / do not copy to native automatically
- Specify target architecture
*/

pub fn steel_home() -> Option<PathBuf> {
    std::env::var("STEEL_HOME")
        .ok()
        .map(PathBuf::from)
        .or_else(|| {
            let home = env_home::env_home_dir().map(|x| x.join(".steel"));

            if let Some(home) = home {
                if home.exists() {
                    return Some(home);
                }

                #[cfg(target_os = "windows")]
                {
                    if let Err(_) = std::fs::create_dir(&home) {
                        eprintln!("Unable to create steel home directory {:?}", home)
                    }

                    return Some(home);
                }
            }
            #[cfg(not(target_os = "windows"))]
            {
                let bd = xdg::BaseDirectories::new();
                let home = bd.data_home;

                home.map(|mut x: PathBuf| {
                    x.push("steel");

                    // Just go ahead and initialize the directory, even though
                    // this is probably not the best place to do this. This almost
                    // assuredly could be lifted out of this check since failing here
                    // could cause some annoyance.
                    if !x.exists() {
                        if let Err(_) = std::fs::create_dir(&x) {
                            eprintln!("Unable to create steel home directory {:?}", x)
                        }
                    }

                    x
                })
            }

            #[cfg(target_os = "windows")]
            None
        })
}

pub fn run(args: Vec<String>, env_vars: Vec<(String, String)>) -> Result<(), Box<dyn Error>> {
    let mut steel_home = steel_home().expect("Unable to find STEEL_HOME");

    steel_home.push("native");

    if !steel_home.exists() {
        std::fs::create_dir(&steel_home)?;
    }

    // --manifest-path
    let mut metadata_command = MetadataCommand::new();

    for pair in args.chunks(2) {
        match &pair {
            &[arg, path] if arg == "--manifest-path" => {
                metadata_command.manifest_path(path);
            }
            _ => {}
        }
    }

    let metadata = metadata_command.exec()?;

    let package = match metadata.root_package() {
        Some(p) => p,
        None => return Err("cargo steel-lib must be run from within a crate".into()),
    };

    println!("Attempting to install: {:#?}", package.name);

    if package_contains_dependency_on_steel(&metadata.packages).is_none() {
        return Err(
            "Cannot install package as a steel dylib - does not contain a dependency on steel!"
                .into(),
        );
    }

    let mut command = Command::new("cargo")
        .args([
            "build",
            "--release",
            "--message-format=json-render-diagnostics",
        ])
        .args(args)
        .envs(env_vars)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let reader = std::io::BufReader::new(command.stdout.take().unwrap());

    let artifacts = cargo_metadata::Message::parse_stream(reader).filter_map(|x| {
        if let Ok(Message::CompilerArtifact(artifact)) = x {
            Some(artifact)
        } else {
            None
        }
    });

    for last in artifacts {
        if last
            .target
            .kind
            .iter()
            .find(|x| x.as_str() == "cdylib")
            .is_some()
        {
            for file in last.filenames {
                if file.extension() == Some(std::env::consts::DLL_EXTENSION) {
                    println!("Found a cdylib!");
                    let filename = file.file_name().unwrap();

                    steel_home.push(filename);

                    println!("Copying {} to {}", file, &steel_home.to_str().unwrap());

                    if steel_home.exists() {
                        std::fs::remove_file(&steel_home)
                            .expect("Unable to delete the existing dylib");
                    }

                    std::fs::copy(file, &steel_home).unwrap();

                    steel_home.pop();
                    break;
                }
            }
        }
    }

    println!("Done!");

    command.wait().expect("Couldn't get cargo's exit status");

    Ok(())
}
