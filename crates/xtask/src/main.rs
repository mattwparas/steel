use std::{
    error::Error,
    path::{Path, PathBuf},
};

fn workspace_dir() -> PathBuf {
    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    let cargo_path = Path::new(std::str::from_utf8(&output).unwrap().trim());
    cargo_path.parent().unwrap().to_path_buf()
}

// Generate documentation by invoking the docs script!
fn generate_docs() -> Result<(), Box<dyn Error>> {
    println!("Generating docs...");

    let mut workspace_dir = workspace_dir();

    let mut base = workspace_dir.clone();

    workspace_dir.push("crates");
    workspace_dir.push("steel-doc");

    std::process::Command::new("cargo")
        .arg("run")
        .current_dir(&workspace_dir)
        .spawn()?
        .wait()?;

    workspace_dir.pop();
    workspace_dir.pop();

    workspace_dir.push("docs");
    workspace_dir.push("src");
    workspace_dir.push("builtins");

    println!("Cleaning target directory...");

    std::fs::remove_dir_all(&workspace_dir)?;

    base.push("crates");
    base.push("steel-doc");
    base.push("generated");

    println!("Moving generated docs into place...");

    std::fs::rename(base, &workspace_dir)?;

    println!("Done!");

    Ok(())
}

fn install_everything() -> Result<(), Box<dyn Error>> {
    println!("Installing `steel`...");

    let mut workspace_dir = workspace_dir();

    std::process::Command::new("cargo")
        .arg("install")
        .arg("--path")
        .arg(".")
        .arg("--force")
        .spawn()?
        .wait()?;

    println!("Successfully installed `steel`");

    println!("Installing `steel-language-server`");

    workspace_dir.push("crates");
    workspace_dir.push("steel-language-server");

    std::process::Command::new("cargo")
        .arg("install")
        .arg("--path")
        .arg(&workspace_dir)
        .spawn()?
        .wait()?;

    println!("Successfully installed `steel-language-server`");

    workspace_dir.pop();

    workspace_dir.push("cargo-steel-lib");

    println!("Installing `cargo-steel-lib`");

    std::process::Command::new("cargo")
        .arg("install")
        .arg("--path")
        .arg(&workspace_dir)
        .spawn()?
        .wait()?;

    println!("Successfully installed `cargo-steel-lib`");

    println!("Installing all dylibs");

    let dylibs = &[
        "steel-sys-info",
        "steel-websockets",
        "steel-webrequests",
        "steel-webserver",
    ];

    workspace_dir.pop();

    // Could build in parallel, but we can do sequentially for now
    for dylib in dylibs {
        workspace_dir.push(dylib);

        std::process::Command::new("cargo")
            .arg("steel-lib")
            .current_dir(&workspace_dir)
            .spawn()?
            .wait()?;

        workspace_dir.pop();
    }

    println!("Finished.");

    Ok(())
}

fn install_cogs() -> Result<(), Box<dyn Error>> {
    let mut workspace_dir = workspace_dir();
    workspace_dir.push("cogs");

    std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("install.scm")
        .current_dir(workspace_dir)
        .spawn()?
        .wait()?;

    Ok(())
}

fn run_tests() -> Result<(), Box<dyn Error>> {
    std::process::Command::new("cargo")
        .arg("test")
        .arg("--all")
        .spawn()?
        .wait()?;

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let task = std::env::args().nth(1);
    match task {
        None => {}
        Some(t) => match t.as_str() {
            "install" => install_everything()?,
            "cogs" => install_cogs()?,
            "docgen" => generate_docs()?,
            "test" => run_tests()?,
            invalid => return Err(format!("Invalid task name: {}", invalid).into()),
        },
    };
    Ok(())
}
