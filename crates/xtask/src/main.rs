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

fn main() -> Result<(), Box<dyn Error>> {
    let task = std::env::args().nth(1);
    match task {
        None => {}
        Some(t) => match t.as_str() {
            "docgen" => generate_docs()?,
            // "themelint" => tasks::themelint(env::args().nth(2))?,
            // "query-check" => tasks::querycheck()?,
            invalid => return Err(format!("Invalid task name: {}", invalid).into()),
        },
    };
    Ok(())
}
