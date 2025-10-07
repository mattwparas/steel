use std::error::Error;

mod build;
mod util;

use crate::util::workspace_dir;

// Generate documentation by invoking the docs script!
fn generate_docs() -> Result<(), Box<dyn Error>> {
    println!("Generating docs...");

    let workspace_dir = workspace_dir();

    let steel_doc_dir = workspace_dir.join("crates/steel-doc");
    std::process::Command::new("cargo")
        .arg("run")
        .current_dir(&steel_doc_dir)
        .spawn()?
        .wait()?;

    println!("Cleaning target directory...");

    let docs = workspace_dir.join("docs/src");
    std::fs::remove_dir_all(&docs.join("builtins"))?;
    std::fs::remove_dir_all(&docs.join("stdlib"))?;

    println!("Moving generated docs into place...");

    let generated = steel_doc_dir.join("generated");
    std::fs::rename(generated.join("builtins"), docs.join("builtins"))?;
    std::fs::rename(generated.join("stdlib"), docs.join("stdlib"))?;

    println!("Done!");

    Ok(())
}

fn install_everything() -> Result<(), Box<dyn Error>> {
    println!("Installing `steel`...");

    let mut workspace_dir = workspace_dir();

    // Check if cargo pgo is installed
    if which::which("cargo-pgo").is_ok() {
        println!("`cargo-pgo` found - building with PGO");
        install_pgo()?;
    } else {
        std::process::Command::new("cargo")
            .arg("install")
            .arg("--path")
            .arg(".")
            .arg("--force")
            .spawn()?
            .wait()?;
    }

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

    println!("Installing `forge`");

    workspace_dir.pop();
    workspace_dir.push("forge");

    std::process::Command::new("cargo")
        .arg("install")
        .arg("--path")
        .arg(&workspace_dir)
        .arg("--force")
        .spawn()?
        .wait()?;

    install_cogs()?;

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

fn install_pgo() -> Result<(), Box<dyn Error>> {
    std::process::Command::new("cargo")
        .arg("pgo")
        .arg("build")
        .spawn()?
        .wait()?;

    let binary = format!("target/{}/release/steel", env!("TARGET_PLATFORM"));

    let benches = &[
        "r7rs-benchmarks/scheme.scm",
        "r7rs-benchmarks/simplex.scm",
        "r7rs-benchmarks/array1.scm",
        "r7rs-benchmarks/triangl.scm",
        "benchmarks/bin-trees/bin-trees.scm",
        "benchmarks/fib/fib.scm",
    ];

    for _ in 0..3 {
        for bench in benches {
            std::process::Command::new(&binary)
                .arg(bench)
                .spawn()?
                .wait()?;
        }
    }

    std::process::Command::new("cargo")
        .arg("pgo")
        .arg("optimize")
        .spawn()?
        .wait()?;

    let mut cargo_bin_location = which::which("cargo").expect("Unable to find cargo");
    cargo_bin_location.pop();
    cargo_bin_location.push("steel");
    println!("Installing to: {:?}", cargo_bin_location);
    std::fs::copy(binary, cargo_bin_location).unwrap();

    Ok(())
}

// build-specific tasks live in `build.rs`

fn print_usage() {
    eprintln!("xtask usage:");
    eprintln!("  cargo xtask install");
    eprintln!("  cargo xtask cogs");
    eprintln!("  cargo xtask docgen");
    eprintln!("  cargo xtask test");
    eprintln!("  cargo xtask pgo");
    eprintln!("  cargo xtask build <target>");
    eprintln!("\nTargets:");
    eprintln!("  wasm   Build no_std set for wasm32-unknown-unknown");
    eprintln!("  thumb  Build no_std set for thumbv7em-none-eabihf");
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    match args.next().as_deref() {
        None => {
            print_usage();
        }
        Some("install") => install_everything()?,
        Some("cogs") => install_cogs()?,
        Some("docgen") => generate_docs()?,
        Some("test") => run_tests()?,
        Some("pgo") => install_pgo()?,
        // new structured CLI: cargo xtask build <target>
        Some("build") => match args.next().as_deref() {
            Some("wasm") => build::wasm_build()?,
            Some("thumb") => build::thumb_build()?,
            Some(other) => return Err(format!("Unknown build target: {}", other).into()),
            None => {
                eprintln!("missing target. try: wasm | thumb");
                print_usage();
                std::process::exit(2);
            }
        },
        Some(invalid) => return Err(format!("Invalid task name: {}", invalid).into()),
    }
    Ok(())
}
