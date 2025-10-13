use crate::util::{run_in_workspace, workspace_dir};
use std::error::Error;
use std::path::PathBuf;
use std::process::Command;
use std::{fs, io};

pub fn no_std_wasm_test(extra_runner_args: &[String]) -> Result<(), Box<dyn Error>> {
    println!("Running no_std tests for target wasm32 (xtask, Rust harness)");

    // 1) Compile the wasm test (no-run), keeping panic abort flags from .cargo/no_std.toml
    let compile_status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "-q",
            "--config",
            ".cargo/no_std.toml",
            "test",
            "-p",
            "steel-core",
            "--test",
            "no_std_suite",
            "--target",
            "wasm32-unknown-unknown",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std",
            "--no-run",
        ],
    )?;
    if !compile_status.success() {
        return Err("no_std tests (wasm32) build failed".into());
    }

    // 2) Attempt to discover the produced wasm artifact for the test
    let wasm_path = find_wasm_test_artifact()?;
    println!("Found wasm test artifact: {:?}", wasm_path);

    // 3) Invoke the Rust runner with optional passthrough args
    let mut cmd = Command::new("cargo");
    cmd.arg("run");
    cmd.arg("-q");
    cmd.arg("--manifest-path");
    cmd.arg("crates/test-runner/Cargo.toml");
    cmd.arg("--");
    cmd.arg(&wasm_path);
    for arg in extra_runner_args {
        cmd.arg(arg);
    }
    let status = cmd.current_dir(workspace_dir()).status()?;
    if !status.success() {
        return Err("no_std wasm tests failed".into());
    }

    Ok(())
}

fn find_wasm_test_artifact() -> Result<PathBuf, Box<dyn Error>> {
    // Prefer parsing the cargo JSON output if present (artifact discovery);
    // as a simple robust fallback, scan the deps dir for the newest no_std_suite*.wasm
    let target_dir = workspace_dir()
        .join("target")
        .join("wasm32-unknown-unknown")
        .join("debug")
        .join("deps");
    let mut newest: Option<(PathBuf, std::time::SystemTime)> = None;
    let read_dir = fs::read_dir(&target_dir)
        .map_err(|e| io::Error::new(e.kind(), format!("reading {:?}: {}", target_dir, e)))?;
    for entry in read_dir.flatten() {
        let path = entry.path();
        if let (Some(name), Some(ext)) = (
            path.file_name().and_then(|s| s.to_str()),
            path.extension().and_then(|s| s.to_str()),
        ) {
            if ext == "wasm" && name.starts_with("no_std_suite-") {
                let meta = fs::metadata(&path)?;
                let mtime = meta.modified().unwrap_or(std::time::SystemTime::UNIX_EPOCH);
                if newest.as_ref().map(|(_, t)| mtime > *t).unwrap_or(true) {
                    newest = Some((path.clone(), mtime));
                }
            }
        }
    }
    if let Some((path, _)) = newest { return Ok(path); }
    Err("could not locate no_std_suite wasm artifact in target".into())
}

pub fn no_std_thumb_test() -> Result<(), Box<dyn Error>> {
    println!("Compiling no_std tests for target thumbv7em (xtask, compile-only)");

    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "test",
            "-p",
            "steel-core",
            "--test",
            "no_std_suite",
            "--target",
            "thumbv7em-none-eabihf",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std",
            "--no-run",
        ],
    )?;

    if !status.success() {
        return Err("no_std tests (thumbv7em) build failed".into());
    }

    println!("Note: running on thumb requires a hardware/emulator runner; this step only compiles.");
    Ok(())
}
