use crate::util::run_in_workspace;
use std::error::Error;

pub fn no_std_wasm_test(extra_args: &[String]) -> Result<(), Box<dyn Error>> {
    println!("Running no_std tests for target wasm32 (xtask)");

    let mut args: Vec<String> = [
        "+nightly",
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
    ]
    .into_iter()
    .map(|s| s.to_string())
    .collect();

    if !extra_args.is_empty() {
        args.push("--".to_string());
        args.extend(extra_args.iter().cloned());
    }

    let status = run_in_workspace("cargo", args)?;

    if !status.success() {
        return Err("no_std tests (wasm32) failed".into());
    }

    Ok(())
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
