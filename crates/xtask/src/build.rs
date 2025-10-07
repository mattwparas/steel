use std::error::Error;
use crate::util::run_in_workspace;

pub fn wasm_build() -> Result<(), Box<dyn Error>> {
    println!("Building no_std targets for wasm32 (xtask)");

    // steel-derive (wasm32)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-derive",
            "--target",
            "wasm32-unknown-unknown",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std",
        ],
    )?;
    if !status.success() {
        return Err("steel-derive wasm build failed".into());
    }

    // cargo-steel-lib (wasm32)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "cargo-steel-lib",
            "--target",
            "wasm32-unknown-unknown",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
        ],
    )?;
    if !status.success() {
        return Err("cargo-steel-lib wasm build failed".into());
    }

    // steel-gen (wasm32)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-gen",
            "--target",
            "wasm32-unknown-unknown",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
        ],
    )?;
    if !status.success() {
        return Err("steel-gen wasm build failed".into());
    }

    // steel-parser (wasm32) - allow failure to match workflow's continue-on-error
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-parser",
            "--target",
            "wasm32-unknown-unknown",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std",
        ],
    )?;
    if !status.success() {
        eprintln!("warning: steel-parser wasm build failed (continuing)");
    }

    // steel-core (wasm32)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-core",
            "--target",
            "wasm32-unknown-unknown",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std_core",
        ],
    )?;
    if !status.success() {
        return Err("steel-core wasm build failed".into());
    }

    Ok(())
}

pub fn thumb_build() -> Result<(), Box<dyn Error>> {
    println!("Building no_std targets for thumbv7em (xtask)");

    // steel-derive (thumbv7em)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-derive",
            "--target",
            "thumbv7em-none-eabihf",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std",
        ],
    )?;
    if !status.success() {
        return Err("steel-derive thumb build failed".into());
    }

    // cargo-steel-lib (thumbv7em)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "cargo-steel-lib",
            "--target",
            "thumbv7em-none-eabihf",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
        ],
    )?;
    if !status.success() {
        return Err("cargo-steel-lib thumb build failed".into());
    }

    // steel-gen (thumbv7em)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-gen",
            "--target",
            "thumbv7em-none-eabihf",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
        ],
    )?;
    if !status.success() {
        return Err("steel-gen thumb build failed".into());
    }

    // steel-parser (thumbv7em) - allow failure to match workflow's continue-on-error
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-parser",
            "--target",
            "thumbv7em-none-eabihf",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std",
        ],
    )?;
    if !status.success() {
        eprintln!("warning: steel-parser thumb build failed (continuing)");
    }

    // steel-core (thumbv7em)
    let status = run_in_workspace(
        "cargo",
        [
            "+nightly",
            "--config",
            ".cargo/no_std.toml",
            "build",
            "-p",
            "steel-core",
            "--target",
            "thumbv7em-none-eabihf",
            "-Zbuild-std=core,alloc",
            "--no-default-features",
            "--features",
            "no_std_core",
        ],
    )?;
    if !status.success() {
        return Err("steel-core thumb build failed".into());
    }

    Ok(())
}
