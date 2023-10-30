use std::error::Error;

use clap::Parser;
use steel_interpreter::Args;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    // let mut builder = env_logger::Builder::new();

    // let log_targets = [
    // "requires",
    // "modules",
    // "gc",
    // "kernel",
    // "super-instructions",
    // "lambda-lifting",
    // "dylibs",
    // ];

    // for target in log_targets {
    // builder.filter(Some(target), log::LevelFilter::Trace);
    // }

    // builder.init();

    let clap_args = Args::parse();

    steel_interpreter::run(clap_args)?;

    Ok(())
}
