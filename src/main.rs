use std::error::Error;

use clap::Parser;
use steel_interpreter::SteelCliArgs;

#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let clap_args = SteelCliArgs::parse();
    steel_interpreter::run(clap_args)?;
    Ok(())
}
