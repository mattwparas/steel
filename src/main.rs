use std::error::Error;

use clap::Parser;
use steel_interpreter::SteelCliArgs;

#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[cfg(feature = "snmalloc")]
#[global_allocator]
static GLOBAL: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    // Later runs start from the boot image this one keeps
    steel::steel_vm::engine::Engine::enable_boot_image_cache(None);
    let clap_args = SteelCliArgs::parse();
    let result = steel_interpreter::run(clap_args);
    if steel::deopt_census_enabled() {
        steel::dump_deopt_census();
    }
    result?;
    Ok(())
}
