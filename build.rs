fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "build-info")]
    {
        use vergen::{BuildBuilder, CargoBuilder, Emitter, RustcBuilder};
        let build = BuildBuilder::all_build()?;
        let cargo = CargoBuilder::all_cargo()?;
        let rustc = RustcBuilder::all_rustc()?;

        Emitter::default()
            .add_instructions(&build)?
            .add_instructions(&cargo)?
            .add_instructions(&rustc)?
            .emit()?;
    }
    Ok(())
}
