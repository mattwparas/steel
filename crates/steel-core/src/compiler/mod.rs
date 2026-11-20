// pub mod code_generator;
#[allow(clippy::module_inception)]
pub mod compiler;
pub mod constants;
pub mod map;
pub(crate) mod module_cache;
pub mod modules;
pub mod passes;
pub mod program;

pub mod code_gen;
