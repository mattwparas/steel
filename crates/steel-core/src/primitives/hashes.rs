use md5::{Digest, Md5};

use crate::gc::ShareableMut;
use crate::steel_vm::builtin::BuiltInModule;

use crate::rvals::{AsRefMutSteelVal, Custom, IntoSteelVal, Result, SteelByteVector, SteelVal};
use crate::stop;

struct Md5Hasher(Md5);

impl Custom for Md5Hasher {}

pub fn hashes_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/hashes");

    module
        .register_native_fn_definition(MD5_HASHER_DEFINITION)
        .register_native_fn_definition(MD5_HASHER_TO_BYTES_DEFINITION)
        .register_native_fn_definition(MD5_HASHER_UPDATE_DEFINITION);

    module
}

#[steel_derive::function(name = "md5-hasher")]
pub fn md5_hasher() -> SteelVal {
    Md5Hasher(Md5::new()).into_steelval().unwrap()
}

#[steel_derive::function(name = "md5-hasher-update!")]
pub fn md5_hasher_update(hasher: &SteelVal, value: &SteelVal) -> Result<SteelVal> {
    let mut hasher = Md5Hasher::as_mut_ref(hasher)?;

    match value {
        SteelVal::ByteVector(b) => {
            hasher.0.update(b.vec.read().as_slice());
        }
        SteelVal::StringV(b) => {
            hasher.0.update(b.as_bytes());
        }
        _ => {
            stop!(Generic =>
                "md5-hasher-update! requires a hashable value (strings or bytevector), found: {}",
                value
            )
        }
    }

    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "md5-hasher->bytes")]
pub fn md5_hasher_to_bytes(value: &SteelVal) -> Result<SteelVal> {
    let hash = Md5Hasher::as_mut_ref(value)?.0.finalize_reset();
    Ok(SteelVal::ByteVector(SteelByteVector::new(hash.to_vec())))
}
