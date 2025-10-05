//! Custom fallback for `getrandom` when the `std` feature is disabled.
//!
//! The implementation below provides a deterministic, non-cryptographic source of
//! bytes so code that depends on `getrandom` (e.g. tests or hashing seeds) keeps
//! working in `no_std` builds without relying on the `wasm_js` backend.

use core::slice;
use getrandom::Error;

#[inline]
fn stir(mut state: u64) -> u64 {
    // xorshift64*; good enough for seeding hashers in no_std builds.
    state ^= state >> 12;
    state ^= state << 25;
    state ^= state >> 27;
    state.wrapping_mul(0x2545_F491_4F6C_DD1D)
}

fn fill_buffer(buf: &mut [u8], mut state: u64) {
    for byte in buf.iter_mut() {
        state = stir(state);
        *byte = (state >> 56) as u8;
    }
}

#[no_mangle]
pub unsafe extern "Rust" fn __getrandom_v03_custom(dest: *mut u8, len: usize) -> Result<(), Error> {
    if len == 0 {
        return Ok(());
    }

    if dest.is_null() {
        return Err(Error::UNSUPPORTED);
    }

    let buf = unsafe { slice::from_raw_parts_mut(dest, len) };
    let base = dest as usize as u64;
    let seed = 0xA076_1D64_78BD_642F ^ base.rotate_left(17) ^ (len as u64);
    fill_buffer(buf, seed);
    Ok(())
}
