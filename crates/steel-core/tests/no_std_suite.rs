#![cfg(all(feature = "no_std_core", test))]
#![no_std]
#![no_main]

#![feature(custom_test_frameworks)]
#![reexport_test_harness_main = "test_main"]
#![test_runner(test_runner)]

extern crate alloc;

mod portable;

use portable::{
    arithmetic_smoke_test, redefinition_of_functions_for_constant_evaluation,
    redefinition_of_values_over_time, top_level_error_allows_redefining,
};

#[cfg(target_arch = "wasm32")]
mod alloc_support {
    use core::alloc::{GlobalAlloc, Layout};
    use core::sync::atomic::{AtomicUsize, Ordering};

    pub struct BumpAllocator;

    const HEAP_SIZE: usize = 1024 * 1024; // 1 MiB scratch space for tests.
    static mut HEAP: [u8; HEAP_SIZE] = [0; HEAP_SIZE];
    static NEXT: AtomicUsize = AtomicUsize::new(0);

    unsafe impl GlobalAlloc for BumpAllocator {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            let align_mask = layout.align().saturating_sub(1);
            let size = layout.size();
            let mut current = NEXT.load(Ordering::Relaxed);

            loop {
                let aligned = (current + align_mask) & !align_mask;
                let new_next = match aligned.checked_add(size) {
                    Some(value) => value,
                    None => return core::ptr::null_mut(),
                };

                if new_next > HEAP_SIZE {
                    return core::ptr::null_mut();
                }

                match NEXT.compare_exchange(current, new_next, Ordering::SeqCst, Ordering::Relaxed) {
                    Ok(_) => unsafe { return HEAP.as_mut_ptr().add(aligned) },
                    Err(previous) => current = previous,
                }
            }
        }

        unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
            // bump allocator: leak allocations for the duration of the test run
        }
    }

    #[global_allocator]
    pub static ALLOCATOR: BumpAllocator = BumpAllocator;
}

/// Entry point invoked by the custom runner (see `scripts/no_std_runner.js`).
#[no_mangle]
pub extern "C" fn run() -> i32 {
    test_main();
    0
}

fn test_runner(tests: &[&dyn Fn()]) {
    for test in tests {
        test();
    }
}

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo<'_>) -> ! {
    // Propagate panic as a trap so the runner sees a non-zero exit.
    #[cfg(target_arch = "wasm32")]
    core::arch::wasm32::unreachable();

    #[cfg(not(target_arch = "wasm32"))]
    loop {}
}

#[test_case]
fn check_top_level_error_allows_redefining() {
    top_level_error_allows_redefining();
}

#[test_case]
fn check_redefinition_of_values() {
    redefinition_of_values_over_time();
}

#[test_case]
fn check_redefinition_of_functions_for_constant_evaluation() {
    redefinition_of_functions_for_constant_evaluation();
}

#[test_case]
fn check_arithmetic_smoke_test() {
    arithmetic_smoke_test();
}
