#![cfg_attr(all(feature = "no_std_core", test), no_std)]
#![cfg_attr(all(feature = "no_std_core", test), no_main)]

#[cfg(all(feature = "no_std_core", test))]
extern crate alloc;
#[cfg(all(feature = "no_std_core", test))]
use alloc::vec;
#[cfg(all(feature = "no_std_core", test))]
use core::ptr::null;

#[cfg(all(feature = "no_std_core", test))]
use steel::core::instructions::{disassemble, u24, Instruction};
#[cfg(all(feature = "no_std_core", test))]
use steel::core::labels::fresh as fresh_label;
#[cfg(all(feature = "no_std_core", test))]
use steel::core::opcode::OpCode;

#[cfg(all(feature = "no_std_core", target_arch = "wasm32", test))]
#[allow(static_mut_refs)]
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

                match NEXT.compare_exchange(current, new_next, Ordering::SeqCst, Ordering::Relaxed)
                {
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

#[cfg(all(feature = "no_std_core", not(target_arch = "wasm32"), test))]
#[allow(static_mut_refs)]
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

                match NEXT.compare_exchange(current, new_next, Ordering::SeqCst, Ordering::Relaxed)
                {
                    Ok(_) => unsafe { return HEAP.as_mut_ptr().add(aligned) },
                    Err(previous) => current = previous,
                }
            }
        }

        unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
            // bump allocator: leak allocations for the duration of the test build
        }
    }

    #[global_allocator]
    pub static ALLOCATOR: BumpAllocator = BumpAllocator;
}

/// Entry point invoked by the custom runner (see `scripts/no_std_runner.js`).
/// Runs all no_std tests in sequence; on panic (trap), the process aborts.
#[cfg(all(feature = "no_std_core", test))]
#[no_mangle]
pub extern "C" fn run() -> i32 {
    for &(_, f) in named() {
        f();
    }
    0
}

#[cfg(all(feature = "no_std_core", test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo<'_>) -> ! {
    // Propagate panic as a trap so the runner sees a non-zero exit.
    #[cfg(target_arch = "wasm32")]
    core::arch::wasm32::unreachable();

    #[cfg(not(target_arch = "wasm32"))]
    loop {}
}

#[cfg(all(feature = "no_std_core", test))]
fn u24_roundtrip() {
    let values = [0u32, 1, 255, 256, 65_535, 1 << 20, (1 << 24) - 1];
    for &n in &values {
        let x = u24::from_u32(n);
        assert_eq!(x.to_u32(), n);
    }
}

#[cfg(all(feature = "no_std_core", test))]
fn disassemble_contains_opcode() {
    let instrs = vec![Instruction::new_from_parts(OpCode::ADD, u24::from_u32(2), None)];
    let text = disassemble(&instrs);
    assert!(text.contains("ADD"));
    assert!(text.contains("2"));
}

#[cfg(all(feature = "no_std_core", test))]
fn labels_are_unique_and_increasing() {
    let a = fresh_label();
    let b = fresh_label();
    assert_ne!(a, b);
}

// ---- Named test API for per-test execution ----

#[cfg(all(feature = "no_std_core", test))]
type TestFn = fn();

#[inline(always)]
#[cfg(all(feature = "no_std_core", test))]
fn named() -> &'static [(&'static str, TestFn)] {
    &[
        ("u24_roundtrip", u24_roundtrip as TestFn),
        (
            "disassemble_contains_opcode",
            disassemble_contains_opcode as TestFn,
        ),
        (
            "labels_are_unique_and_increasing",
            labels_are_unique_and_increasing as TestFn,
        ),
    ]
}

#[cfg(all(feature = "no_std_core", test))]
#[no_mangle]
pub extern "C" fn test_count() -> i32 {
    named().len() as i32
}

#[cfg(all(feature = "no_std_core", test))]
#[no_mangle]
pub extern "C" fn test_name_ptr(i: i32) -> *const u8 {
    let i = i as usize;
    let table = named();
    if i >= table.len() { return null(); }
    table[i].0.as_ptr()
}

#[cfg(all(feature = "no_std_core", test))]
#[no_mangle]
pub extern "C" fn test_name_len(i: i32) -> i32 {
    let i = i as usize;
    let table = named();
    if i >= table.len() { return 0; }
    table[i].0.len() as i32
}

#[cfg(all(feature = "no_std_core", test))]
#[no_mangle]
pub extern "C" fn test_run_index(i: i32) -> i32 {
    let i = i as usize;
    let table = named();
    let f = table[i].1;
    f();
    0
}

// For host builds (without `no_std_core`), provide a dummy main so this
// harness=false test compiles and is ignored by nextest.
#[cfg(not(all(feature = "no_std_core", test)))]
fn main() {}
