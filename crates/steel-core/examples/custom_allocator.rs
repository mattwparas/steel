//! Demonstrates running a compiled Steel program under a caller-supplied allocator, so it
//! can execute on a hard-real-time thread (e.g. an audio callback) without ever touching the
//! global allocator. See ALLOCATOR_SPEC.md for the full design.
//!
//! Run with:
//!
//!   cargo run -p steel-core --example custom_allocator --no-default-features \
//!       --features std,modules,sync,biased,allocator-api2
//!
//! (`jit2` and `allocator-api2` can't be enabled together -- see the `compile_error!` in
//! `lib.rs` -- so this example is only meaningful with `jit2` left off.)

extern crate steel;

use std::alloc::Layout;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use allocator_api2::alloc::{AllocError, Allocator};
use steel::rvals::SteelValGeneric;
use steel::steel_vm::engine::Engine;

/// A fixed-capacity bump allocator: hands out memory by advancing a pointer through a
/// pre-allocated buffer and never actually frees individual allocations. This is the shape of
/// allocator a real-time audio callback wants -- no syscalls, no locks beyond a single atomic
/// compare-exchange, and a hard, predictable capacity instead of unbounded growth.
#[derive(Clone)]
struct BumpAllocator {
    inner: Arc<BumpInner>,
}

struct BumpInner {
    start: NonNull<u8>,
    capacity: usize,
    offset: AtomicUsize,
    // Owns the backing memory; never read directly, just keeps it alive.
    _buffer: Box<[u8]>,
}

// The raw pointer in `BumpInner` only ever points inside `_buffer`, which is owned
// exclusively by this struct, so sharing it across threads is sound.
unsafe impl Send for BumpInner {}
unsafe impl Sync for BumpInner {}

impl BumpAllocator {
    fn new(capacity: usize) -> Self {
        let mut buffer = vec![0u8; capacity].into_boxed_slice();
        let start = NonNull::new(buffer.as_mut_ptr()).unwrap();

        BumpAllocator {
            inner: Arc::new(BumpInner {
                start,
                capacity,
                offset: AtomicUsize::new(0),
                _buffer: buffer,
            }),
        }
    }

    /// Bytes handed out so far -- proof that the program is actually allocating through this
    /// allocator rather than falling back to the global one.
    fn bytes_used(&self) -> usize {
        self.inner.offset.load(Ordering::Relaxed)
    }
}

unsafe impl Allocator for BumpAllocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        loop {
            let current = self.inner.offset.load(Ordering::Relaxed);
            let aligned = (current + layout.align() - 1) & !(layout.align() - 1);
            let next = aligned.checked_add(layout.size()).ok_or(AllocError)?;

            if next > self.inner.capacity {
                return Err(AllocError);
            }

            if self
                .inner
                .offset
                .compare_exchange_weak(current, next, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                let ptr = unsafe { self.inner.start.as_ptr().add(aligned) };
                let slice = std::ptr::slice_from_raw_parts_mut(ptr, layout.size());
                return Ok(unsafe { NonNull::new_unchecked(slice) });
            }
        }
    }

    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // A bump allocator never frees individual allocations -- the whole arena is reclaimed
        // at once when the last `Arc<BumpInner>` (and thus the buffer) is dropped.
    }
}

const SCRIPT: &str = r#"
    (define history (box '()))
    (define event-count (box 0))

    (define (clamp value low high)
      (if (<= value low)
          low
          (if (<= high value)
              high
              value)))

    ;; A toy "MIDI mapping": note-on messages (status 144) add the two data
    ;; bytes together and clamp to the MIDI range; anything else subtracts
    ;; them. Every call records the incoming velocity into `history` and
    ;; bumps a running counter, both via boxes -- mutated captured state,
    ;; exactly like a real mapping script would keep around.
    (define (on-midi-in status data1 data2)
      (set-box! history (cons data2 (unbox history)))
      (set-box! event-count (+ 1 (unbox event-count)))
      (if (= status 144)
          (clamp (+ data1 data2) 0 127)
          (clamp (- data1 data2) 0 127)))

    (define (get-event-count) (unbox event-count))
"#;

fn main() -> steel::rvals::Result<()> {
    // Compile and register as usual, off the real-time thread, using the ordinary global
    // allocator -- the compiler, module registry, and dylib loader always stay Global-only
    // regardless of what `A` the runtime thread ends up using (see ALLOCATOR_SPEC.md).
    //
    // Deliberately built with `Engine::new_raw()` + just the native primitive registrations,
    // not the full `Engine::new()` REPL environment: `new_engine_with_allocator` deep-converts
    // every binding already present into the new allocator (see `clone_globals_from`), and the
    // full prelude/kernel/contract machinery pulls in a lot of environment that a real-time
    // thread has no business sharing anyway. A lean, explicit set of primitives is both what
    // you'd want for this kind of hand-off and the best-covered path today.
    let mut compiler_engine = Engine::new_base();

    let raw_program = compiler_engine.emit_raw_program_no_path(SCRIPT)?;
    let executable = compiler_engine.raw_program_to_executable(raw_program)?;

    // Hand off a fresh execution context, sharing the compiler/module registry, but backed by
    // our real-time-safe allocator instead of the global one.
    let bump = BumpAllocator::new(1 << 20);
    let mut realtime_engine = compiler_engine.new_engine_with_allocator(bump.clone())?;

    // Run the compiled bytecode inside the new engine: this is where `on-midi-in`, `history`,
    // and every closure/box/cons cell it needs get constructed for the first time, all
    // through `bump` rather than the global allocator.
    realtime_engine.run_executable(&executable)?;

    println!("bytes allocated after setup: {}", bump.bytes_used());

    // Simulate a small stream of MIDI messages arriving on the "audio thread". Each call
    // allocates only through `bump`: the two integer arguments are plain `IntV`s (no
    // allocation at all), and `cons`ing onto `history` inside the script allocates a new pair
    // through the allocator we generalized this same way.
    let messages = [(144u8, 60, 90), (144, 64, 40), (128, 60, 0), (144, 67, 110)];

    for (status, data1, data2) in messages {
        let args = vec![
            SteelValGeneric::IntV(status as isize),
            SteelValGeneric::IntV(data1),
            SteelValGeneric::IntV(data2),
        ];

        let result = realtime_engine.call_function_by_name_with_args("on-midi-in", args)?;

        println!("on-midi-in({status}, {data1}, {data2}) => {result}");
    }

    println!("bytes allocated after processing events: {}", bump.bytes_used());

    // The compiler engine's own (Global-allocated) `on-midi-in` was never called, and never
    // will be -- it exists only so `realtime_engine` could share its compiled bytecode.
    let get_event_count = realtime_engine.extract_value("get-event-count")?;
    let event_count = realtime_engine.call_function_with_args(get_event_count, vec![])?;
    println!("event-count (in the real-time engine's own environment): {event_count}");

    Ok(())
}
