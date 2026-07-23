# Spec: a single, compile-time-generic allocator for all of Steel

Branch: `feat_allocator_generic`, forked fresh from `master` (no code changes yet — this
document only).

## 1. Goal

Every heap allocation reachable while running an already-compiled Steel program goes
through **one** caller-supplied allocator (`allocator_api2::alloc::Allocator`), chosen at
**compile time** via a generic parameter — not a `dyn`/thread-local/runtime-switchable
mechanism. No value kind gets its own special-cased allocator; there is exactly one `A`
per `Engine` instantiation, and it applies uniformly.

This is in service of running Steel on a hard-realtime thread (e.g. an audio callback)
backed by a real-time-safe allocator (bump/pool/free-list, no locks, no syscalls), and,
longer-term, of `no_std` support.

Explicitly rejected: a `#[global_allocator]`-based approach (wrong tool — Steel is meant
to be embedded in a host process/plugin that already owns the process-wide allocator
decision) and the previous, narrower `feat_allocator_api` design (only routed
`SteelVal::Custom` through a settable allocator; everything else stayed on `Global`).
That branch is left as-is; this is a clean restart with the full scope.

## 2. Ground truth about the current (pristine `master`) codebase

Gathered directly from the source, not assumed:

- `SteelVal` (`rvals.rs:1636`) has 33 variants; all but a handful (`BoolV`, `NumV`,
  `IntV`, `Rational`, `CharV`, `Void`, `FuncV`, `MutFunc`, `BuiltIn`, `Complex`) own
  heap data, always through one layer of `Gc<T>` (or a raw `HeapRef<T>`/weak handle).
  Full variant → backing-type table is in the appendix.
- Hard budget: `assert!(size_of::<SteelVal>() <= 16)` (`rvals.rs:2181`), checked against
  exactly 16 by a test. This must keep holding for every variant regardless of which `A`
  the engine is instantiated with.
- `Gc<T>` (`gc.rs:455`) is currently `pub struct Gc<T: ?Sized + 'static>(Shared<T>)` — a
  single parameter, no allocator hook. `Shared<T>` resolves via features to one of four
  distinct concrete types: `Rc<T>` (default), `Arc<T>` (`sync`), `triomphe::Arc<T>`
  (`sync+triomphe`), or `steel_rc::BiasedRc<T>` (`sync+biased`).
- **Only `BiasedRc` can be made allocator-generic on stable Rust.** `Rc`/`Arc` gained
  allocator parameters only behind the unstable `allocator_api` nightly feature;
  `allocator-api2` (the stable-Rust mirror we're using) deliberately does not attempt to
  replicate `Rc`/`Arc`, only `Box`/`Vec`. `triomphe::Arc` also has no allocator hook.
  → **the allocator-generic build requires the `biased` feature; the `Rc`/`Arc`/
  `triomphe` backing choices are not addressable by this work.**
- 275 call sites use `Gc::new(`, 7 use `Gc::new_mut(`, across `crates/steel-core/src`.
- Strings: `SteelVal::StringV`/`SymbolV` both wrap `SteelString = Gc<String>`
  (`rvals.rs:2095`). `std::String` has no allocator parameter on stable — this needs a
  small hand-rolled replacement (a UTF-8-checked wrapper over
  `allocator_api2::vec::Vec<u8, A>`).
- Persistent collections: default build uses `im-rc` (`Rc`-based, single-threaded);
  `sync` swaps to `im` (`Arc`-based). Both are third-party, hard-code their own
  `Rc`/`Arc` internally, **no allocator hook of any kind.** A third option already exists
  behind the `imbl` feature (`steel-imbl` crate + `im-lists`), which is generic over a
  `PointerFamily` trait that this codebase already implements using `Gc<T>`
  (`values/mod.rs:60-104`) — this is the one existing extension point that could carry an
  `A` through, *if* the chunk/node arrays inside `imbl`/`im-lists` themselves allocate via
  the pointer family rather than a hardcoded `Vec`. **Unverified — needs a spike (Phase
  0) before the plan below can be trusted.**
- Operand stack: `SteelThread.stack: Vec<SteelVal>` (`vm.rs:392`), allocated once
  (`Vec::with_capacity(128)`) and reused for the life of the thread via `truncate` at
  call/return boundaries — good, this is exactly the shape a bump/pool allocator wants
  (no per-call alloc/dealloc churn once warmed up).
- Closures: `ByteCodeLambda` (`functions.rs:111`) captures via `CaptureVec`, which is
  `Vec<SteelVal>` by default or `SmallVec<[SteelVal; 3]>` under the `inline-captures`
  feature (`functions.rs:101-108` — note: not a feature literally named `smallvec`; that
  name exists in `Cargo.toml` but is an empty, no-op flag, since the `smallvec` crate
  itself is a hard, non-optional dependency used unconditionally throughout the
  compiler/parser). `smallvec`'s spill-to-heap path has no allocator hook, and two more
  unconditional (non-feature-gated) sites hit it on the hot path too: the builtin-call
  argument buffer (`vm.rs:4852-4856`, `vm.rs:5261-5265`) and `UserDefinedStruct.fields`
  (`values/structs.rs:163`, pooled via a `Recycle<T>` allocator-side-stepping thread-local
  reuse cache — `values/recycler.rs`).
- `Env` (`env.rs:43`) is `Vec<SteelVal>` in the default build, or (`sync`)
  `SharedVectorWrapper` over the third-party `shared_vector::AtomicSharedVector` — also
  no allocator hook.
- Numeric towers: `BigNum`/`BigRational`/`Rational32` are backed by `num-bigint`/
  `num-rational`. `num-bigint`'s `BigInt` stores its digits in a plain `Vec<u32>` — no
  allocator hook.

## 3. Design decisions

### 3.1 One generic parameter, threaded everywhere

`A: Allocator + Clone + 'static` (`+ Send + Sync` additionally required wherever `sync`
is active, mirroring `BiasedRc`'s existing bound structure). No `dyn`, no thread-local,
no per-variant special case. Every heap-owning `SteelVal` variant, `Env`, `ByteCodeLambda`,
the operand stack, and the persistent collections carry the same `A`.

`Engine<A: Allocator + Clone + 'static = Global>` — the default type parameter means
every existing caller (`Engine::new()`, `register_fn`, etc.) keeps compiling unchanged,
monomorphized to `Engine<Global>`. Opting into a custom allocator is `Engine::<MyAlloc>::new_in(alloc)`
or similar, not a change to the default path.

### 3.2 `Gc<T, A>`: allocator lives in the heap block, not the handle

Already validated on the previous branch and worth carrying forward as-is: `RcBox<T, A>`
stores `alloc: A` in the heap-allocated block; `BiasedRc<T, A>` itself stays exactly
pointer-sized (`ptr: NonNull<RcBox<T, A>>`, no `A`-sized field on the stack side). This is
*why* `SteelVal`'s 16-byte budget survives becoming generic over `A` at all — the size of
`Gc<T, A>` (and hence every variant wrapping it) is independent of `A`, only the
allocation logic differs. Confirmed previously via
`assert_eq!(size_of::<BiasedRc<i32, CountingAllocator>>(), size_of::<usize>())`.

### 3.3 `SteelVal` becomes generic

Rename the enum to `SteelValGeneric<A>` (or similar internal name), and reintroduce
`SteelVal` as `pub type SteelVal = SteelValGeneric<Global>;` for source compatibility —
every existing pattern match, `impl` block, and downstream consumer of the *default*
engine keeps working untouched. Only code that opts into a non-`Global` `A` needs to
write `SteelValGeneric<MyAlloc>` explicitly.

This is the single largest mechanical phase: every `Gc::new(` call site (275+), every
struct that embeds a `SteelVal` (`ByteCodeLambda`, `Env`, `UserDefinedStruct`,
`Transducer`, `Reducer`, `LazyStream`, `Syntax`, `OpaqueReference`, `ContinuationMark`,
...) picks up an `<A>` parameter, and every free function that touches `SteelVal` either
stays non-generic (because it only ever sees `SteelVal = SteelValGeneric<Global>`, if it's
part of the non-generic-facing API) or itself becomes generic over `A`.

### 3.4 `SteelString<A> = Gc<str, A>` — no new type, no hand-rolled buffer

`std::String` can't take a custom allocator on stable Rust, and no stable-Rust,
`allocator-api2`-based `String<A>` crate exists (checked: the one real candidate,
[`string-alloc`](https://docs.rs/string-alloc/latest/string_alloc/), is built on the
unstable nightly `allocator_api`, is v0.0.3, and deliberately omits several std `String`
methods — not viable without forcing nightly on the whole `allocator-api2` feature).

Rather than hand-roll a wrapper over `allocator_api2::vec::Vec<u8, A>`, or take on that
nightly dependency, replace `SteelString = Gc<String>` with `SteelString = Gc<str, A>`
directly. `BiasedRc` already supports unsized DSTs for exactly this purpose —
`RcBox<T: ?Sized, A>` is already declared `?Sized`, and `impl<T> BiasedRc<[T]>` /
`impl From<&str> for BiasedRc<str>` / `impl From<String> for BiasedRc<str>` already exist
in `crates/steel-rc/src/lib.rs`. Extending that existing unsized support with the same
`<A>` parameter Phase 1 already adds is enough — no new type, no third-party dependency.

This is also a strict improvement over today, not just a lateral move: `Gc<String>`
today is *two* heap allocations per string — one for the `Gc`'s `RcBox`, and a second,
separate one inside `std::String`'s own byte buffer. `Gc<str, A>` is one contiguous
allocation (refcount + UTF-8 bytes together), the same trick `std::Rc<str>`/`Arc<str>`
use. Strings stay immutable at the `SteelVal` level either way (matching today: mutation
rebinds to a new `Gc`, it never mutates bytes in place), so this is a drop-in
representation change, not a semantics change.

**Explicitly set aside:** true stack/inline string storage (small strings embedded
directly in `SteelVal`'s own bytes, no heap allocation at all) was considered and isn't
part of this plan. `SteelVal`'s hard budget (`assert!(size_of::<SteelVal>() <= 16)`,
§2) applies to the whole enum, sized to its largest variant — an inline buffer large
enough to hold a useful number of characters would force every other variant to grow to
match, which is a much bigger, separate change to the interpreter's memory footprint than
this plan is scoped to make. Worth revisiting only as its own explicit decision, not as a
side effect of the string design.

### 3.5 Persistent collections: `allocator-api2` will require `imbl` — Phase 0 verified, findings below

**Phase 0 is done.** Both crates' actual published source (v0.12.1 / v7.1.0, read directly
from the local registry cache, not guessed) were inspected. The picture is more precise
than the original speculation, and different in an important way.

- **`im` / `im-rc`: excluded, unconditionally.** They hard-code `Rc`/`Arc` internally
  with no allocator extension point of any kind. `allocator-api2` together with the
  default or plain-`sync` collection backend is a `compile_error!`. Neither crate is
  patched, touched, or removed — anyone who leaves `allocator-api2` off keeps using
  `im-rc` (default) or `im` (`sync`) exactly as today.
- **`im-lists` (backs `ListV`/`Pair`): its chunk storage is *already* allocator-generic.**
  `UnrolledCell`'s element storage is `AtomicSharedVector<T, A = Global>`
  (`shared_vector/shared.rs:18`, `RefCountedVector<T, AtomicRefCount, A>`), and the
  underlying `allocate_header_buffer::<T, A>` (`shared_vector/raw.rs`) is written directly
  against `allocator_api2::alloc::Allocator` — this crate uses `allocator-api2`
  internally already, for its own reasons, unrelated to Steel. The gap is that
  `UnrolledCell`/`UnrolledList`/`GenericList`'s own generic parameter lists don't expose
  that second type parameter to callers — it's just elided to the `Global` default. Real
  work, but it's *plumbing an existing capability upward*, not writing new allocation
  logic.
- **`steel-imbl` (backs `VectorV`/`HashMapV`/`HashSetV`): its leaf/chunk storage needs no
  patch at all.** The RRB-tree/HAMT leaves (`imbl_sized_chunks::{Chunk, SparseChunk,
  InlineArray}`) are genuinely inline — `data: MaybeUninit<[A; N]>` embedded directly in
  the node struct, no separate heap pointer, nothing to route through an allocator.
- **Both crates share one real gap, and it's not the one the spec originally worried
  about.** Both define a `PointerFamily` trait (`im-lists::shared::PointerFamily`,
  `steel-imbl::shared_ptr::PointerFamily` — separate trait definitions, same shape) used
  to construct the tree/cons-cell *node* itself. Its constructor is
  `fn new<T>(value: T) -> Self::Pointer<T>` — no allocator parameter. A `GcPointerType<A>`
  implementing this can't just receive the allocator instance as an argument at the call
  site; there's no slot for it.

**Decision:** patch the `PointerFamily` trait itself in both crates to add an allocator
parameter (`fn new<T>(value: T, alloc: &Self::Alloc) -> Self::Pointer<T>`, or equivalent),
and thread it through every call site — `rrb.rs`, `hamt.rs`, `btree.rs`, the
`vector`/`hash`/`ord` modules in `steel-imbl`, and `unrolled.rs` in `im-lists`. Dozens of
call sites across two crates, more work than "just parametrize an existing hook," but it
keeps `A` a genuine per-instance value with no ambient/thread-local recovery mechanism
anywhere — consistent with every other phase of this plan. The alternative (constraining
`A` to something recoverable without a per-call argument, e.g. a `Default`-backed handle
resolving to a leaked/thread-local instance internally) was considered and rejected: it
reintroduces, for just these two collections, exactly the ambient-state pattern
compile-time generics were chosen to avoid everywhere else.

Both crates are already non-upstream forks/vendored-ish for this project (`steel-imbl` at
`github.com/mattwparas/steel-imbl`, authored by the same person who maintains this repo;
`im-lists` published directly by them too) — patching them is a fork-and-PR-later
situation, not asking an unrelated third party for a change we don't control.

### 3.6 Explicitly out of scope for now (documented exceptions, not silently ignored)

Nothing here is dropped from Steel — every one of these crates/paths keeps working
exactly as it does today for anyone not enabling `allocator-api2`; the crates behind them
have no allocator hook and aren't, on their own, worth forking given they sit off the
realtime hot path (arbitrary-precision arithmetic, cross-thread capture publishing). Each
gets its own resolution for v1, not a single blanket treatment:

- `BigNum`/`BigRational` (`num-bigint`/`num-rational`) — **disabled outright when
  `allocator-api2` is enabled**, rather than silently falling back to `Global`: arithmetic
  that would normally promote a fixnum/rational into arbitrary precision instead produces
  a runtime error under that feature, until `num-bigint`/`num-rational` (or a replacement)
  get real allocator support. This is a conscious, visible limitation, not a silent
  violation of "one allocator for everything" — fixable later, not now. Fully unaffected
  when `allocator-api2` is off — bignum/rational promotion works exactly as today. Note
  `Rational32` (backing plain `SteelVal::Rational`) is two inline `i32`s, never
  heap-allocates at all, and needs no change either way.
- `SmallVec`'s heap-spill path — resolved the same way as bignum/rational, not left as a
  silent gap: the common case (contents fit inline) stays inline, zero allocation,
  completely unaffected — no change there at all. Only the *spill* case (more elements
  than the inline capacity) is a gap, and under `allocator-api2` specifically it errors
  at runtime instead of silently falling back to `Global` — a capacity check before the
  point where `smallvec` would otherwise grow onto the heap, not a patch to `smallvec`
  itself. `smallvec` itself, and its spill path under the ordinary build, are completely
  untouched. This trades a rare, sharp error (calls/structs that exceed the inline count
  under `allocator-api2`) for keeping the invariant airtight, rather than a quiet
  violation of "one allocator for everything." Three sites, not one, all inline-capacity
  4 unless noted:
  `CaptureVec` (closure captures, gated by the `inline-captures` feature —
  `functions.rs:101-108`; *not* a feature literally named `smallvec`, which exists in
  `Cargo.toml` but is an empty, no-op flag since the `smallvec` dependency itself isn't
  optional); the builtin-call argument buffer (`vm.rs:4852-4856` and `vm.rs:5261-5265`,
  unconditional, on every builtin/`MutFunc` call — the most hot-path-central of the
  three); and `UserDefinedStruct.fields` (`values/structs.rs:163`, backing
  `SteelVal::CustomStruct`, unconditional) — though the latter is wrapped in a
  `Recycle<T>` thread-local pool (`values/recycler.rs`) that reuses previously-allocated
  buffers via `Drop`/`get()` rather than hitting the allocator fresh each time, so a
  warmed-up hot loop likely never touches `Global` there regardless.
- `shared_vector::AtomicSharedVector` (`Env`'s `sync` bindings path) — this is a genuine
  **incompatibility between `sync` and `allocator-api2` specifically**, not a removal of
  either feature on its own: `sync` alone behaves exactly as today; `allocator-api2` alone
  (without `sync`) behaves exactly as designed; only the combination of both together
  needs `Env` to fall back to a `Vec<SteelVal, A>`-based representation instead of
  `AtomicSharedVector` (enforced as a `compile_error!` on that specific combination until
  it gets its own allocator-aware replacement). Given the realtime use case is a single
  dedicated thread, this combination is unlikely to be needed soon, but it's a real,
  named gap rather than a silent one.

None of these three is a silent, quiet exception in the end — every one of them either
holds the invariant exactly or fails loudly instead of hiding a violation:
`shared_vector`+`sync`+`allocator-api2` is a hard, named incompatibility
(`compile_error!`, decided at build time). `BigNum`/`BigRational` and `SmallVec`'s spill
path are both the same shape — disabled/erroring outright rather than falling back to
`Global` — just at different times: bignum promotion is a build-time-known limitation
(no bignum arithmetic under `allocator-api2` at all, until fixed later), while
`smallvec`'s spill is a runtime check (the common, inline case is completely unaffected;
only exceeding the inline count under `allocator-api2` errors). None of the three sit on
the call path you're optimizing (`call_function_by_name_with_args_from_mut_slice` on an
already-compiled closure with no bignums/rationals involved, and argument/struct counts
that fit inline), so shipping with these three documented v1 boundaries is defensible,
not scope creep hiding as a "special internal allocator."

### 3.7 dylibs

Steel supports loading compiled dylib modules (`dylibs` feature, `cargo-steel-lib`).
`SteelValGeneric<Global>` and `SteelValGeneric<MyBumpAlloc>` are different, ABI-incompatible
monomorphizations, so a dylib built against one `A` can't load into an `Engine`
instantiated with a different `A` — same as any other generic type in Rust. Not a design
problem to solve, just a mechanical fact to know: a dylib and the engine loading it need
to agree on `A`.

## 4. Inventory: every allocating type and what it needs

Every type below is heap-allocating and sits somewhere on the path this work cares
about (directly in `SteelVal`'s representation, or in a struct `SteelVal` embeds).
Split by whether we can patch the source ourselves or have to work around it.

### 4.1 Std library types — can't patch, need an allocator-api2-compatible replacement

| Type | Where it's used | No allocator hook because | Replacement |
|---|---|---|---|
| `std::rc::Rc<T>` | `Shared<T>` in the default (non-`sync`) build | std, stable-Rust `allocator_api` is nightly-only | unaffected when `allocator-api2` is off; that feature requires `biased` to also be on (`compile_error!` otherwise), which is what actually supplies the allocator-generic `Shared<T>` (§2) — `Rc` itself isn't touched or replaced |
| `std::sync::Arc<T>` | `Shared<T>` under `sync`, no `triomphe`/`biased` | same | same — unaffected unless `allocator-api2` is enabled, which requires `biased` instead |
| `std::string::String` | `SteelString = Gc<String>` (`rvals.rs:2095`) | same | not replaced with a new type — `SteelString = Gc<str, A>` instead, reusing `BiasedRc`'s existing unsized-DST support from Phase 1 (§3.4). `std::String` itself is simply dropped from `SteelString`'s definition, not given an allocator hook |
| `std::boxed::Box<dyn Trait>` | `Custom`'s `Box<dyn CustomType>`; `FutureFunc`'s `Box<dyn Fn(...)>`; `BoxedDynFunction`; `Pin<Box<dyn Future<...>>>` for `FutureV`/`BoxedFutureResult` | same | `allocator_api2::boxed::Box<dyn Trait, A>` via the `unsize_box!` macro (already prototyped for the `Custom` path on the old branch); `Pin` needs a manual `Pin::new_unchecked` wrap since `allocator_api2::Box` has no built-in `Box<T> -> Pin<Box<T>>` conversion the way `std::boxed::Box` does |
| `std::vec::Vec<T>` | operand stack (`SteelThread.stack`); `ByteVector`'s `Vec<u8>`; `MutableVector`'s `Vec<SteelVal>`; default (non-`inline-captures`) `CaptureVec` | same | `allocator_api2::vec::Vec<T, A>` — drop-in, already used for the VM's per-call scratch buffer on the old branch |
| `std::collections::HashMap`/`HashSet` | ~77 internal call sites (symbol tables, module registries, compiler bookkeeping — mostly *not* `SteelVal`-adjacent) | same | `hashbrown::HashMap<K,V,S,A>`/`HashSet<K,S,A>` — hashbrown natively supports `allocator-api2` and is literally what `std::HashMap` is built on, so this is a clean swap. Most of the 77 sites are compiler/parser bookkeeping that never runs once a program is already compiled, so they can stay on `Global` without violating the goal (§1 only requires *runtime* allocation to route through `A`) — first pass: audit those 77 sites to find the few, if any, actually reachable from `call_function_by_name_with_args_from_mut_slice`'s hot path, and convert only those |

### 4.2 Third-party open-source crates — could be patched/forked

| Crate/type | Used for | Current allocator hook | Patch needed |
|---|---|---|---|
| `triomphe::Arc<T>` | alternate `Shared<T>` under `sync+triomphe` | none | plausible fork target — simpler than `BiasedRc` (no merge/queue machinery), genuinely easier than it looks; unaffected unless `allocator-api2` is enabled, in which case `biased` is required instead (see above), not because `triomphe` is harder to patch, just less work to reuse what already exists |
| `im-rc` (`Vector`/`HashMap`/`HashSet`) | default (non-`sync`) `VectorV`/`HashMapV`/`HashSetV` | none — hardcodes `Rc` internally, no extension point at all | fully unaffected when `allocator-api2` is off (remains the default, exactly as today); incompatible with `allocator-api2` specifically (§3.5) — enabling both is a `compile_error!`, not a deletion of the crate |
| `im` (Arc-based twin of `im-rc`) | `sync` build of same variants | none, same as `im-rc` | same — unaffected under plain `sync`; incompatible only with `sync+allocator-api2` together |
| `im-lists` (`GenericList`) | `ListV`/`Pair`-adjacent list backend | element storage (`AtomicSharedVector<T, A=Global>`) is *already* `allocator-api2`-generic internally; `PointerFamily::new<T>(value)` (for the cons-cell node itself) has no allocator-argument slot | **verified (Phase 0):** expose the existing `A` param up through `UnrolledCell`/`UnrolledList`/`GenericList` (plumbing, not new logic); patch `PointerFamily::new` to accept an allocator argument (real, multi-site work — decided over an ambient-recovery alternative, §3.5) |
| `steel-imbl` (`Generic{Vector,HashMap,HashSet}`) | `VectorV`/`HashMapV`/`HashSetV` under the `imbl` feature | leaf/chunk storage (`imbl_sized_chunks::{Chunk,SparseChunk,InlineArray}`) is genuinely inline (`MaybeUninit<[A;N]>` in the node, no separate allocation) — nothing to patch there; same `PointerFamily::new<T>(value)` gap as `im-lists` (separate trait, same shape) | **verified (Phase 0):** no patch needed for chunk storage; same `PointerFamily::new` patch as `im-lists` needed for the node pointer itself |
| `smallvec` | Three hot-path sites: `CaptureVec` under `inline-captures` (`functions.rs:101-108`); the builtin-call argument buffer, unconditional (`vm.rs:4852-4856`, `vm.rs:5261-5265`); `UserDefinedStruct.fields`, unconditional (`structs.rs:163`, pooled via `Recycle<T>`) | inline storage never allocates (fine as-is); heap-spill path has no allocator parameter in mainline `smallvec`, at any of the three sites | **resolved**: not patched, not silently accepted — the inline fast path is untouched at all three sites; only the spill case errors at runtime under `allocator-api2` (a capacity check ahead of the point `smallvec` would otherwise grow), mirroring how bignum/rational is handled. No patch to `smallvec`, no change at all when `allocator-api2` is off |
| `shared_vector` (`AtomicSharedVector`) | `Env`'s `sync` bindings path (`SharedVectorWrapper`) | none found | unaffected under plain `sync`; incompatible specifically with `sync+allocator-api2` together (§3.6) until `Env` gets its own allocator-aware bindings for that combination |
| `num-bigint` (`BigInt`/`BigUint`) | `SteelVal::BigNum` | none — digit storage is a plain `Vec<u32>` | **disabled, not patched, under `allocator-api2`** (§3.6): bignum promotion errors at runtime instead of silently allocating via `Global`. Fully unaffected when `allocator-api2` is off. Patchable in principle, revisit later |
| `num-rational` (`BigRational`) | `SteelVal::BigRational` | none, built on `num-bigint` | same — disabled under `allocator-api2`, fixable later. Note `Rational32` (backing plain `SteelVal::Rational`) is two inline `i32`s and never heap-allocates at all, so it needs no change either way |

### 4.3 Already fine — no patch or alternative needed

- `allocator_api2::boxed::Box<T, A>` / `allocator_api2::vec::Vec<T, A>` — the crate's whole purpose, already used this way in the prior branch's work.
- `steel_rc::BiasedRc<T, A>` — ours; the allocator-generic patch already exists and is proven (Phase 1 is redoing it cleanly on this branch, not inventing it).
- `parking_lot::RwLock<T>`/`Mutex<T>` — wrap the value inline, no separate per-instance heap allocation to route through an allocator (parking_lot's contended-path "parking" bookkeeping is a one-time global structure, not a per-`Gc` allocation).

## 5. Phased plan

Each phase is scoped to be its own reviewable commit (or small commit series), matching
how the previous branch's work was structured.

- **Phase 0 — spike: done.** Read `im-lists` v0.12.1 and `steel-imbl` v7.1.0's actual
  source. Findings and the resulting decision are in §3.5: `im-lists`' chunk storage is
  already `allocator-api2`-generic (needs exposing, not building); `steel-imbl`'s chunks
  are inline (no patch needed); both crates' `PointerFamily::new` needs a patch to accept
  an allocator argument. Phase 4 is unblocked and scoped by this.
- **Phase 1 — `Gc<T, A>` foundation:** redo the already-validated `BiasedRc<T, A>` /
  `Gc<T, A>` work (steel-rc + steel-core `gc.rs`), cfg-gated to `sync+biased`, allocator
  stored in the heap block. Lowest risk — this exact design was already proven out.
- **Phase 2 — `SteelString<A>`:** point `SteelString` at `Gc<str, A>` instead of
  `Gc<String>`, extending `BiasedRc`'s existing unsized-DST (`BiasedRc<[T]>`/
  `BiasedRc<str>`) support with the `<A>` parameter from Phase 1 — no new type, just
  updating construction sites that currently build a `String` first. Easy to land and
  test in isolation, no dependents yet.
- **Phase 3 — `SteelValGeneric<A>` + `Env<A>` + `ByteCodeLambda<A>` + operand stack:**
  the large mechanical phase — rename/genericize `SteelVal`, thread `A` through every
  struct in the appendix table, alias `SteelVal = SteelValGeneric<Global>`, fix every
  call site the compiler flags. Numerically the biggest phase (275+ `Gc::new` sites,
  every file that matches on `SteelVal`).
- **Phase 4 — persistent collections:** gate `im`/`im-rc` out only when `allocator-api2`
  is enabled (`compile_error!` on that combination; both crates stay exactly as they are
  for every other build). Fork `im-lists`/`steel-imbl`: expose `im-lists`' existing `A`
  parameter up through `UnrolledCell`/`UnrolledList`/`GenericList`, and patch both crates'
  `PointerFamily::new` to accept an allocator argument (§3.5). Parametrize
  `GcPointerType`/`List`/`Vector`/`HashMap`/`HashSet` by `A` on top of that for the
  `allocator-api2` build.
- **Phase 5 — `Engine<A>`/public API:** `Engine<A = Global>`, `Engine::new_in(alloc)`
  construction (§6.5), `RegisterFn`/`IntoSteelVal` bound propagation,
  update `examples/custom_allocator.rs` to exercise the full path (closures, pairs,
  vectors, hashmaps, strings — not just `Custom` values as before), confirm the
  documented exceptions in §3.6 with `PANIC_ON_ALLOCATION`-style tests proving everything
  *else* is allocation-free on the hot path.
- **Phase 6 — cleanup pass:** re-verify `size_of::<SteelVal>() <= 16` still holds for at
  least one non-`Global` instantiation, full test suite across feature combinations,
  update docs.

## 6. Open questions before implementation starts

1. ~~Is making `imbl`+`im-lists` the forced collection backend specifically when
   `allocator-api2` is enabled acceptable?~~ **Resolved:** yes — it only affects builds
   that opt into `allocator-api2`, it reuses an already-existing feature in this
   codebase rather than introducing something novel, and Phase 0 gates it on
   verification before Phase 4 commits to it.
2. ~~Are bignum/rational's lack of allocator support acceptable as a documented
   exception?~~ **Resolved:** `BigNum`/`BigRational` are disabled outright under
   `allocator-api2` (a runtime error on overflow into arbitrary precision, rather than a
   silent `Global` allocation); fixable later, not blocking this plan.
3. ~~Are the remaining §3.6 items — `smallvec`'s spill path and `sync`+`allocator-api2`'s
   `Env` combination — acceptable as documented v1 boundaries, or is forking
   `smallvec`/`shared_vector` in scope now?~~ **Resolved, no forking:** `shared_vector`
   stays a hard `compile_error!` on `sync`+`allocator-api2` together — a build-time
   feature combination with no runtime middle ground, and not worth forking given the
   realtime use case is single-threaded anyway. `smallvec`'s spill path is resolved the
   same way as bignum/rational (§3.6): the inline fast path is untouched, only the rare
   spill case errors at runtime under `allocator-api2` — no fork needed.
4. ~~Is the §3.7 dylib incompatibility across different `A` instantiations acceptable?~~
   **Resolved:** it's an expected mechanical consequence of monomorphization, not a design
   concern — not blocking this plan.
5. ~~Naming/API shape for opting into a custom allocator?~~ **Resolved:**
   `Engine::new_in(alloc)`, following the `Gc::new_in(val, arena)` precedent already used
   in `examples/custom_allocator.rs` and mirroring Rust's own `_in`-suffix convention
   (`Vec::new_in`, `Box::new_in`). No builder, no `SteelConfig`-bundling trait — that
   would be premature abstraction for a single type parameter with nothing else to
   bundle yet; revisit only if a second config axis actually shows up later.

## Appendix: `SteelVal` variant → current backing type

| Variant | Current type | Heap? |
|---|---|---|
| `Closure` | `Gc<ByteCodeLambda>` | yes |
| `BoolV`/`NumV`/`IntV`/`Rational`/`CharV`/`Void` | inline | no |
| `VectorV` | `Gc<im_rc::Vector<SteelVal>>` (or `im`/`imbl` variant) | yes |
| `StringV`/`SymbolV` | `SteelString = Gc<String>` | yes |
| `FuncV`/`MutFunc`/`BuiltIn` | plain `fn` pointer | no |
| `Custom` | `Gc<RefCell<Box<dyn CustomType>>>` | yes (2 layers) |
| `HashMapV` | `Gc<im_rc::HashMap<SteelVal, SteelVal>>` | yes |
| `HashSetV` | `Gc<im_rc::HashSet<SteelVal>>` | yes |
| `CustomStruct` | `Gc<UserDefinedStruct>` | yes |
| `PortV` | `Gc<RefCell<SteelPortRepr>>` | yes |
| `IterV` | `Gc<Transducer>` | yes |
| `ReducerV` | `Gc<Reducer>` | yes |
| `FutureFunc` | `Shared<Box<dyn Fn(...)>>` | yes |
| `FutureV` | `Gc<FutureResult>` | yes |
| `StreamV` | `Gc<LazyStream>` | yes |
| `BoxedFunction` | `Gc<BoxedDynFunction>` | yes |
| `ContinuationFunction` | `Gc<RefCell<ContinuationMark>>`-ish | yes |
| `ListV` | `im_lists::GenericList<SteelVal, GcPointerType, 4, 2, _>` | yes |
| `Pair` | `Gc<Pair>` | yes |
| `MutableVector` | `HeapRef<Vec<SteelVal>>` (weak, into VM `Heap`) | yes |
| `BoxedIterator` | `Gc<RefCell<OpaqueIterator>>` | yes |
| `SyntaxObject` | `Gc<Syntax>` | yes |
| `Boxed` | `Gc<RefCell<SteelVal>>` | yes |
| `HeapAllocated` | `HeapRef<SteelVal>` | yes |
| `Reference` | `Gc<OpaqueReference<'static>>` | yes |
| `BigNum` | `Gc<BigInt>` (num-bigint, no allocator hook) | yes |
| `BigRational` | `Gc<BigRational>` (num-rational, no allocator hook) | yes |
| `Complex` | `{ re: SteelVal, im: SteelVal }` inline | no (recurses) |
| `ByteVector` | `Gc<RefCell<Vec<u8>>>` | yes |
