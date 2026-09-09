use steel_derive::function;

use crate::{
    gc::shared::ShareableMut,
    rerrs::ErrorKind,
    rvals::{FromSteelVal, IntoSteelVal, RestArgsIter, Result, SteelByteVector},
    steel_vm::builtin::BuiltInModule,
    stop, throw, SteelErr, SteelVal,
};

#[steel_derive::define_module(name = "steel/bytevectors")]
pub fn bytevector_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/bytevectors");

    module
        .register_native_fn_definition(BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(BYTES_DEFINITION)
        .register_native_fn_definition(IS_BYTES_DEFINITION)
        .register_native_fn_definition(BYTEVECTOR_COPY_NEW_DEFINITION)
        .register_native_fn_definition(MAKE_BYTES_DEFINITION)
        .register_native_fn_definition(IS_BYTE_DEFINITION)
        .register_native_fn_definition(BYTES_LENGTH_DEFINITION)
        .register_native_fn_definition(BYTES_REF_DEFINITION)
        .register_native_fn_definition(BYTES_SET_DEFINITION)
        .register_native_fn_definition(BYTES_TO_LIST_DEFINITION)
        .register_native_fn_definition(LIST_TO_BYTES_DEFINITION)
        .register_native_fn_definition(BYTES_APPEND_DEFINITION)
        .register_native_fn_definition(BYTES_TO_STRING_DEFINITION)
        .register_native_fn_definition(BYTES_PUSH_DEFINITION)
        .register_native_fn_definition(BYTES_CLEAR_DEFINITION);

    module
        .register_native_fn_definition(S16_BYTES_REF_DEFINITION)
        .register_native_fn_definition(S16_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(U16_BYTES_REF_DEFINITION)
        .register_native_fn_definition(U16_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(U32_BYTES_REF_DEFINITION)
        .register_native_fn_definition(U32_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(U64_BYTES_REF_DEFINITION)
        .register_native_fn_definition(U64_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(S8_BYTES_REF_DEFINITION)
        .register_native_fn_definition(S8_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(S32_BYTES_REF_DEFINITION)
        .register_native_fn_definition(S32_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(S64_BYTES_REF_DEFINITION)
        .register_native_fn_definition(S64_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(F32_BYTES_REF_DEFINITION)
        .register_native_fn_definition(F32_BYTES_SET_DEFINITION);

    module
        .register_native_fn_definition(F64_BYTES_REF_DEFINITION)
        .register_native_fn_definition(F64_BYTES_SET_DEFINITION);

    module
}

/// Returns a new mutable vector with each byte as the given arguments.
/// Each argument must satisfy the `byte?` predicate, meaning it is an exact
/// integer range from 0 - 255 (inclusive)
///
/// (bytevector b ...)
///
/// * b : byte?
///
///
/// # Examples
/// ```scheme
/// (bytevector 65 112 112 108 101)
/// ```
#[steel_derive::native(name = "bytevector", arity = "AtLeast(0)")]
pub fn bytevector(args: &[SteelVal]) -> Result<SteelVal> {
    args.iter()
        .map(|x| u8::from_steelval(x))
        .collect::<Result<Vec<_>>>()
        .map(SteelByteVector::new)
        .map(SteelVal::ByteVector)
}

/// Returns a new mutable vector with each byte as the given arguments.
/// Each argument must satisfy the `byte?` predicate, meaning it is an exact
/// integer range from 0 - 255 (inclusive)
///
/// (bytes b ...)
///
/// * b : byte?
///
///
/// # Examples
/// ```scheme
/// (bytes 65 112 112 108 101)
/// ```
#[steel_derive::native(name = "bytes", alias = "bytevector", arity = "AtLeast(0)")]
pub fn bytes(args: &[SteelVal]) -> Result<SteelVal> {
    args.iter()
        .map(|x| u8::from_steelval(x))
        .collect::<Result<Vec<_>>>()
        .map(SteelByteVector::new)
        .map(SteelVal::ByteVector)
}

/// Returns `#t` if this value is a bytevector
///
/// # Examples
/// ```scheme
/// (bytes? (bytes 0 1 2)) ;; => #t
/// (bytes? (list 10 20 30)) ;; => #f
/// ```
#[steel_derive::function(name = "bytes?", alias = "bytevector?")]
pub fn is_bytes(arg: &SteelVal) -> bool {
    matches!(arg, SteelVal::ByteVector(_))
}

/// Creates a copy of a bytevector.
///
/// (bytevector-copy vector [start end]) -> bytes?
///
/// * vector : bytes?
/// * start: int? = 0
/// * end: int? = (bytes-length vector)
///
/// # Examples
///
/// ```scheme
/// (define vec (bytes 1 2 3 4 5))
///
/// (bytes-copy vec) ;; => (bytes 1 2 3 4 5)
/// (bytes-copy vec 1 3) ;; => (bytes 2 3)
/// ```
#[steel_derive::function(name = "bytes-copy", alias = "bytevector-copy")]
pub fn bytevector_copy_new(
    bytevector: &SteelByteVector,
    mut rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let guard = bytevector.vec.read();

    let start = if let Some(start) = rest.next() {
        let start = start?;

        start.try_into().map_err(|_err| {
            SteelErr::new(
                ErrorKind::ConversionError,
                format!("Unable to convert isize to usize for indexing: {}", start),
            )
        })?
    } else {
        0
    };

    let end = if let Some(end) = rest.next() {
        let end = end?;

        end.try_into().map_err(|_err| {
            SteelErr::new(
                ErrorKind::ConversionError,
                format!("Unable to convert isize to usize for indexing: {}", start),
            )
        })?
    } else {
        guard.len()
    };

    let copy = guard
        .get(start..end)
        .ok_or_else(throw!(Generic => "index out of bounds: attempted to slice range: {:?} for bytevector: {:?}", start..end, guard))?.to_vec();

    Ok(SteelVal::ByteVector(SteelByteVector::new(copy)))
}

/// Creates a bytevector given a length and a default value.
///
/// (make-bytes len default) -> bytes?
///
/// * len : int?
/// * default : byte?
///
/// # Examples
/// ```scheme
/// (make-bytes 6 42) ;; => (bytes 42 42 42 42 42)
/// ```
#[function(name = "make-bytes", alias = "make-bytevector")]
pub fn make_bytes(k: usize, mut c: RestArgsIter<'_, isize>) -> Result<SteelVal> {
    let default = c.next();

    // We want the iterator to be exhaused
    if let Some(next) = c.next() {
        stop!(ArityMismatch => format!("make-bytes expected 1 or 2 arguments, got an additional argument {}", next?))
    }

    let unwrapped = default.unwrap_or(Ok(0))?;

    let default: u8 = unwrapped.try_into().map_err(|_err| {
        SteelErr::new(
            ErrorKind::ConversionError,
            format!(
                "Unable to convert isize to u8 for default value: {}",
                unwrapped
            ),
        )
    })?;

    Ok(SteelVal::ByteVector(SteelByteVector::new(vec![default; k])))
}

/// Returns `#t` if the given value is a byte, meaning an exact
/// integer between 0 and 255 inclusive, `#f` otherwise.
///
/// # Examples
/// ```scheme
/// (byte? 65) ;; => #t
/// (byte? 0) ;; => #t
/// (byte? 256) ;; => #f
/// (byte? 100000) ;; => #f
/// (byte? -1) ;; => #f
/// ```
#[function(name = "byte?")]
pub fn is_byte(value: &SteelVal) -> bool {
    if let SteelVal::IntV(i) = value {
        u8::try_from(*i).is_ok()
    } else {
        false
    }
}

/// Returns the length of the given byte vector
///
/// # Examples
/// ```scheme
/// (bytes-length (bytes 1 2 3 4 5)) ;; => 5
/// ```
#[function(name = "bytes-length", alias = "bytevector-length")]
pub fn bytes_length(value: &SteelByteVector) -> usize {
    value.vec.read().len()
}

/// Fetches the byte at the given index within the bytevector.
/// If the index is out of bounds, this will error.
///
/// (bytes-ref vector index)
///
/// * vector : bytes?
/// * index: (and exact? int?)
///
/// # Examples
/// ```scheme
/// (bytes-ref (bytes 0 1 2 3 4 5) 3) ;; => 4
/// (bytes-ref (bytes) 10) ;; error
/// ```
#[function(name = "bytes-ref", alias = "bytevector-u8-ref")]
pub fn bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    guard
        .get(index)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| SteelVal::IntV(*x as isize))
}

/// Sets the byte at the given index to the given byte. Will error
/// if the index is out of bounds.
///
/// (bytes-set! vector index byte)
///
/// * vector : bytes?
/// * index: (and exact? int?)
/// * byte: byte?
///
/// # Examples
/// ```scheme
/// (define my-bytes (bytes 0 1 2 3 4 5))
/// (bytes-set! my-bytes 0 100)
/// (bytes-ref my-bytes 0) ;; => 100
/// ```
#[function(name = "bytes-set!", alias = "bytevector-u8-set!")]
pub fn bytes_set(value: &mut SteelByteVector, index: usize, byte: u8) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index >= guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    guard[index] = byte;

    Ok(SteelVal::Void)
}

/// Appends a byte to the end of the given bytevector, growing it in place.
///
/// (bytes-push! vector byte)
///
/// * vector : bytes?
/// * byte : byte?
///
/// # Examples
/// ```scheme
/// (define my-bytes (bytes 0 1 2))
/// (bytes-push! my-bytes 3)
/// my-bytes ;; =>  #u8(#x00 #x01 #x02 #x03)
/// ```
#[function(name = "bytes-push!")]
pub fn bytes_push(value: &mut SteelByteVector, byte: u8) -> Result<SteelVal> {
    let mut guard = value.vec.write();
    guard.push(byte);
    Ok(SteelVal::Void)
}

/// Removes all bytes from the given bytevector, leaving it empty.
///
/// (bytes-clear! vector)
///
/// * vector : bytes?
///
/// # Examples
/// ```scheme
/// (define my-bytes (bytes 0 1 2 3))
/// (bytes-clear! my-bytes)
/// my-bytes ;; => #u8()
/// ```
#[function(name = "bytes-clear!")]
pub fn bytes_clear(value: &mut SteelByteVector) -> Result<SteelVal> {
    let mut guard = value.vec.write();
    guard.clear();
    Ok(SteelVal::Void)
}

/// Converts the bytevector to the equivalent list representation.
///
/// # Examples
/// ```scheme
/// (bytes->list (bytes 0 1 2 3 4 5)) ;; => '(0 1 2 3 4 5)
/// ```
#[function(name = "bytes->list")]
pub fn bytes_to_list(value: &SteelByteVector) -> Result<SteelVal> {
    Ok(SteelVal::ListV(
        value
            .vec
            .read()
            .iter()
            .map(|x| SteelVal::IntV(*x as isize))
            .collect(),
    ))
}

/// Converts the list of bytes to the equivalent bytevector representation.
/// The list must contain _only_ values which satisfy the `byte?` predicate,
/// otherwise this function will error.
///
/// # Examples
/// ```scheme
/// (list->bytes (list 0 1 2 3 4 5)) ;; => (bytes 0 1 2 3 4 5)
/// ```
#[function(name = "list->bytes")]
pub fn list_to_bytes(value: Vec<u8>) -> Result<SteelVal> {
    Ok(SteelVal::ByteVector(SteelByteVector::new(value)))
}

/// Append multiple byte vectors into a new bytevector.
///
/// # Examples
/// ```scheme
/// (bytes-append #u8(0 1 2) #u8(3 4 5)) ;; => #u8(#x00 #x01 #x02 #x03 #x04 #x05)
///
/// (bytes-append #u8(0) #u8(1) #u8() #u8(2)) ;; => #u8(#x00 #x01 #x02)
/// ```
#[function(name = "bytes-append", alias = "bytevector-append")]
pub fn bytes_append(mut rest: RestArgsIter<'_, &SteelByteVector>) -> Result<SteelVal> {
    let mut vector = vec![];

    while let Some(bytes) = rest.next().transpose()? {
        let borrow = bytes.vec.read();
        vector.extend(&*borrow);
    }

    Ok(SteelVal::ByteVector(SteelByteVector::new(vector)))
}

/// Decodes a string from a bytevector containing valid UTF-8.
///
/// (bytes->string/utf8 buf [start] [end]) -> string?
///
/// * buf : bytes?
/// * start: int? = 0
/// * end: int? = (bytes-length buf)
///
/// # Examples
/// ```scheme
/// (bytes->string/utf8 (bytes #xe5 #x8d #x83 #xe8 #x91 #x89)) ;; => "千葉"
/// ```
#[function(name = "bytes->string/utf8", alias = "utf8->string")]
pub fn bytes_to_string(
    value: &SteelByteVector,
    mut rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let borrowed = value.vec.read();

    let start = rest.next().transpose()?.unwrap_or(0);
    let end = rest.next().transpose()?.unwrap_or(borrowed.len() as isize);

    if rest.next().is_some() {
        stop!(ArityMismatch => "expected at most 3 arguments");
    }

    if start < 0 {
        stop!(ContractViolation => "start should be a positive number, got {}", start);
    }

    if end < 0 {
        stop!(ContractViolation => "end should be a positive number, got {}", end);
    }

    if end < start {
        stop!(ContractViolation => "start should be smaller than end, got {} and {}", start, end);
    }

    let start = start as usize;
    let end = end as usize;

    let Ok(s) = core::str::from_utf8(&(&*borrowed)[start..end]) else {
        stop!(ConversionError => "bytevector contains malformed UTF-8")
    };

    Ok(s.to_string().into())
}

macro_rules! bytevector_accessor {
    (
        $ref_fn:ident => $ref_name:literal,
        $set_fn:ident => $set_name:literal,
        int $ty:ty,
        width = $width:literal,
        kind = $kind:literal,
        example = $example:literal $(,)?
    ) => {
        bytevector_accessor!(@build
            $ref_fn => $ref_name,
            $set_fn => $set_name,
            $ty,
            width = $width,
            kind = $kind,
            range = "int?",
            example = $example
        );
    };

    (
        $ref_fn:ident => $ref_name:literal,
        $set_fn:ident => $set_name:literal,
        real $ty:ty,
        width = $width:literal,
        kind = $kind:literal,
        example = $example:literal $(,)?
    ) => {
        bytevector_accessor!(@build
            $ref_fn => $ref_name,
            $set_fn => $set_name,
            $ty,
            width = $width,
            kind = $kind,
            range = "real?",
            example = $example
        );
    };

    (@build
        $ref_fn:ident => $ref_name:literal,
        $set_fn:ident => $set_name:literal,
        $ty:ty,
        width = $width:literal,
        kind = $kind:literal,
        range = $range:literal,
        example = $example:literal $(,)?
    ) => {
        #[doc = concat!("Reads ", $kind, " from the bytevector at the given element")]
        #[doc = "index, interpreting the bytes using the platform's native byte order"]
        #[doc = concat!("(endianness). The value occupies the ", stringify!($width), " bytes starting at offset")]
        #[doc = concat!("`index * ", stringify!($width), "`; if that range is out of bounds, this will error.")]
        #[doc = ""]
        #[doc = concat!("(", $ref_name, " vector index)")]
        #[doc = ""]
        #[doc = "* vector : bytes?"]
        #[doc = "* index : (and exact? int?)"]
        #[doc = ""]
        #[doc = "# Examples"]
        #[doc = "```scheme"]
        #[doc = concat!("(define vec (make-bytes ", stringify!($width), " 0))")]
        #[doc = concat!("(", $set_name, " vec 0 ", $example, ")")]
        #[doc = concat!("(", $ref_name, " vec 0) ;; => ", $example)]
        #[doc = "```"]
        #[function(name = $ref_name)]
        pub fn $ref_fn(vector: &SteelByteVector, index: usize) -> Result<SteelVal> {
            const WIDTH: usize = core::mem::size_of::<$ty>();
            const _: () = assert!(WIDTH == $width);

            let guard = vector.vec.read();

            guard
                .chunks_exact(WIDTH)
                .nth(index)
                .ok_or_else(
                    throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
                )
                .and_then(|x| {
                    let mut buf = [0u8; WIDTH];
                    buf.copy_from_slice(x);
                    <$ty>::from_ne_bytes(buf).into_steelval()
                })
        }

        #[doc = concat!("Writes ", $kind, " into the bytevector at the given element")]
        #[doc = "index, storing the bytes using the platform's native byte order (endianness)."]
        #[doc = concat!("The value occupies the ", stringify!($width), " bytes starting at offset `index * ", stringify!($width), "`;")]
        #[doc = "will error if the index is out of bounds."]
        #[doc = ""]
        #[doc = concat!("(", $set_name, " vector index value)")]
        #[doc = ""]
        #[doc = "* vector : bytes?"]
        #[doc = "* index : (and exact? int?)"]
        #[doc = concat!("* value : ", $range)]
        #[doc = ""]
        #[doc = "# Examples"]
        #[doc = "```scheme"]
        #[doc = concat!("(define vec (make-bytes ", stringify!($width), " 0))")]
        #[doc = concat!("(", $set_name, " vec 0 ", $example, ")")]
        #[doc = concat!("(", $ref_name, " vec 0) ;; => ", $example)]
        #[doc = "```"]
        #[function(name = $set_name)]
        pub fn $set_fn(vector: &mut SteelByteVector, index: usize, value: $ty) -> Result<SteelVal> {
            const WIDTH: usize = core::mem::size_of::<$ty>();

            let mut guard = vector.vec.write();

            let Some(slot) = guard.chunks_exact_mut(WIDTH).nth(index) else {
                stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
            };

            slot.copy_from_slice(&value.to_ne_bytes());

            Ok(SteelVal::Void)
        }
    };
}

bytevector_accessor!(
    u16_bytes_ref => "bytevector-u16-ref",
    u16_bytes_set => "bytevector-u16-set!",
    int u16,
    width = 2,
    kind = "a 16-bit unsigned integer",
    example = "1000",
);

bytevector_accessor!(
    u32_bytes_ref => "bytevector-u32-ref",
    u32_bytes_set => "bytevector-u32-set!",
    int u32,
    width = 4,
    kind = "a 32-bit unsigned integer",
    example = "100000",
);

bytevector_accessor!(
    u64_bytes_ref => "bytevector-u64-ref",
    u64_bytes_set => "bytevector-u64-set!",
    int u64,
    width = 8,
    kind = "a 64-bit unsigned integer",
    example = "1000000",
);

bytevector_accessor!(
    f32_bytes_ref => "bytevector-f32-ref",
    f32_bytes_set => "bytevector-f32-set!",
    real f32,
    width = 4,
    kind = "a 32-bit floating point number",
    example = "1.5",
);

bytevector_accessor!(
    f64_bytes_ref => "bytevector-f64-ref",
    f64_bytes_set => "bytevector-f64-set!",
    real f64,
    width = 8,
    kind = "a 64-bit floating point number",
    example = "1.5",
);

bytevector_accessor!(
    s16_bytes_ref => "bytevector-s16-ref",
    s16_bytes_set => "bytevector-s16-set!",
    int i16,
    width = 2,
    kind = "a 16-bit signed (two's complement) integer",
    example = "-1000",
);

bytevector_accessor!(
    s32_bytes_ref => "bytevector-s32-ref",
    s32_bytes_set => "bytevector-s32-set!",
    int i32,
    width = 4,
    kind = "a 32-bit signed (two's complement) integer",
    example = "-100000",
);

bytevector_accessor!(
    s64_bytes_ref => "bytevector-s64-ref",
    s64_bytes_set => "bytevector-s64-set!",
    int i64,
    width = 8,
    kind = "a 64-bit signed (two's complement) integer",
    example = "-1000000",
);

/// Reads an 8-bit signed integer from the bytevector at the
/// given index. Unlike the wider variants, the index addresses a single byte
/// directly (offset `index`). If the index is out of bounds, this will error.
///
/// (bytevector-s8-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 1 0))
/// (bytevector-s8-set! vec 0 -100)
/// (bytevector-s8-ref vec 0) ;; => -100
/// ```
#[function(name = "bytevector-s8-ref")]
pub fn s8_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    guard
        .get(index)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| SteelVal::IntV(*x as i8 as isize))
}

/// Writes an 8-bit signed integer into the bytevector at the
/// given index. Unlike the wider variants, the index addresses a single byte
/// directly (offset `index`). Will error if the index is out of bounds.
///
/// (bytevector-s8-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int?
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 1 0))
/// (bytevector-s8-set! vec 0 -100)
/// (bytevector-s8-ref vec 0) ;; => -100
/// ```
#[function(name = "bytevector-s8-set!")]
pub fn s8_bytes_set(value: &mut SteelByteVector, index: usize, byte: i8) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index >= guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    guard[index] = byte as _;

    Ok(SteelVal::Void)
}
