use steel_derive::function;

use crate::{
    gc::shared::ShareableMut,
    rerrs::ErrorKind,
    rvals::{FromSteelVal, RestArgsIter, Result, SteelByteVector},
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
        .register_native_fn_definition(S16_BYTES_REF_DEFINITION)
        .register_native_fn_definition(S16_BYTES_SET_DEFINITION);

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

    let copy = guard.get(start..end).ok_or_else(throw!(Generic => "index out of bounds: attempted to slice range: {:?} for bytevector: {:?}", start..end, guard))?.to_vec();

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

    if index > guard.len() {
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

/// Reads a 16-bit unsigned integer from the bytevector at the given element
/// index, interpreting the bytes using the platform's native byte order
/// (endianness). The value occupies the 2 bytes starting at offset `index * 2`;
/// if that range is out of bounds, this will error.
///
/// (bytevector-u16-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 2 0))
/// (bytevector-u16-set! vec 0 65535)
/// (bytevector-u16-ref vec 0) ;; => 65535
/// ```
#[function(name = "bytevector-u16-ref")]
pub fn u16_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 2;
    guard
        .get(start..start + 2)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let int = u16::from_ne_bytes([x[0], x[1]]);
            SteelVal::IntV(int as isize)
        })
}

/// Writes a 16-bit unsigned integer into the bytevector at the given element
/// index, storing the bytes using the platform's native byte order (endianness).
/// The value occupies the 2 bytes starting at offset `index * 2`; will error if
/// the index is out of bounds.
///
/// (bytevector-u16-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int? ;; 0 to 65535
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 2 0))
/// (bytevector-u16-set! vec 0 4660)
/// (bytevector-u16-ref vec 0) ;; => 4660
/// ```
#[function(name = "bytevector-u16-set!")]
pub fn u16_bytes_set(value: &mut SteelByteVector, index: usize, int: u16) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 2;

    let bytes = int.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];

    Ok(SteelVal::Void)
}

/// Reads a 32-bit unsigned integer from the bytevector at the given element
/// index, interpreting the bytes using the platform's native byte order
/// (endianness). The value occupies the 4 bytes starting at offset `index * 4`;
/// if that range is out of bounds, this will error.
///
/// (bytevector-u32-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 4 0))
/// (bytevector-u32-set! vec 0 4294967295)
/// (bytevector-u32-ref vec 0) ;; => 4294967295
/// ```
#[function(name = "bytevector-u32-ref")]
pub fn u32_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 4;
    guard
        .get(start..start + 4)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let int = u32::from_ne_bytes([x[0], x[1], x[2], x[3]]);
            SteelVal::IntV(int as isize)
        })
}

/// Writes a 32-bit unsigned integer into the bytevector at the given element
/// index, storing the bytes using the platform's native byte order (endianness).
/// The value occupies the 4 bytes starting at offset `index * 4`; will error if
/// the index is out of bounds.
///
/// (bytevector-u32-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int? ;; 0 to 4294967295
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 4 0))
/// (bytevector-u32-set! vec 0 305419896)
/// (bytevector-u32-ref vec 0) ;; => 305419896
/// ```
#[function(name = "bytevector-u32-set!")]
pub fn u32_bytes_set(value: &mut SteelByteVector, index: usize, int: u32) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 4;

    let bytes = int.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];
    guard[slice + 2] = bytes[2];
    guard[slice + 3] = bytes[3];

    Ok(SteelVal::Void)
}

/// Reads a 64-bit unsigned integer from the bytevector at the given element
/// index, interpreting the bytes using the platform's native byte order
/// (endianness). The value occupies the 8 bytes starting at offset `index * 8`;
/// if that range is out of bounds, this will error.
///
/// (bytevector-u64-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 8 0))
/// (bytevector-u64-set! vec 0 1099511627776)
/// (bytevector-u64-ref vec 0) ;; => 1099511627776
/// ```
#[function(name = "bytevector-u64-ref")]
pub fn u64_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 8;
    guard
        .get(start..start + 8)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let int = u64::from_ne_bytes([x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]]);
            SteelVal::IntV(int as isize)
        })
}

/// Writes a 64-bit unsigned integer into the bytevector at the given element
/// index, storing the bytes using the platform's native byte order (endianness).
/// The value occupies the 8 bytes starting at offset `index * 8`; will error if
/// the index is out of bounds.
///
/// (bytevector-u64-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int? ;; 0 to 18446744073709551615
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 8 0))
/// (bytevector-u64-set! vec 0 1099511627776)
/// (bytevector-u64-ref vec 0) ;; => 1099511627776
/// ```
#[function(name = "bytevector-u64-set!")]
pub fn u64_bytes_set(value: &mut SteelByteVector, index: usize, int: u64) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 8;

    let bytes = int.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];
    guard[slice + 2] = bytes[2];
    guard[slice + 3] = bytes[3];
    guard[slice + 4] = bytes[4];
    guard[slice + 5] = bytes[5];
    guard[slice + 6] = bytes[6];
    guard[slice + 7] = bytes[7];

    Ok(SteelVal::Void)
}

/// Reads a 32-bit floating point number from the bytevector at the
/// given element index, interpreting the bytes using the platform's native byte
/// order (endianness). The value occupies the 4 bytes starting at offset
/// `index * 4`; if that range is out of bounds, this will error.
///
/// (bytevector-f32-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 4 0))
/// (bytevector-f32-set! vec 0 1.5)
/// (bytevector-f32-ref vec 0) ;; => 1.5
/// ```
#[function(name = "bytevector-f32-ref")]
pub fn f32_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 4;
    guard
        .get(start..start + 4)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let f = f32::from_ne_bytes([x[0], x[1], x[2], x[3]]);
            SteelVal::NumV(f as f64)
        })
}

/// Writes a 32-bit floating point number into the bytevector at the
/// given element index, storing the bytes using the platform's native byte order
/// (endianness). The value occupies the 4 bytes starting at offset `index * 4`;
/// will error if the index is out of bounds.
///
/// (bytevector-f32-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : real?
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 4 0))
/// (bytevector-f32-set! vec 0 1.5)
/// (bytevector-f32-ref vec 0) ;; => 1.5
/// ```
#[function(name = "bytevector-f32-set!")]
pub fn f32_bytes_set(value: &mut SteelByteVector, index: usize, f: f32) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 4;

    let bytes = f.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];
    guard[slice + 2] = bytes[2];
    guard[slice + 3] = bytes[3];

    Ok(SteelVal::Void)
}

/// Reads a 64-bit floating point number from the bytevector at the
/// given element index, interpreting the bytes using the platform's native byte
/// order (endianness). The value occupies the 8 bytes starting at offset
/// `index * 8`; if that range is out of bounds, this will error.
///
/// (bytevector-f64-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 8 0))
/// (bytevector-f64-set! vec 0 1.5)
/// (bytevector-f64-ref vec 0) ;; => 1.5
/// ```
#[function(name = "bytevector-f64-ref")]
pub fn f64_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 8;
    guard
        .get(start..start + 8)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let f = f64::from_ne_bytes([x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]]);
            SteelVal::NumV(f)
        })
}

/// Writes a 64-bit floating point number into the bytevector at the
/// given element index, storing the bytes using the platform's native byte order
/// (endianness). The value occupies the 8 bytes starting at offset `index * 8`;
/// will error if the index is out of bounds.
///
/// (bytevector-f64-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : real?
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 8 0))
/// (bytevector-f64-set! vec 0 1.5)
/// (bytevector-f64-ref vec 0) ;; => 1.5
/// ```
#[function(name = "bytevector-f64-set!")]
pub fn f64_bytes_set(value: &mut SteelByteVector, index: usize, f: f64) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 8;

    let bytes = f.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];
    guard[slice + 2] = bytes[2];
    guard[slice + 3] = bytes[3];
    guard[slice + 4] = bytes[4];
    guard[slice + 5] = bytes[5];
    guard[slice + 6] = bytes[6];
    guard[slice + 7] = bytes[7];

    Ok(SteelVal::Void)
}

/// Reads a 16-bit signed (two's complement) integer from the bytevector at the
/// given element index, interpreting the bytes using the platform's native byte
/// order (endianness). The value occupies the 2 bytes starting at offset
/// `index * 2`; if that range is out of bounds, this will error.
///
/// (bytevector-s16-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 2 0))
/// (bytevector-s16-set! vec 0 -1)
/// (bytevector-s16-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s16-ref")]
pub fn s16_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 2;
    guard
        .get(start..start + 2)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let int = i16::from_ne_bytes([x[0], x[1]]);
            SteelVal::IntV(int as isize)
        })
}

/// Writes a 16-bit signed integer into the bytevector at the
/// given element index, storing the bytes using the platform's native byte order
/// (endianness). The value occupies the 2 bytes starting at offset `index * 2`;
/// will error if the index is out of bounds.
///
/// (bytevector-s16-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int? ;; -32768 to 32767
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 2 0))
/// (bytevector-s16-set! vec 0 -1)
/// (bytevector-s16-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s16-set!")]
pub fn s16_bytes_set(value: &mut SteelByteVector, index: usize, int: i16) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 2;

    let bytes = int.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];

    Ok(SteelVal::Void)
}

/// Reads a 32-bit signed integer from the bytevector at the
/// given element index, interpreting the bytes using the platform's native byte
/// order (endianness). The value occupies the 4 bytes starting at offset
/// `index * 4`; if that range is out of bounds, this will error.
///
/// (bytevector-s32-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 4 0))
/// (bytevector-s32-set! vec 0 -1)
/// (bytevector-s32-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s32-ref")]
pub fn s32_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 4;
    guard
        .get(start..start + 4)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let int = i32::from_ne_bytes([x[0], x[1], x[2], x[3]]);
            SteelVal::IntV(int as isize)
        })
}

/// Writes a 32-bit signed integer into the bytevector at the
/// given element index, storing the bytes using the platform's native byte order
/// (endianness). The value occupies the 4 bytes starting at offset `index * 4`;
/// will error if the index is out of bounds.
///
/// (bytevector-s32-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int? ;; -2147483648 to 2147483647
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 4 0))
/// (bytevector-s32-set! vec 0 -1)
/// (bytevector-s32-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s32-set!")]
pub fn s32_bytes_set(value: &mut SteelByteVector, index: usize, int: i32) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 4;

    let bytes = int.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];
    guard[slice + 2] = bytes[2];
    guard[slice + 3] = bytes[3];

    Ok(SteelVal::Void)
}

/// Reads a 64-bit signed integer from the bytevector at the
/// given element index, interpreting the bytes using the platform's native byte
/// order (endianness). The value occupies the 8 bytes starting at offset
/// `index * 8`; if that range is out of bounds, this will error.
///
/// (bytevector-s64-ref vector index)
///
/// * vector : bytes?
/// * index : (and exact? int?)
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 8 0))
/// (bytevector-s64-set! vec 0 -1)
/// (bytevector-s64-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s64-ref")]
pub fn s64_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    let start = index * 8;
    guard
        .get(start..start + 8)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| {
            let int = i64::from_ne_bytes([x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]]);
            SteelVal::IntV(int as isize)
        })
}

/// Writes a 64-bit signed integer into the bytevector at the
/// given element index, storing the bytes using the platform's native byte order
/// (endianness). The value occupies the 8 bytes starting at offset `index * 8`;
/// will error if the index is out of bounds.
///
/// (bytevector-s64-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int? ;; -9223372036854775808 to 9223372036854775807
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 8 0))
/// (bytevector-s64-set! vec 0 -1)
/// (bytevector-s64-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s64-set!")]
pub fn s64_bytes_set(value: &mut SteelByteVector, index: usize, int: i64) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    let slice = index * 8;

    let bytes = int.to_ne_bytes();

    guard[slice] = bytes[0];
    guard[slice + 1] = bytes[1];
    guard[slice + 2] = bytes[2];
    guard[slice + 3] = bytes[3];
    guard[slice + 4] = bytes[4];
    guard[slice + 5] = bytes[5];
    guard[slice + 6] = bytes[6];
    guard[slice + 7] = bytes[7];

    Ok(SteelVal::Void)
}

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
/// (bytevector-s8-set! vec 0 -1)
/// (bytevector-s8-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s8-ref")]
pub fn s8_bytes_ref(value: &SteelByteVector, index: usize) -> Result<SteelVal> {
    let guard = value.vec.read();
    guard
        .get(index)
        .ok_or_else(
            throw!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard),
        )
        .map(|x| SteelVal::IntV(*x as isize))
}

/// Writes an 8-bit signed integer into the bytevector at the
/// given index. Unlike the wider variants, the index addresses a single byte
/// directly (offset `index`). Will error if the index is out of bounds.
///
/// (bytevector-s8-set! vector index value)
///
/// * vector : bytes?
/// * index : (and exact? int?)
/// * value : int? ;; -128 to 127
///
/// # Examples
/// ```scheme
/// (define vec (make-bytes 1 0))
/// (bytevector-s8-set! vec 0 -1)
/// (bytevector-s8-ref vec 0) ;; => -1
/// ```
#[function(name = "bytevector-s8-set!")]
pub fn s8_bytes_set(value: &mut SteelByteVector, index: usize, byte: i8) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    guard[index] = byte as _;

    Ok(SteelVal::Void)
}
