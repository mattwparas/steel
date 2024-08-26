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
        .register_native_fn_definition(BYTES_TO_STRING_DEFINITION);

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
        .map(|x| u8::from_steelval(&x))
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
#[steel_derive::native(name = "bytes", arity = "AtLeast(0)")]
pub fn bytes(args: &[SteelVal]) -> Result<SteelVal> {
    args.iter()
        .map(|x| u8::from_steelval(&x))
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
#[steel_derive::function(name = "bytes?")]
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
/// (bytevector-copy vec) ;; => (bytes 1 2 3 4 5)
/// (bytevector-copy vec 1 3) ;; => (bytes 2 3)
/// ```
#[steel_derive::function(name = "bytevector-copy")]
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
#[function(name = "make-bytes")]
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
#[function(name = "bytes-length")]
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
#[function(name = "bytes-ref")]
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
#[function(name = "bytes-set!")]
pub fn bytes_set(value: &mut SteelByteVector, index: usize, byte: u8) -> Result<SteelVal> {
    let mut guard = value.vec.write();

    if index > guard.len() {
        stop!(Generic => "index out of bounds: index: {} of byte vector {:?}", index, guard);
    }

    guard[index] = byte;

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
#[function(name = "bytes-append")]
pub fn bytes_append(mut rest: RestArgsIter<'_, &SteelByteVector>) -> Result<SteelVal> {
    let mut vector = vec![];

    while let Some(bytes) = rest.next().transpose()? {
        let borrow = (&*bytes.vec).read();
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

    let Ok(s) = std::str::from_utf8(&(&*borrowed)[start..end]) else {
        stop!(ConversionError => "bytevector contains malformed UTF-8")
    };

    Ok(s.to_string().into())
}
