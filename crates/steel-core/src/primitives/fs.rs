use crate::path::OwnedPath;
use crate::rvals::{
    self, AsRefMutSteelVal, AsRefSteelVal, Custom, IntoSteelVal, RestArgsIter, Result, SteelString,
    SteelVal,
};
use crate::steel_vm::builtin::BuiltInModule;
use crate::{steelerr, throw};
use alloc::format;
use alloc::string::String;
use std::env::{current_dir, set_current_dir};
use std::path::Path;

use std::fs::{self, DirEntry, Metadata, ReadDir};
use std::io;

fn get_extension_from_filename(filename: &str) -> Option<&str> {
    Path::new(filename)
        .extension()
        .and_then(std::ffi::OsStr::to_str)
}

/// Copy files from source to destination recursively.
/// from https://nick.groenen.me/notes/recursively-copy-files-in-rust/
pub fn copy_recursively(source: impl AsRef<Path>, destination: impl AsRef<Path>) -> io::Result<()> {
    fs::create_dir_all(&destination)?;
    for entry in fs::read_dir(source)? {
        let entry = entry?;
        let filetype = entry.file_type()?;
        if filetype.is_dir() {
            copy_recursively(entry.path(), destination.as_ref().join(entry.file_name()))?;
        } else {
            fs::copy(entry.path(), destination.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}

impl Custom for OwnedPath {
    fn fmt(&self) -> Option<core::result::Result<String, core::fmt::Error>> {
        Some(Ok(format!("#<Path:{}>", self.to_string_lossy())))
    }
}

impl Custom for glob::MatchOptions {}
impl Custom for glob::PatternError {}
impl Custom for glob::Paths {}

#[steel_derive::function(name = "glob")]
pub fn glob(pattern: SteelString, mut rest: RestArgsIter<'_, &SteelVal>) -> Result<SteelVal> {
    use crate::rvals::FromSteelVal;

    let options = rest
        .next()
        .map(|x| glob::MatchOptions::from_steelval(x.unwrap()))
        .unwrap_or_else(|| Ok(glob::MatchOptions::default()))?;

    if rest.next().is_some() {
        crate::stop!(ArityMismatch => "glob expects no more than 2 arguments");
    }

    glob::glob_with(pattern.as_str(), options)
        .map_err(|x| throw!(Generic => "glob pattern: {:?}", x)())?
        .into_steelval()
}

#[steel_derive::function(name = "glob-iter-next!")]
pub fn glob_paths_next(paths: &SteelVal) -> Result<SteelVal> {
    let mut paths = glob::Paths::as_mut_ref(paths)?;
    match paths.next() {
        Some(Ok(v)) => OwnedPath::from(v).into_steelval(),
        Some(Err(e)) => crate::stop!(Generic => "glob-iter-next!: {:?}", e),
        None => Ok(SteelVal::BoolV(false)),
    }
}

#[steel_derive::function(name = "path->string")]
pub fn path_to_string(path: &SteelVal) -> Result<SteelVal> {
    let path = <OwnedPath as rvals::AsRefSteelVal>::as_ref(path)?;

    #[cfg(feature = "std")]
    {
        return path
            .as_path()
            .to_str()
            .map(|x| SteelVal::StringV(x.to_string().into()))
            .into_steelval();
    }

    #[cfg(not(feature = "std"))]
    {
        Ok(SteelVal::StringV(path.as_str().into()))
    }
}

/// Filesystem functions, mostly just thin wrappers around the `std::fs` functions in
/// the Rust std library.
#[steel_derive::define_module(name = "steel/filesystem")]
pub fn fs_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/filesystem");
    module
        .register_native_fn_definition(DELETE_DIRECTORY_DEFINITION)
        .register_native_fn_definition(CREATE_DIRECTORY_DEFINITION)
        .register_native_fn_definition(COPY_DIRECTORY_RECURSIVELY_DEFINITION)
        .register_native_fn_definition(IS_DIR_DEFINITION)
        .register_native_fn_definition(IS_FILE_DEFINITION)
        .register_native_fn_definition(READ_DIR_DEFINITION)
        .register_native_fn_definition(PATH_EXISTS_DEFINITION)
        .register_native_fn_definition(FILE_NAME_DEFINITION)
        .register_native_fn_definition(PARENT_NAME_DEFINITION)
        .register_native_fn_definition(CANONICALIZE_PATH_DEFINITION)
        .register_native_fn_definition(CURRENT_DIRECTORY_DEFINITION)
        .register_native_fn_definition(CHANGE_CURRENT_DIRECTORY_DEFINITION)
        .register_native_fn_definition(GET_EXTENSION_DEFINITION)
        .register_native_fn_definition(DELETE_FILE_DEFINITION)
        .register_native_fn_definition(READ_DIR_ITER_DEFINITION)
        .register_native_fn_definition(READ_DIR_ITER_NEXT_DEFINITION)
        .register_native_fn_definition(READ_DIR_ENTRY_IS_DIR_DEFINITION)
        .register_native_fn_definition(READ_DIR_ENTRY_IS_FILE_DEFINITION)
        .register_native_fn_definition(READ_DIR_ENTRY_IS_SYMLINK_DEFINITION)
        .register_native_fn_definition(READ_DIR_ENTRY_PATH_DEFINITION)
        .register_native_fn_definition(READ_DIR_ENTRY_FILE_NAME_DEFINITION)
        .register_native_fn_definition(READ_DIR_ENTRY_METADATA_DEFINITION)
        .register_native_fn_definition(FS_METADATA_MODIFIED_DEFINITION)
        .register_native_fn_definition(FS_METADATA_ACCESSED_DEFINITION)
        .register_native_fn_definition(FS_METADATA_CREATED_DEFINITION)
        .register_native_fn_definition(FS_METADATA_IS_FILE_DEFINITION)
        .register_native_fn_definition(FS_METADATA_IS_DIR_DEFINITION)
        .register_native_fn_definition(FS_METADATA_IS_SYMLINK_DEFINITION)
        .register_native_fn_definition(FS_METADATA_LEN_DEFINITION)
        .register_native_fn_definition(FILE_METADATA_DEFINITION)
        .register_native_fn_definition(IS_FS_METADATA_DEFINITION)
        .register_native_fn_definition(IS_READ_DIR_DEFINITION)
        .register_native_fn_definition(IS_READ_DIR_ITER_ENTRY_DEFINITION)
        .register_native_fn_definition(GLOB_DEFINITION)
        .register_native_fn_definition(GLOB_PATHS_NEXT_DEFINITION)
        .register_native_fn_definition(PATH_TO_STRING_DEFINITION);
    module
}

#[steel_derive::define_module(name = "steel/filesystem")]
pub fn fs_module_sandbox() -> BuiltInModule {
    BuiltInModule::new("steel/filesystem")
}

impl Custom for ReadDir {}
impl Custom for DirEntry {}
impl Custom for Metadata {}

/// Creates an iterator over the contents of the given directory.
/// The given path must be a directory.
///
/// (read-dir-iter dir) -> #<ReadDir>
///
/// * dir : (is-dir?) - the directory to iterate over
///
/// # Examples
/// ```scheme
/// (define my-iter (read-dir-iter "src"))
/// (read-dir-iter-next! my-iter) ;; => #<DirEntry> src/lib.rs
/// (read-dir-iter-next! my-iter) ;; => #<DirEntry> src/main.rs
/// (read-dir-iter-next! my-iter) ;; => #false
/// ```
#[steel_derive::function(name = "read-dir-iter")]
pub fn read_dir_iter(directory: &SteelString) -> Result<SteelVal> {
    let p = Path::new(directory.as_ref());
    p.read_dir()?.into_steelval()
}

/// Reads one entry from the iterator. Reads a `ReadDir` struct.
///
/// (read-dir-iter-next! read-dir-iter) -> #<DirEntry>
///
/// * dir : (read-dir-iter?) - the directory to iterate over
///
/// # Examples
/// ```scheme
/// (define my-iter (read-dir-iter "src"))
/// (define nex-entry (read-dir-iter-next! my-iter)) ;; => #<DirEntry> src/lib.rs
/// (read-dir-entry-is-dir? next-entry) ;; => #false
/// (read-dir-entry-is-file? next-entry) ;; => #true
/// (read-dir-entry-file-name) ;; => "lib.rs"
/// ```
#[steel_derive::function(name = "read-dir-iter-next!")]
pub fn read_dir_iter_next(iter: &SteelVal) -> Result<SteelVal> {
    ReadDir::as_mut_ref(iter)?
        .next()
        .transpose()
        .map(|x| x.map(|entry| entry.into_steelval().unwrap()))?
        .into_steelval()
}

/// Checks whether the given value is a #<ReadDir>
///
/// (read-dir-iter? value) -> bool?
///
/// # Examples
/// ```scheme
/// (define my-iter (read-dir-iter "src"))
/// (read-dir-iter? my-iter) ;; => #true
/// (read-dir-iter "not an iter") ;; => #false
/// ```
#[steel_derive::function(name = "read-dir-iter?")]
pub fn is_read_dir(value: &SteelVal) -> Result<SteelVal> {
    ReadDir::as_ref(value).is_ok().into_steelval()
}

/// Checks whether the given value is a #<DirEntry>
///
/// (read-dir-iter-entry? value) -> bool?
///
/// # Examples
/// ```scheme
/// (define my-iter (read-dir-iter "src"))
/// (define next (read-dir-iter-next! my-iter))
/// (read-dir-iter-entry? next) ;; => #true
/// ```
#[steel_derive::function(name = "read-dir-iter-entry?")]
pub fn is_read_dir_iter_entry(value: &SteelVal) -> Result<SteelVal> {
    DirEntry::as_ref(value).is_ok().into_steelval()
}

/// Checks whether the read dir entry is a directory.
///
/// (read-dir-entry-is-dir? value) -> bool?
///
/// * value : read-dir-iter-entry?
///
/// # Examples
/// ```scheme
/// (define my-iter (read-dir-iter "src"))
/// (define next (read-dir-iter-next! my-iter))
///
/// (read-dir-entry-path) ;; => "src/lib.rs"
/// (read-dir-entry-is-dir? next) ;; #false - because this is a file
///
/// ```
#[steel_derive::function(name = "read-dir-entry-is-dir?")]
pub fn read_dir_entry_is_dir(value: &SteelVal) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(
        DirEntry::as_ref(value)?.file_type()?.is_dir(),
    ))
}

/// Checks whether the read dir entry is a file.
///
/// (read-dir-entry-is-dir? value) -> bool?
///
/// * value : read-dir-iter-entry?
///
/// # Examples
/// ```scheme
/// (define my-iter (read-dir-iter "src"))
/// (define next (read-dir-iter-next! my-iter))
///
/// (read-dir-entry-path) ;; => "src/lib.rs"
/// (read-dir-entry-is-dir? next) ;; #true - because this is a file
///
/// ```
#[steel_derive::function(name = "read-dir-entry-is-file?")]
pub fn read_dir_entry_is_file(value: &SteelVal) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(
        DirEntry::as_ref(value)?.file_type()?.is_file(),
    ))
}

/// Checks whether the read dir entry is a symlink.
#[steel_derive::function(name = "read-dir-entry-is-symlink?")]
pub fn read_dir_entry_is_symlink(value: &SteelVal) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(
        DirEntry::as_ref(value)?.file_type()?.is_symlink(),
    ))
}

/// Returns the path from a given read-dir-entry.
#[steel_derive::function(name = "read-dir-entry-path")]
pub fn read_dir_entry_path(value: &SteelVal) -> Result<SteelVal> {
    match DirEntry::as_ref(value)?.path().to_str() {
        Some(p) => Ok(SteelVal::StringV(p.into())),
        None => Ok(SteelVal::BoolV(false)),
    }
}

/// Returns the file name from a given read-dir-entry.
#[steel_derive::function(name = "read-dir-entry-file-name")]
pub fn read_dir_entry_file_name(value: &SteelVal) -> Result<SteelVal> {
    match DirEntry::as_ref(value)?.file_name().to_str() {
        Some(p) => Ok(SteelVal::StringV(p.into())),
        None => Ok(SteelVal::BoolV(false)),
    }
}

/// Checks if this value is a #<Metadata>
#[steel_derive::function(name = "fs-metadata?")]
pub fn is_fs_metadata(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value).is_ok().into_steelval()
}

/// Extract the file metadata from the #<DirEntry>
#[steel_derive::function(name = "read-dir-entry-metadata")]
pub fn read_dir_entry_metadata(value: &SteelVal) -> Result<SteelVal> {
    let entry = DirEntry::as_ref(value)?;
    entry.metadata()?.into_steelval()
}

/// Get the last modified time from the file metadata
#[steel_derive::function(name = "fs-metadata-modified")]
pub fn fs_metadata_modified(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value)?.modified()?.into_steelval()
}

/// Get the last accessed time from the file metadata
#[steel_derive::function(name = "fs-metadata-accessed")]
pub fn fs_metadata_accessed(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value)?.accessed()?.into_steelval()
}

/// Get the created time from the file metadata
#[steel_derive::function(name = "fs-metadata-created")]
pub fn fs_metadata_created(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value)?.created()?.into_steelval()
}

/// Check if this metadata is from a file
#[steel_derive::function(name = "fs-metadata-is-file?")]
pub fn fs_metadata_is_file(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value)?.is_file().into_steelval()
}

/// Check if this metadata is from a directory
#[steel_derive::function(name = "fs-metadata-is-dir?")]
pub fn fs_metadata_is_dir(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value)?.is_dir().into_steelval()
}

/// Check if this metadata is from a symlink
#[steel_derive::function(name = "fs-metadata-is-symlink?")]
pub fn fs_metadata_is_symlink(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value)?.is_symlink().into_steelval()
}

/// Get the length of the file in bytes
#[steel_derive::function(name = "fs-metadata-len")]
pub fn fs_metadata_len(value: &SteelVal) -> Result<SteelVal> {
    Metadata::as_ref(value)?.len().into_steelval()
}

/// Access the file metadata for a given path
#[steel_derive::function(name = "file-metadata")]
pub fn file_metadata(path: &SteelString) -> Result<SteelVal> {
    std::fs::metadata(path.as_str())?.into_steelval()
}

/// Deletes the directory
///
/// (delete-directory! dir) -> void?
///
/// * dir : (string?) - The directory name to delete.
///
/// # Examples
/// ```scheme
/// > (delete-directory! "logs") ;;
/// ```
#[steel_derive::function(name = "delete-directory!")]
pub fn delete_directory(directory: &SteelString) -> Result<SteelVal> {
    std::fs::remove_dir_all(directory.as_str())?;
    Ok(SteelVal::Void)
}

/// Creates the directory
///
/// (create-directory! dir) -> void?
///
/// * dir : (string?) - The directory name to create.
///
/// # Examples
/// ```scheme
/// > (create-directory! "logs") ;;
/// ```
#[steel_derive::function(name = "create-directory!")]
pub fn create_directory(directory: &SteelString) -> Result<SteelVal> {
    std::fs::create_dir_all(directory.as_str())?;

    Ok(SteelVal::Void)
}

/// Recursively copies the contents of the source directory to the destination
///
/// (copy-directory-recursively! source destination) -> void?
///
/// * source : (string?) - The directory to copy.
/// * destination : (string?) - The destination directory into which to copy.
///
/// # Examples
/// ```scheme
/// > (copy-directory-recursively! "logs" "backup") ;;
/// ```
#[steel_derive::function(name = "copy-directory-recursively!")]
pub fn copy_directory_recursively(
    source: &SteelString,
    destination: &SteelString,
) -> Result<SteelVal> {
    copy_recursively(source.as_str(), destination.as_str())?;

    Ok(SteelVal::Void)
}

/// Checks if a path exists
///
/// (path-exists? path) -> bool?
///
/// * path : (string?) - The path to check
///
/// # Examples
/// ```scheme
/// > (path-exists? "logs") ;; => #true
/// > (path-exists? "backup/logs") ;; => #false
/// ```
#[steel_derive::function(name = "path-exists?")]
pub fn path_exists(path: &SteelString) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(Path::new(path.as_ref()).exists()))
}

/// Checks if a path is a file
///
/// (is-file? path) -> bool?
///
/// * path : (string?) - The path to check
///
/// # Examples
/// ```scheme
/// > (is-file? "logs") ;; => #false
/// > (is-file? "logs/today.json") ;; => #true
/// ```
#[steel_derive::function(name = "is-file?")]
pub fn is_file(path: &SteelString) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(Path::new(path.as_ref()).is_file()))
}

/// Checks if a path is a directory
///
/// (is-dir? path) -> bool?
///
/// * path : (string?) - The path to check
///
/// # Examples
/// ```scheme
/// > (is-dir? "logs") ;; => #true
/// > (is-dir? "logs/today.json") ;; => #false
/// ```
#[steel_derive::function(name = "is-dir?")]
pub fn is_dir(path: &SteelString) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(Path::new(path.as_ref()).is_dir()))
}

/// Gets the extension from a path
///
/// (path->extension path) -> (or/c string? void?)
///
/// * path : (string?) - The path to check
///
/// # Examples
/// ```scheme
/// > (path->extension "logs") ;; => void
/// > (path->extension "logs/today.json") ;; => ".json"
/// ```
#[steel_derive::function(name = "path->extension")]
pub fn get_extension(path: &SteelString) -> Result<SteelVal> {
    if let Some(ext) = get_extension_from_filename(path) {
        Ok(SteelVal::StringV(ext.into()))
    } else {
        Ok(SteelVal::Void)
    }
}

/// Gets the filename for a given path
///
/// (file-name path) -> string?
///
/// * path : (string?) - The path to check
///
/// # Examples
/// ```scheme
/// > (file-name "logs") ;; => "logs"
/// > (file-name "logs/today.json") ;; => "today.json"
/// ```
#[steel_derive::function(name = "file-name")]
pub fn file_name(path: &SteelString) -> Result<SteelVal> {
    Ok(SteelVal::StringV(
        Path::new(path.as_str())
            .file_name()
            .and_then(|x| x.to_str())
            .unwrap_or("")
            .into(),
    ))
}

/// Gets the parent directory name for a given path
///
/// (parent-name path) -> string?
///
/// * path : (string?) - The path to check
///
/// # Examples
/// ```scheme
/// > (parent-name "logs") ;; => ""
/// > (parent-name "logs/today.json") ;; => "logs"
/// ```
#[steel_derive::function(name = "parent-name")]
pub fn parent_name(path: &SteelString) -> Result<SteelVal> {
    Ok(SteelVal::StringV(
        Path::new(path.as_str())
            .parent()
            .and_then(|x| x.to_str())
            .unwrap_or("")
            .into(),
    ))
}

/// Returns canonical path with all components normalized
///
/// (canonicalize-path path) -> string?
///
/// * path : (string?) - The path to canonicalize
///
/// # Examples
/// ```scheme
/// > (canonicalize-path "logs") ;; => "/Users/me/Desktop/programming/logs"
/// > (canonicalize-path "logs/today.json") ;; => "/Users/me/Desktop/programming/logs/today.json"
/// ```
#[steel_derive::function(name = "canonicalize-path")]
pub fn canonicalize_path(path: &SteelString) -> Result<SteelVal> {
    let path = path.as_str();
    let canonicalized = if path.len() > 0 && path.starts_with('~') {
        if path.len() > 1 && !path.starts_with("~/") {
            steelerr!(Generic => "references to other users home directories are not supported")?
        } else {
            let mut expanded = env_home::env_home_dir()
                .ok_or_else(throw!(Generic => "could not determine user home directory"))?;
            if path.len() > 2 {
                expanded.push(&path[2..]);
            }
            fs::canonicalize(expanded)?
        }
    } else {
        fs::canonicalize(path)?
    };
    Ok(SteelVal::StringV(
        canonicalized
            .to_str()
            .ok_or_else(throw!(Generic => "path contains non-unicode characters"))?
            .into(),
    ))
}

/// Returns the contents of the directory as a list
///
/// (read-dir path) -> list?
///
/// * path : (string?) - The path to check
///
/// # Examples
/// ```scheme
/// > (read-dir "logs") ;; => '("logs/today.json" "logs/yesterday.json")
/// > (read-dir "empty_dir") ;; => '()
/// ```
#[steel_derive::function(name = "read-dir")]
pub fn read_dir(path: &SteelString) -> Result<SteelVal> {
    let p = Path::new(path.as_ref());
    let iter = p.read_dir()?;
    Ok(SteelVal::ListV(
        iter.into_iter()
            .map(|x| match x?.path().to_str() {
                Some(s) => Ok(SteelVal::StringV(s.into())),
                None => Ok(SteelVal::BoolV(false)),
            })
            .collect::<Result<_>>()?,
    ))
}

/// Outputs the current working directory as a string
///
/// (current-directory) -> string?
///
/// # Examples
/// ```scheme
/// > (current-directory) ;; => "/Users/me/Desktop/programming"
/// ```
#[steel_derive::function(name = "current-directory")]
pub fn current_directory() -> Result<SteelVal> {
    let path = current_dir()?;
    Ok(SteelVal::StringV(path.to_str().unwrap_or("").into()))
}

/// Change the current working directory
///
/// (change-current-directory! path) -> void?
///
/// * path : (string?) - The directory to switch to
///
/// # Examples
/// ```scheme
/// > (change-current-directory! "logs") ;;
/// > (change-current-directory! "..") ;;
/// ```
#[steel_derive::function(name = "change-current-directory!")]
pub fn change_current_directory(path: &SteelString) -> Result<SteelVal> {
    let path = Path::new(path.as_ref());

    set_current_dir(path)?;
    Ok(SteelVal::Void)
}

/// Deletes the file
///
/// (delete-file! path) -> void?
///
/// * path : (string?) - The file to delete
///
/// # Examples
/// ```scheme
/// > (delete-file! "logs/today.json") ;;
/// ```
#[steel_derive::function(name = "delete-file!")]
pub fn delete_file(file: &SteelString) -> Result<SteelVal> {
    std::fs::remove_file(file.as_str())?;
    Ok(SteelVal::Void)
}
