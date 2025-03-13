use crate::rvals::{Custom, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::{steelerr, stop, throw};
use dirs;
use std::env::{current_dir, set_current_dir};
use std::path::{Path, PathBuf};

use std::fs;
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

impl Custom for PathBuf {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("#<Path:{:?}>", self)))
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
        .register_native_fn_definition(DELETE_FILE_DEFINITION);
    module
}

#[steel_derive::define_module(name = "steel/filesystem")]
pub fn fs_module_sandbox() -> BuiltInModule {
    BuiltInModule::new("steel/filesystem")
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
            let mut expanded = dirs::home_dir()
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
    if p.is_dir() {
        let iter = p.read_dir();
        match iter {
            Ok(i) => Ok(SteelVal::ListV(
                i.into_iter()
                    .map(|x| match x?.path().to_str() {
                        Some(s) => Ok(SteelVal::StringV(s.into())),
                        None => Ok(SteelVal::BoolV(false)),
                    })
                    .collect::<Result<_>>()?,
            )),
            Err(e) => stop!(Generic => e.to_string()),
        }
    } else {
        stop!(TypeMismatch => "read-dir expected a dir, found a file: {}", path)
    }
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
