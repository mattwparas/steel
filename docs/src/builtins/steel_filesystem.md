# steel/filesystem
Filesystem functions, mostly just thin wrappers around the `std::fs` functions in
the Rust std library.
### **canonicalize-path**
Returns canonical path with all components normalized.

(canonicalize-path path) -> string?

* path : string? - The path to canonicalize.

#### Examples
```scheme
> (canonicalize-path "logs") ;; => "/Users/me/Desktop/programming/logs"
> (canonicalize-path "logs/today.json") ;; => "/Users/me/Desktop/programming/logs/today.json"
```
### **change-current-directory!**
Change the current working directory

(change-current-directory! path) -> void?

* path : (string?) - The directory to switch to

#### Examples
```scheme
> (change-current-directory! "logs") ;;
> (change-current-directory! "..") ;;
```
### **copy-directory-recursively!**
Recursively copies the contents of the source directory to the destination.

(copy-directory-recursively! source destination) -> void?

* source : string? - The directory to copy.
* destination : string? - The destination directory into which to copy.

#### Examples
```scheme
> (copy-directory-recursively! "logs" "backup") ;;
```
### **create-directory!**
Creates the directory

(create-directory! dir) -> void?

* dir : (string?) - The directory name to create.

#### Examples
```scheme
> (create-directory! "logs") ;;
```
### **current-directory**
Outputs the current working directory as a string

(current-directory) -> string?

#### Examples
```scheme
> (current-directory) ;; => "/Users/me/Desktop/programming"
```
### **delete-directory!**
Deletes the directory

(delete-directory! dir) -> void?

* dir : (string?) - The directory name to delete.

#### Examples
```scheme
> (delete-directory! "logs") ;;
```
### **delete-file!**
Deletes the file

(delete-file! path) -> void?

* path : (string?) - The file to delete

#### Examples
```scheme
> (delete-file! "logs/today.json") ;;
```
### **file-metadata**
Access the file metadata for a given path
### **file-name**
Gets the filename for a given path.

(file-name path) -> string?

* path : string? - The path from which to get the file name.

#### Examples
```scheme
> (file-name "logs") ;; => "logs"
> (file-name "logs/today.json") ;; => "today.json"
```
### **fs-metadata-accessed**
Get the last accessed time from the file metadata
### **fs-metadata-created**
Get the created time from the file metadata
### **fs-metadata-is-dir?**
Check if this metadata is from a directory
### **fs-metadata-is-file?**
Check if this metadata is from a file
### **fs-metadata-is-symlink?**
Check if this metadata is from a symlink
### **fs-metadata-len**
Get the length of the file in bytes
### **fs-metadata-modified**
Get the last modified time from the file metadata
### **fs-metadata?**
Checks if this value is a #<Metadata>
### **is-dir?**
Checks if a path is a directory.

(is-dir? path) -> bool?

* path : string? - The path to check.

#### Examples
```scheme
> (is-dir? "logs") ;; => #true
> (is-dir? "logs/today.json") ;; => #false
```
### **is-file?**
Checks if a path is a file.

(is-file? path) -> bool?

* path : string? - The path to check.

#### Examples
```scheme
> (is-file? "logs") ;; => #false
> (is-file? "logs/today.json") ;; => #true
```
### **parent-name**
Gets the parent directory name for a given path.

(parent-name path) -> string?

* path : string? - The path from which to get the parent.

#### Examples
```scheme
> (parent-name "logs") ;; => ""
> (parent-name "logs/today.json") ;; => "logs"
```
### **path->extension**
Gets the extension from a path.

(path->extension path) -> (or/c string? void?)

* path : string? - The path from which to get the extension.

#### Examples
```scheme
> (path->extension "logs") ;; => void
> (path->extension "logs/today.json") ;; => ".json"
```
### **path-exists?**
Checks if a path exists.

(path-exists? path) -> bool?

* path : (string?) - The path to check

#### Examples
```scheme
> (path-exists? "logs") ;; => #true
> (path-exists? "backup/logs") ;; => #false
```
### **read-dir**
Returns the contents of the directory as a list

(read-dir path) -> list?

* path : string? - The path to the directory.

#### Examples
```scheme
> (read-dir "logs") ;; => '("logs/today.json" "logs/yesterday.json")
> (read-dir "empty_dir") ;; => '()
```
### **read-dir-entry-file-name**
Returns the file name from a given read-dir-entry.
### **read-dir-entry-is-dir?**
Checks whether the read dir entry is a directory.

(read-dir-entry-is-dir? value) -> bool?

* value : read-dir-iter-entry?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define next (read-dir-iter-next! my-iter))

(read-dir-entry-path) ;; => "src/lib.rs"
(read-dir-entry-is-dir? next) ;; #false - because this is a file

```
### **read-dir-entry-is-file?**
Checks whether the read dir entry is a file.

(read-dir-entry-is-dir? value) -> bool?

* value : read-dir-iter-entry?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define next (read-dir-iter-next! my-iter))

(read-dir-entry-path) ;; => "src/lib.rs"
(read-dir-entry-is-dir? next) ;; #true - because this is a file

```
### **read-dir-entry-is-symlink?**
Checks whether the read dir entry is a symlink.
### **read-dir-entry-metadata**
Extract the file metadata from the #<DirEntry>
### **read-dir-entry-path**
Returns the path from a given read-dir-entry.
### **read-dir-iter**
Creates an iterator over the contents of the given directory.
The given path must be a directory.

(read-dir-iter dir) -> #<ReadDir>

* dir : (is-dir?) - the directory to iterate over

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(read-dir-iter-next! my-iter) ;; => #<DirEntry> src/lib.rs
(read-dir-iter-next! my-iter) ;; => #<DirEntry> src/main.rs
(read-dir-iter-next! my-iter) ;; => #false
```
### **read-dir-iter-entry?**
Checks whether the given value is a #<DirEntry>

(read-dir-iter-entry? value) -> bool?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define next (read-dir-iter-next! my-iter))
(read-dir-iter-entry? next) ;; => #true
```
### **read-dir-iter-next!**
Reads one entry from the iterator. Reads a `ReadDir` struct.

(read-dir-iter-next! read-dir-iter) -> #<DirEntry>

* dir : (read-dir-iter?) - the directory to iterate over

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define nex-entry (read-dir-iter-next! my-iter)) ;; => #<DirEntry> src/lib.rs
(read-dir-entry-is-dir? next-entry) ;; => #false
(read-dir-entry-is-file? next-entry) ;; => #true
(read-dir-entry-file-name) ;; => "lib.rs"
```
### **read-dir-iter?**
Checks whether the given value is a #<ReadDir>

(read-dir-iter? value) -> bool?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(read-dir-iter? my-iter) ;; => #true
(read-dir-iter "not an iter") ;; => #false
```
### **glob**
### **glob-iter-next!**
### **path->string**
