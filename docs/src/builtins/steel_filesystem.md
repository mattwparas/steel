# steel/filesystem
Filesystem functions, mostly just thin wrappers around the `std::fs` functions in
the Rust std library.
### **canonicalize-path**
Returns canonical path with all components normalized

(canonicalize-path path) -> string?

* path : (string?) - The path to canonicalize

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
Recursively copies the contents of the source directory to the destination

(copy-directory-recursively! source destination) -> void?

* source : (string?) - The directory to copy.
* destination : (string?) - The destination directory into which to copy.

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
### **file-name**
Gets the filename for a given path

(file-name path) -> string?

* path : (string?) - The path to check

#### Examples
```scheme
> (file-name "logs") ;; => "logs"
> (file-name "logs/today.json") ;; => "today.json"
```
### **is-dir?**
Checks if a path is a directory

(is-dir? path) -> bool?

* path : (string?) - The path to check

#### Examples
```scheme
> (is-dir? "logs") ;; => #true
> (is-dir? "logs/today.json") ;; => #false
```
### **is-file?**
Checks if a path is a file

(is-file? path) -> bool?

* path : (string?) - The path to check

#### Examples
```scheme
> (is-file? "logs") ;; => #false
> (is-file? "logs/today.json") ;; => #true
```
### **parent-name**
Gets the parent directory name for a given path

(parent-name path) -> string?

* path : (string?) - The path to check

#### Examples
```scheme
> (parent-name "logs") ;; => ""
> (parent-name "logs/today.json") ;; => "logs"
```
### **path->extension**
Gets the extension from a path

(path->extension path) -> (or/c string? void?)

* path : (string?) - The path to check

#### Examples
```scheme
> (path->extension "logs") ;; => void
> (path->extension "logs/today.json") ;; => ".json"
```
### **path-exists?**
Checks if a path exists

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

* path : (string?) - The path to check

#### Examples
```scheme
> (read-dir "logs") ;; => '("logs/today.json" "logs/yesterday.json")
> (read-dir "empty_dir") ;; => '()
```
