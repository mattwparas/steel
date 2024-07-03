# steel/filesystem
Filesystem functions, mostly just thin wrappers around the `std::fs` functions in
the Rust std library.
### **canonicalize-path**
Returns canonical path with all components normalized
### **copy-directory-recursively!**
Recursively copies the directory from source to destination
### **create-directory!**
Creates the directory
### **current-directory**
Check the current working directory
### **delete-directory!**
Deletes the directory
### **delete-file!**
Deletes the file
### **file-name**
Gets the filename for a given path
### **is-dir?**
Checks if a path is a directory
### **is-file?**
Checks if a path is a file
### **path->extension**
Gets the extension from a path
### **path-exists?**
Checks if a path exists
### **read-dir**
Returns the contents of the directory as a list
