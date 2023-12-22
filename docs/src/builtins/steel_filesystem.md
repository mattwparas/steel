# steel/filesystem
#### steel/filesystem

Filesystem functions, mostly just thin wrappers around the `std::fs` functions in
the Rust std library.
### **delete-directory!**
Deletes the directory
### **copy-directory-recursively!**
Recursively copies the directory from source to destination
### **path->extension**
Gets the extension from a path
### **is-file?**
Checks if a path is a file
### **path-exists?**
Checks if a path exists
### **is-dir?**
Checks if a path is a directory
### **create-directory!**
Creates the directory
### **file-name**
Gets the filename for a given path
### **current-directory**
Check the current working directory
### **read-dir**
Returns the contents of the directory as a list
