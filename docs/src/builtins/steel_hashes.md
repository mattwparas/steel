# steel/hashes
### **md5-hasher**
Creates a new hasher for MD5 digests.

(md5-hasher) -> hasher?
### **md5-hasher->bytes**
Returns the 16-byte MD5 digest of everything fed to the hasher, and resets it.

(md5-hasher->bytes hasher) -> bytes?
### **md5-hasher-update!**
Feeds the bytes of a string (as UTF-8) or a bytevector to the hasher.

(md5-hasher-update! hasher value) -> void?

* hasher : the result of `md5-hasher`
* value : (or/c string? bytes?)
