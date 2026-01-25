# steel/process
### **child-stderr**
Get a handle to the stderr handle of the child process. The process
must have been started with the `with-stderr-piped` option for this
to be available, otherwise stderr will be inherited. This will return
false if the handle has already been consumed.

(child-stderr subprocess) -> (or input-port? #false)

subprocess : ChildProcess?
### **child-stdin**
Get a handle to the stdin handle of the child process. The process
must have been started with the `with-stdin-piped` option for this
to be available, otherwise stdin will be inherited. This will return
false if the handle has already been consumed.

(child-stdout subprocess) -> (or input-port? #false)

subprocess : ChildProcess?
### **child-stdout**
Get a handle to the stdout handle of the child process. The process
must have been started with the `with-stdout-piped` option for this
to be available, otherwise stdout will be inherited. This will return
false if the handle has already been consumed.

(child-stdout subprocess) -> (or output-port? #false)

subprocess : ChildProcess?

```scheme
(define handle (~> (command "/bin/ls" '())
                   with-stdout-piped
                   spawn-process
                   unwrap-ok))
(read-port-to-string (child-stdout handle)) ;; The resulting string
```
### **command**
Create a `CommandBuilder` from a command and a list of arguments. Used to spawn
a subprocess.

(command cmd args) -> CommandBuilder?

* cmd : string?
* args : (listof string?)

```scheme
> (spawn-process (command "echo" (list "hello" "world")))
```
### **kill**
Terminate the subprocess.

(subprocess-kill subprocess)

subprocess : ChildProcess?
### **process-wait**
Alias of `wait`.
### **set-current-dir!**
Alias of `with-current-dir`.
### **set-env-var!**
Alias of `with-env-var`.
### **spawn-process**
Spawn the given process. Returns a result indicating whether the process was
able to be spawned.

(spawn-process process) -> (Result? ChildProcess?)

* process : CommandBuilder?

```scheme
> (require "steel/result")
> (define spawned (spawn-process (command "/bin/ls" '()))) ;; => (Ok #<steel::primitives::process::ChildProcess>)
> (define child (unwrap-ok spawned))
```
### **subprocess-kill**
Alias of `kill`.
### **wait**
Wait for the subprocess to finish. Returns a result with the status code
of the awaited subprocess.

(wait process) -> (Result? int?)

* process : ChildProcess?

```scheme
> (~> (command "echo" (list "hello"))
      spawn-process
      unwrap-ok
      wait)
```
### **with-cleared-env-vars**
Removes all environment variables for the child.

(with-cleared-env-vars process) -> CommandBuilder?

* process - CommandBuilder?

```scheme
> (define pb (command "echo" (list "hello")))
> (~> (command "echo" (list "hello"))
      (with-cleared-env-vars "FOO")
      spawn-process
      unwrap-ok
      wait)
```
### **with-current-dir**
Sets the current directory for the child. `set-current-dir!` is an alias.

(with-current-dir process dir) -> CommandBuilder?

* process - CommandBuilder?
* dir - string?

```scheme
> (define pb (command "echo" (list "hello")))
> (with-current-dir pb "/home/foo")
> (~> (command "echo" (list "hello"))
      (with-current-dir "/home/foo")
      spawn-process
      unwrap-ok
      wait)
```
### **with-env-var**
Sets an environment variable for the child. `set-env-var!` is an alias.

(with-env-var process key value) -> CommandBuilder?

* process - CommandBuilder?
* key - string?
* value - string?

```scheme
> (define pb (command "echo" (list "hello")))
> (~> (command "echo" (list "hello"))
      (with-env-var "FOO" "BAR")
      spawn-process
      unwrap-ok
      wait)
```
### **with-stderr**
Redirect stderr from the process to the given port

(with-stderr process port) -> CommandBuilder?

* process : CommandBuilder?
* port : (and output-port? file-port?)

```scheme
> (define output (open-output-file "test.txt"))
> (~> (command "echo" (list "hello"))
      (with-stderr output)
      spawn-process
      unwrap-ok
      wait)
```
### **with-stderr-piped**
Constructs a pipe to be arranged to connect to stderr.

(with-stderr-piped process) -> CommandBuilder?

* process : CommandBuilder?

```scheme
> (~> (command "echo" (list "hello"))
      with-stderr-piped
      spawn-process
      unwrap-ok
      wait)
```
### **with-stdin**
Redirect stdin from the process to the given port

(with-stdin process port) -> CommandBuilder?

* process : CommandBuilder?
* port : (and input-port? file-port?)

```scheme
> (define output (open-input-file "test.txt"))
> (~> (command "echo" (list "hello"))
      (with-stdin output)
      spawn-process
      unwrap-ok
      wait)
```
### **with-stdin-piped**
Constructs a pipe to be arranged to connect to stdin.

(with-stdin-piped process) -> CommandBuilder?

* process : CommandBuilder?

```scheme
> (~> (command "echo" (list "hello"))
      with-stdin-piped
      spawn-process
      unwrap-ok
      wait)
```
### **with-stdout**
Redirect stdout from the process to the given port

(with-stdout process port) -> CommandBuilder?

* process : CommandBuilder?
* port : (and output-port? file-port?)

```scheme
> (define output (open-output-file "test.txt"))
> (~> (command "echo" (list "hello"))
      (with-stdout output)
      spawn-process
      unwrap-ok
      wait)
```
### **with-stdout-piped**
Constructs a pipe to be arranged to connect to stdout.

(with-stdout-piped process) -> CommandBuilder?

* process : CommandBuilder?

```scheme
> (~> (command "echo" (list "hello"))
      with-stdout-piped
      spawn-process
      unwrap-ok
      wait)
```
### **without-env-var**
Removes an environment variable for the child.

(with-env-var process key) -> CommandBuilder?

* process - CommandBuilder?
* key - string?

```scheme
> (define pb (command "echo" (list "hello")))
> (~> (command "echo" (list "hello"))
      (without-env-var "FOO")
      spawn-process
      unwrap-ok
      wait)
```
### **ChildProcess?**
### **CommandBuilder?**
### **command-builder?**
### **set-stdout-piped!**
### **subprocess?**
### **wait->stdout**
### **which**
