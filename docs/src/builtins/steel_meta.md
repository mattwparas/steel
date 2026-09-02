# steel/meta
### **box**
Creates a mutable box holding the given value. The box is tracked by the
garbage collector, so values stored in it (including ones that form cycles)
are reclaimed safely. Use `unbox` to read the value and `set-box!` to update
it.

(box value) -> box?

* value : any? - The initial value to store in the box.

#### Examples
```scheme
> (define b (box 10)) ;;
> (unbox b) ;; => 10
> (set-box! b 20) ;; => 10
> (unbox b) ;; => 20
```
### **box-strong**
Creates a strong box holding the given value. Use `unbox-strong` to read the
value and `set-strong-box!` to update it.

(box-strong value) -> box-strong?

* value : any? - The initial value to store in the box.

Strong boxes are reference counted and are _not_ tracked by the garbage
collector. Storing a value that (directly or indirectly) refers back to the
box creates a reference count cycle that will never be reclaimed, leaking
memory. Because they skip the GC bookkeeping that `box` requires, strong
boxes are slightly more efficient, so they are the preferred option when you
are certain no cycles can form (or are willing to accept the leak if one
does).

#### Examples
```scheme
> (define b (box-strong 10)) ;;
> (unbox-strong b) ;; => 10
> (set-strong-box! b 20) ;;
> (unbox-strong b) ;; => 20
```
### **command-line**
Returns the command line passed to this process,
including the command name as first argument.
### **current-os!**
Returns the name of the operating system that this Steel runtime was built
for, for example `"linux"`, `"macos"`, or `"windows"`.

(current-os!) -> string?
### **env-var**
Retrieves the value of the given environment variable as a string. Raises an
error if the variable is not set.

(env-var name) -> string?

* name : string?

```scheme
> (env-var "PATH") ;; => "/usr/bin:/bin:..."
```
### **error-object-message**
Returns the message of an error object.

(error-object-message error?) -> string?
### **feature-dylib-build?**
Returns `#true` if this Steel runtime was compiled with support for building
dynamic libraries (the `dylib-build` feature).

(feature-dylib-build?) -> bool?
### **make-weak-box**
Allocates a weak box.

A weak box is similar to a box, but when the garbage collector can prove
that the value of a weak box is only reachable through weak references,
the weak box value will always return #false.

In other words, a weak box does not keep the value contained alive through
a gc collection.
### **maybe-get-env-var**
Retrieves the value of the given environment variable as a string, returning
a `Result` instead of raising if the variable is not set.

(maybe-get-env-var name) -> (Result? string?)

* name : string?
### **path-separator**
Returns the primary path component separator for the current platform as a
string, for example `"/"` on Unix or `"\"` on Windows.

(path-separator) -> string?
### **platform-dll-extension!**
Returns the file extension used for dynamic libraries on the current
platform, for example `"so"`, `"dylib"`, or `"dll"`.

(platform-dll-extension!) -> string?
### **platform-dll-prefix!**
Returns the filename prefix used for dynamic libraries on the current
platform, for example `"lib"` on Linux and macOS, or `""` on Windows.

(platform-dll-prefix!) -> string?
### **set-box!**
Stores a new value inside a box created with `box`, returning the value that
the box held previously.

(set-box! the-box value) -> any?

* the-box : box? - The box to mutate.
* value : any? - The new value to store in the box.

#### Examples
```scheme
> (define b (box 1)) ;;
> (set-box! b 2) ;; => 1
> (unbox b) ;; => 2
```
### **set-strong-box!**
Stores a new value inside a strong box created with `box-strong`.

(set-strong-box! the-box value) -> void?

* the-box : box-strong? - The strong box to mutate.
* value : any? - The new value to store in the box.

Strong boxes are reference counted and are _not_ tracked by the garbage
collector. Storing a value that (directly or indirectly) refers back to the
box creates a reference count cycle that will never be reclaimed, leaking
memory. Because they skip the GC bookkeeping that `box` requires, strong
boxes are slightly more efficient, so they are the preferred option when you
are certain no cycles can form (or are willing to accept the leak if one
does).

#### Examples
```scheme
> (define b (box-strong 1)) ;;
> (set-strong-box! b 2) ;;
> (unbox-strong b) ;; => 2
```
### **steel-home-location**
Returns the path to the Steel home directory - the `STEEL_HOME` location used
to resolve installed packages and cogs - or `#false` if it is not set.

(steel-home-location) -> (or string? #false)
### **target-arch!**
Returns the CPU architecture that this Steel runtime was built for, for
example `"x86_64"` or `"aarch64"`.

(target-arch!) -> string?
### **unbox**
Returns the value stored inside a box created with `box`.

(unbox the-box) -> any?

* the-box : box? - The box to read from.

#### Examples
```scheme
> (define b (box 'a)) ;;
> (unbox b) ;; => 'a
```
### **unbox-strong**
Returns the value stored inside a strong box created with `box-strong`.

(unbox-strong the-box) -> any?

* the-box : box-strong? - The strong box to read from.

Strong boxes are reference counted and are _not_ tracked by the garbage
collector. Storing a value that (directly or indirectly) refers back to the
box creates a reference count cycle that will never be reclaimed, leaking
memory. Because they skip the GC bookkeeping that `box` requires, strong
boxes are slightly more efficient, so they are the preferred option when you
are certain no cycles can form (or are willing to accept the leak if one
does).

#### Examples
```scheme
> (define b (box-strong 'a)) ;;
> (unbox-strong b) ;; => 'a
```
### **weak-box-value**
Returns the value contained in the weak box.
If the garbage collector has proven that the previous content
value of weak-box was reachable only through a weak reference,
then default-value (which defaults to #f) is returned.

```scheme
(define value (make-weak-box 10))
(weak-box-value value) ;; => 10
(set! value #f) ;; Wipe out the previous value
(#%gc-collect)
(weak-box-value value) ;; => #false
```
### **%#interner-memory-usage**
### **%iterator?**
### **Engine::add-module**
### **Engine::clone**
### **Engine::modules->list**
### **Engine::new**
### **Engine::raise_error**
### **active-object-count**
### **arity-object->list**
### **arity?**
### **assert!**
### **attach-contract-struct!**
### **block-on**
### **breakpoint!**
### **bytes->serialized**
### **call-with-current-continuation**
### **call-with-exception-handler**
### **call/cc**
### **callstack-hydrate-names**
### **current-function-span**
### **current-module**
### **current-module-relative**
### **debug-globals**
### **deserialize-value**
### **dump-profiler**
### **emit-expanded**
### **error-with-span**
### **eval**
### **eval!**
### **eval-string**
### **expand!**
### **function-arity**
### **function-name**
### **futures-join-all**
### **get-contract-struct**
### **get-test-mode**
### **inspect**
### **iter-next!**
### **join!**
### **load**
### **load-expanded**
### **local-executor/block-on**
### **make-callstack-profiler**
### **make-struct-type**
### **make-will-executor**
### **memory-address**
### **module->exports**
### **multi-arity?**
### **poll!**
### **raise-error**
### **raise-error-with-span**
### **read!**
### **run!**
### **serialize-value**
### **serialized->bytes**
### **set-env-var!**
### **set-test-mode!**
### **struct->list**
### **value->iterator**
### **value->string**
### **will-execute**
### **will-register**
