# steel/identity
### **bool?**
Alias for `boolean?`. Returns true if the value is a boolean.

(bool? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (bool? #false)
#true

> (bool? "hi")
#false
```
### **boolean?**
Returns true if the value is a boolean (`#true` or `#false`).

(boolean? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (boolean? #true)
#true

> (boolean? #false)
#true

> (boolean? 0)
#false
```
### **complex?**
Checks if the given value is a complex number

(complex? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (complex? 3+4i) ;; => #t
> (complex? 42) ;; => #t
> (complex? "hello") ;; => #f
```
### **eof-object?**
Returns `#t` if the value is an EOF object.

(eof-object? any/c) -> bool?
### **exact-integer?**
Checks if the given value is an exact integer

(exact-integer? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (exact-integer? 42) ;; => #t
> (exact-integer? -42) ;; => #t
> (exact-integer? 4.0) ;; => #f
```
### **float?**
Checks if the given value is a floating-point number

(float? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (float? 42) ;; => #f
> (float? 3.14) ;; => #t
> (float? #t) ;; => #f
```
### **function?**
Returns true if the value is a function or callable.

(function? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (function? (lambda (x) x))
#true

> (function? map)
#true

> (function? 42)
#false
```
### **hash?**
Returns true if the value is a hash map.

(hash? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (hash? (hash 'a 10 'b 20))
#true

> (hash? '(a b c))
#false
```
### **int?**
Checks if the given value is an integer, an alias for `integer?`

(int? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (int? 42) ;; => #t
> (int? 3.14) ;; => #f
> (int? "hello") ;; => #f
```
### **integer?**
Checks if the given value is an integer, an alias for `int?`

(integer? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (integer? 42) ;; => #t
> (integer? 3.14) ;; => #f
> (integer? "hello") ;; => #f
```
### **list?**
Returns true if the value is a list.

(list? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (list? '(1 2 3))
#true

> (list? "not-a-list")
#false
```
### **not**
Returns true if the given value is exactly `#false`.

(not value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (not #false)
#true

> (not #true)
#false

> (not "hello")
#false
```
### **number?**
Checks if the given value is a number

(number? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (number? 42) ;; => #t
> (number? "hello") ;; => #f
> (number? 'symbol) ;; => #f
```
### **rational?**
Returns #t if obj is a rational number, #f otherwise.
Rational numbers are numbers that can be expressed as the quotient of two numbers.
For example, 3/4, -5/2, 0.25, and 0 are rational numbers.

(rational? value) -> bool?

* value : any - The value to check

Examples:
```scheme
> (rational? (/ 0.0)) ;; => #f
> (rational? 3.5) ;; => #t
> (rational? 6/10) ;; => #t
> (rational? +nan.0) ;; => #f
```
### **real?**
Checks if the given value is a real number

(real? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (real? 42) ;; => #t
> (real? 3+4i) ;; => #f
> (real? "hello") ;; => #f
```
### **set?**
Returns true if the value is a hash set.

(set? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (set? (hashset 10 20 30 40))
#true

> (set? "abc")
#false
```
### **string?**
Returns true if the value is a string.

(string? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (string? "hello")
#true

> (string? 'foo)
#false
```
### **symbol?**
Returns true if the value is a symbol.

(symbol? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (symbol? 'hello)
#true

> (symbol? "hello")
#false
```
### **vector?**
Returns true if the value is a vector (mutable or immutable).

(vector? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (vector? #(1 2 3))
#true

> (vector? 'foo)
#false
```
### **void?**
Returns true if the value is `void`.

(void? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (void? void)
#true

> (void? 42)
#false
```
### **atom?**
### **char?**
### **continuation?**
### **error-object?**
### **future?**
### **immutable-vector?**
### **mutable-vector?**
### **port?**
### **procedure?**
### **struct?**
