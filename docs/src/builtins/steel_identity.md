# steel/identity
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
### **atom?**
### **bool?**
### **boolean?**
### **char?**
### **continuation?**
### **function?**
### **future?**
### **hash?**
### **list?**
### **mutable-vector?**
### **not**
### **port?**
### **procedure?**
### **set?**
### **string?**
### **struct?**
### **symbol?**
### **vector?**
### **void?**
