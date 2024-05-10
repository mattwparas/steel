# steel/numbers
### **abs**
Returns the absolute value of the given input
### **exact-integer-sqrt**
Returns an integer that is closest (but not greater than) the square root of an integer and the
remainder.

```scheme
(exact-integer-sqrt x) => '(root rem)
(equal? x (+ (square root) rem)) => #t
```
### **exp**
Returns Euler's number raised to the power of z.
### **magnitude**
Returns the magnitude of the number. For real numbers, this is equvalent to `(abs x)`. For
complex numbers this returns its distance from `(0, 0)` in the complex plane.

```scheme
(magnitude -1/3) => 1/3
(magnitude 3+4i) => 5
```
### **nan?**
Returns `#t` if the real number is Nan.

```scheme
(nan? +nan.0) => #t
(nan? 100000) => #f
```
### **negative?**
Returns `#t` if the real number is negative.

```scheme
(negative?  0) => #f
(negative?  1) => #f
(negative? -1) => #t
```
### **positive?**
Returns `#t` if the real number is positive.

```scheme
(positive?  0) => #f
(positive?  1) => #t
(positive? -1) => #f
```
### **sqrt**
Takes a number and returns the square root. If the number is negative, then a complex number may
be returned.

```scheme
(sqrt  -1)   => 0+1i
(sqrt   4)   => 2
(sqrt   2)   => 1.414..
(sqrt 4/9)   => 2/3
(sqrt -3-4i) => 1-2i
```
### **square**
Squares a number. This is equivalent to `(* x x)`
### **zero?**
Returns `#t` if the real number is 0 or 0.0.

```scheme
(zero? 0  ) => #f
(zero? 0.0) => #t
(zero? 0.1) => #f
```
### *****
### **+**
### **-**
### **/**
### **arithmetic-shift**
### **ceiling**
### **denominator**
### **even?**
### **exact->inexact**
### **exact?**
### **expt**
### **f+**
### **finite?**
### **floor**
### **inexact->exact**
### **inexact?**
### **infinite?**
### **log**
### **numerator**
### **odd?**
### **quotient**
### **round**
