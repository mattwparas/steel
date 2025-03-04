# steel/numbers
### **\***
Multiplies the given numbers.

(* . nums) -> number?

* nums : number? - The numbers to multiply. Can have any number of arguments including zero.

#### Examples
```scheme
> (* 5 3) ;; => 15
> (* 10 3 2) ;; => 60
> (*) ;; => 1
```
### **+**
Adds the given numbers.

(+ . nums) -> number?

* nums : number? - The numbers to add. Can have any number of arguments including zero.

#### Examples
```scheme
> (+ 5 3) ;; => 8
> (+ 10 3 2) ;; => 15
> (+) ;; => 0
```
### **-**
Subtracts the given numbers.

(- . nums) -> number?

* nums : number? - The numbers to subtract. Must have at least one number.

#### Examples
```scheme
> (- 5 3) ;; => 2
> (- 10 3 2) ;; => 5
> (- -5) ;; => 5
```
### **/**
Divides the given numbers.

(/ . nums) -> number?

* nums : number? - The numbers to divide. Must have at least one number.

#### Examples
```scheme
> (/ 10 2) ;; => 5
> (/ 10 2 2.0) ;; => 2.5
> (/ 1 3.0) ;; => 0.3333333333333333
> (/ 1 3) ;; => 1/3
```
### **abs**
Computes the absolute value of the given number.

(abs number) -> number?

* number : number? - The number to compute the absolute value of.

#### Examples
```scheme
> (abs 42) ;; => 42
> (abs -42) ;; => 42
> (abs 0) ;; => 0
```
### **acos**
Returns the arccosine, or inverse cosine, of a value; output is in radians.

(acos n) -> number?

* n : number? - The input value is the cosine of the angle you want and must be from -1 to 1.

#### Examples
```scheme
> (acos -1) ;; => 3.141592653589793
> (acos 0) ;; => 1.5707963267948966
> (acos 0.5) ;; => 1.0471975511965976
> (acos 2) ;; => +nan.0
```
### **asin**
Returns the arcsine, or inverse sine, of a value; output is in radians.

(asin n) -> number?

* n : number? - The input value is the sine of the angle you want and must be from -1 to 1.

#### Examples
```scheme
> (asin -1) ;; => -1.5707963267948966
> (asin 0) ;; => 0
> (asin 0.5) ;; => 0.5235987755982988
> (asin 2) ;; => +nan.0
```
### **atan**
Returns the arctangent, or inverse tangent, of a value; output is in radians.

(atan n) -> number?

* n : number? - The input value is the tangent of the angle you want.

#### Examples
```scheme
> (atan -1) ;; => -0.7853981633974483
> (atan 0) ;; => 0
> (atan 0.5) ;; => 0.46364760900080615
> (atan 2) ;; => 1.1071487177940906
```
### **ceiling**
Rounds the given number up to the nearest integer not less than it.

(ceiling number) -> integer?

* number : number? - The number to round up.

#### Examples
```scheme
> (ceiling 42) ;; => 42
> (ceiling 42.1) ;; => 43
> (ceiling -42.1) ;; => -42
```
### **cos**
Returns the cosine value of the input angle, measured in radians.

(cos n) -> number?

* n : number? - The input angle, in radians.

#### Examples
```scheme
> (cos 0) ;; => 1
> (cos 1) ;; => 0.5403023058681398
> (cos 2.0) ;; => -0.4161468365471424
> (cos 3.14) ;; => -0.9999987317275395
```
### **denominator**
Retrieves the denominator of the given rational number.

(denominator number) -> integer?

* number : number? - The rational number to retrieve the denominator from.

#### Examples
```scheme
> (denominator 1/2) ;; => 2
> (denominator 3/4) ;; => 4
> (denominator 4) ;; => 1
```
### **exact**
Returns the input value if it is an exact number, otherwise raises an error.

(exact n) -> number?

* n : number? - The value to check for exactness.

#### Examples
```scheme
> (exact 5) ;; => 5
> (exact 5/3) ;; => 5/3
```
### **exact->inexact**
Converts an exact number to an inexact number.

(exact->inexact num) -> number?

* num : number? - The number to convert from exact to inexact.

#### Examples
```scheme
> (exact->inexact 10) ;; => 10
> (exact->inexact 1/2) ;; => 0.5
> (exact->inexact 1+2i) ;; => 1+2i
```
### **exact-integer-sqrt**
Computes the integer square root of the given non-negative integer.

(exact-integer-sqrt number) -> (integer? integer?)

* number : (and/c integer? positive?) - The non-negative integer to compute the square root for.

#### Examples
```scheme
> (exact-integer-sqrt 25) ;; => (5 0)
> (exact-integer-sqrt 35) ;; => (5 10)
```
### **exact?**
Checks if the given value is exact.

(exact? val) -> boolean?

* val : any - The value to check for exactness.

#### Examples
```scheme
> (exact? 42) ;; => #t
> (exact? 3.14) ;; => #f
> (exact? "hello") ;; => #f
```
### **exp**
Returns Euler’s number raised to the power of z.

(exp z) -> number?

* z : number? - The number to raise e to the power of.

#### Examples
```scheme
> (exp 0) ;; => 1
> (exp 2) ;; => 7.38905609893065
> (exp 1.5) ;; => 4.4816890703380645
```
### **expt**
Raises the left operand to the power of the right operand.

(expt base exponent) -> number?

* base : number? - The base number.
* exponent : number? - The exponent to raise the base to.

#### Examples
```scheme
> (expt 2 3) ;; => 8
> (expt 2.0 0.5) ;; => 1.4142135623730951
> (expt 9 0.5) ;; => 3
```
### **finite?**
Returns `#t` if the given number is finite.

(finite? number) -> boolean?

* number : number? - The number to check for finiteness.

#### Examples
```scheme
> (finite? 42) ;; => #t
> (finite? 0.1) ;; => #t
> (finite? +inf.0) ;; => #f
> (finite? -inf.0) ;; => #f
> (finite? +nan.0) ;; => #f
```
### **floor**
Computes the largest integer less than or equal to the given number.

(floor number) -> number?

* number : number? - The number to compute the floor for.

#### Examples
```scheme
> (floor 3.14) ;; => 3
> (floor 4.99) ;; => 4
> (floor -2.5) ;; => -3
```
### **inexact->exact**
Converts an inexact number to an exact number.

(inexact->exact num) -> number?

* num : number? - The number to convert from inexact to exact.

#### Examples
```scheme
> (inexact->exact 10.0) ;; => 10
> (inexact->exact 1.5) ;; => 3/2
> (inexact->exact 1.5+2.5i) ;; => 3/2+5/2i
```
### **inexact?**
Checks if the given value is inexact.

(inexact? val) -> boolean?

* val : any - The value to check for inexactness.

#### Examples
```scheme
> (inexact? 42) ;; => #f
> (inexact? 3.14) ;; => #t
```
### **infinite?**
Returns `#t` if the given number is infinite.

(infinite? number) -> boolean?

* number : number? - The number to check for infiniteness.

#### Examples
```scheme
> (infinite? 42) ;; => #f
> (infinite? -nan.0) ;; => #f
> (infinite? +inf.0) ;; => #t
```
### **log**
Computes the natural logarithm of the given number.

(log number [base]) -> number?

* number : number? - The number to compute the logarithm for.
* base : number? - The base of the logarithm. If not provided, defaults to Euler's number (e).

#### Examples
```scheme
> (log 10) ;; => 2.302585092994046
> (log 100 10) ;; => 2
> (log 27 3) ;; => 3
```
### **magnitude**
Computes the magnitude of the given number.

(magnitude number) -> number?

* number : number? - The number to compute the magnitude for.

#### Examples
```scheme
> (magnitude 3+4i) ;; => 5
> (magnitude 5) ;; => 5
> (magnitude -5) ;; => 5
```
### **modulo**
Returns the euclidean remainder of the division of the first number by the second
This differs from the remainder operator when using negative numbers.

(modulo n m) -> integer?

* n : integer?
* m : integer?

#### Examples
```scheme
> (modulo 10 3) ;; => 1
> (modulo -10 3) ;; => 2
> (modulo 10 -3) ;; => -2
> (module -10 -3) ;; => -1
```
### **nan?**
Returns `#t` if the real number is Nan.

(nan? value) -> boolean?

* value : real? - The value to check

```scheme
(nan? +nan.0) => #t
(nan? 100000) => #f
```
### **negative?**
Checks if the given real number is negative.

(negative? num) -> boolean?

* num : real? - The real number to check for negativity.

#### Examples
```scheme
> (negative? 0) ;; => #f
> (negative? 1) ;; => #f
> (negative? -1) ;; => #t
```
### **numerator**
Retrieves the numerator of the given rational number.

(numerator number) -> number?

* number : number? - The rational number to retrieve the numerator from.

#### Examples
```scheme
> (numerator 3/4) ;; => 3
> (numerator 5/2) ;; => 5
> (numerator -2) ;; => -2
```
### **positive?**
Checks if the given real number is positive.

(positive? num) -> boolean?

* num : real? - The real number to check for positivity.

#### Examples
```scheme
> (positive? 0) ;; => #f
> (positive? 1) ;; => #t
> (positive? -1) ;; => #f
```
### **quotient**
Returns quotient of dividing numerator by denomintator.

(quotient numerator denominator) -> integer?

* numerator : integer? - The numerator.
* denominator : integer? - The denominator.

#### Examples
```scheme
> (quotient 11 2) ;; => 5
> (quotient 10 2) ;; => 5
> (quotient -10 2) ;; => -5
```
### **remainder**
Returns the arithmetic remainder of the division of the first number by the second.
This differs from the modulo operator when using negative numbers.

(remainder n m) -> integer?

* n : integer?
* m : integer?

#### Examples
```scheme
> (remainder 10 3) ;; => 1
> (remainder -10 3) ;; => -1
> (remainder 10 -3) ;; => 1
> (remainder -10 -3) ;; => -1
```
### **round**
Rounds the given number to the nearest integer.

(round number) -> number?

* number : number? - The number to round.

#### Examples
```scheme
> (round 3.14) ;; => 3
> (round 4.6) ;; => 5
> (round -2.5) ;; => -3
```
### **sin**
Returns the sine value of the input angle, measured in radians.

(sin n) -> number?

* n : number? - The input angle, in radians.

#### Examples
```scheme
> (sin 0) ;; => 0
> (sin 1) ;; => 0.8414709848078965
> (sin 2.0) ;; => 0.9092974268256817
> (sin 3.14) ;; => 0.0015926529164868282
```
### **sqrt**
Computes the square root of the given number.

(sqrt number) -> number?

* number : number? - The number to compute the square root for.

#### Examples
```scheme
> (sqrt 4) ;; => 2
> (sqrt 2) ;; => 1.4142135623730951
> (sqrt -1) ;; => 0+1i
```
### **square**
Computes the square of the given number.

(square number) -> number?

* number : number? - The number to square.

#### Examples
```scheme
> (square 5) ;; => 25
> (square -3) ;; => 9
> (square 2.5) ;; => 6.25
```
### **tan**
Returns the tangent value of the input angle, measured in radians.

(tan n) -> number?

* n : number? - The input angle, in radians.

#### Examples
```scheme
> (tan 0) ;; => 0
> (tan 1) ;; => 1.557407724654902
> (tan 2.0) ;; => -2.185039863261519
> (tan 3.14) ;; => -0.0015926549364072232
```
### **zero?**
Checks if the given real number is zero.

(zero? num) -> boolean?

* num : real? - The number to check for zero.

#### Examples
```scheme
> (zero? 0) ;; => #t
> (zero? 0.0) ;; => #t
> (zero? 0.1) ;; => #f
```
### **arithmetic-shift**
### **even?**
### **f+**
### **odd?**
