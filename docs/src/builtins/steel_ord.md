# steel/ord
Real numbers ordering module.
### **<**
Compares two real numbers to check if the first is less than the second.

(< left right) -> bool?

* left : real? - The first real number to compare.
* right : real? - The second real number to compare.

#### Examples
```scheme
> (< 3 2) ;; => #f
> (< 2 3) ;; => #t
> (< 3/2 1.5) ;; => #f
> (< 2.5 3/2) ;; => #t
```
### **<=**
Compares two real numbers to check if the first is less than or equal to the second.

(<= left right) -> bool?

* left : real? - The first real number to compare.
* right : real? - The second real number to compare.

#### Examples
```scheme
> (<= 3 2) ;; => #f
> (<= 2 3) ;; => #t
> (<= 3/2 1.5) ;; => #t
> (<= 2.5 3/2) ;; => #f
```
### **>**
Compares two real numbers to check if the first is greater than the second.

(> left right) -> bool?

* left : real? - The first real number to compare.
* right : real? - The second real number to compare.

#### Examples
```scheme
> (> 3 2) ;; => #t
> (> 1 1) ;; => #f
> (> 3/2 1.5) ;; => #f
> (> 3/2 1.4) ;; => #t
```
### **>=**
Compares two real numbers to check if the first is greater than or equal to the second.

(>= left right) -> bool?

* left : real? - The first real number to compare.
* right : real? - The second real number to compare.

#### Examples
```scheme
> (>= 3 2) ;; => #t
> (>= 2 3) ;; => #f
> (>= 3/2 1.5) ;; => #t
> (>= 3/2 1.4) ;; => #t
```
