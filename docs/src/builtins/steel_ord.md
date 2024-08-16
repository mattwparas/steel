# steel/ord
Real numbers ordering module.
### **<**
Compares real numbers to check if any number is less than the subsequent.

(< x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (< 1) ;; => #t
> (< 3 2) ;; => #f
> (< 2 3) ;; => #t
> (< 3/2 1.5) ;; => #f
> (< 2.5 3/2) ;; => #t
> (< 2 5/2 3) ;; #t
```
### **<=**
Compares real numbers to check if any number is less than or equal than the subsequent.

(<= x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (<= 1) ;; => #t
> (<= 3 2) ;; => #f
> (<= 2 3) ;; => #t
> (<= 3/2 1.5) ;; => #t
> (<= 2.5 3/2) ;; => #f
> (<= 2 6/2 3) ;; #t
```
### **>**
Compares real numbers to check if any number is greater than the subsequent.

(> x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (> 1) ;; => #t
> (> 3 2) ;; => #t
> (> 1 1) ;; => #f
> (> 3/2 1.5) ;; => #f
> (> 3/2 1.4) ;; => #t
> (> 3 4/2 1) ;; #t
```
### **>=**
Compares real numbers to check if any number is greater than or equal than the subsequent.

(>= x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (>= 1) ;; => #t
> (>= 3 2) ;; => #t
> (>= 2 3) ;; => #f
> (>= 3/2 1.5) ;; => #t
> (>= 3/2 1.4) ;; => #t
> (>= 2 4/2 1) ;; #t
```
