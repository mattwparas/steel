# steel/equality
### **eq?**
Checks two values for pointer equality, i.e. returns #t if the two values
refer to the same object.

#### Examples
```scheme
(eq? 'yes 'yes) ;; => #t
(eq? 'yes 'no) ;; => #f
(eq? (* 6 7) 42) ;; => #t
(eq? (list 10) (list 10)) ;; => #f
```
### **equal?**
Checks two values for value equality, i.e. returns #t if the two values
are the same data type, and also are equal recursively structurally.

#### Examples
```scheme
(equal? 'yes 'yes) ;; => #t
(equal? 'yes 'no) ;; => #f
(equal? (* 6 7) 42) ;; => #t
(equal? (list 10) (list 10)) ;; => #t
```
### **=**
### **eqv?**
