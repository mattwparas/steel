# #%private/steel/stdlib
**this module is in the prelude and therefore automatically available when running steel.**

### **assoc**
Returns the first pair in the given list, where the car element is `equal?`
to the given obj, returning `#f` if nothing was found.

It is an error if the given list is not a list of pairs.

(assoc obj lst) -> (or/c pair? #f)

* obj : any/c
* lst : (listof pair?)

#### Examples

```scheme
(assoc 2 '((1 1) (2 4) (3 9))) ;; => '(2 4)
(assoc 'b '((a 1) (b 2) (c 3))) ;; => '(b 2)
(assoc #\B '((#\a 1) (#\b 2) (#\c 3))) ;; => #f
```
### **assq**
Returns the first pair in the given list, where the car element is `eq?`
to the given obj, returning `#f` if nothing was found.

This procedure is equivalent to `assoc`, but using `eq?` instead of `equal?`.

It is an error if the given list is not a list of pairs.

(assq obj lst) -> (or/c pair? #f)

* obj : any/c
* lst : (listof pair?)

#### Examples

```scheme
(assq 2 '((1 1) (2 4) (3 9))) ;; => '(2 4)
(assq 'b '((a 1) (b 2) (c 3))) ;; => '(b 2)
(assq #\B '((#\a 1) (#\b 2) (#\c 3))) ;; => #f
```
### **assv**
Returns the first pair in the given list, where the car element is `eqv?`
to the given obj, returning `#f` if nothing was found.

This procedure is equivalent to `assoc`, but using `eqv?` instead of `equal?`.

It is an error if the given list is not a list of pairs.

(assv obj lst) -> (or/c pair? #f)

* obj : any/c
* lst : (listof pair?)

#### Examples

```scheme
(assv 2 '((1 1) (2 4) (3 9))) ;; => '(2 4)
(assv 'b '((a 1) (b 2) (c 3))) ;; => '(b 2)
(assv #\B '((#\a 1) (#\b 2) (#\c 3))) ;; => #f
```
### **drop**
Returns the list l after the first n elements.

(drop l n) -> list?

* l : list?
* n : (and/c positive? int?)

#### Examples

```scheme
> (drop '(1 2 3 4) 2) ;; => '(3 4)
> (drop (range 0 10) 6) ;; => '(6 7 8 9)
```
### **filter**
Returns new list, keeping elements from `lst` which applying `pred` to the element
returns #t.


#### Examples

```scheme
(filter even? (range 0 5)) ;; '(0 2 4)
```
### **map**
Applies `func` to the elements of the `lsts` from the first
elements to the last. The `func` argument must accept the same
number of arguments as the number of supplied `lsts`, and all
`lsts` must have the same number of elements. The result is a list
containing each result of `func` in order.

(map func lst . lsts) -> list?

#### Examples
```scheme
(map add1 (range 0 5)) ;; '(1 2 3 4 5)
```
### **\*abort**
### **\*meta-continuation\***
### **\*reset**
### **\*shift**
### **add1**
### **caaaar**
### **caaar**
### **caadar**
### **caaddr**
### **caadr**
### **caar**
### **cadaar**
### **cadadr**
### **cadar**
### **caddar**
### **cadddr**
### **caddr**
### **cadr**
### **call-with-values**
### **cdaaar**
### **cdaar**
### **cdadar**
### **cdaddr**
### **cdadr**
### **cdar**
### **cddaar**
### **cddadr**
### **cddar**
### **cdddar**
### **cdddr**
### **cdddr**
### **cddr**
### **contains?**
### **curry**
### **curry2**
### **even-rec?**
### **flatten**
### **flip**
### **fold**
### **foldl**
### **foldr**
### **force**
### **id**
### **max**
### **mem-helper**
### **min**
### **odd-rec?**
### **reduce**
### **slice**
### **sub1**
### **sum**
### **unfold**
### **values**
### **with-finalizer**
### **zero?**
