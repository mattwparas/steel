# #%private/steel/stdlib
**this module is in the prelude and therefore automatically available when running steel.**

### **->**
Syntax:
Alias for `~>`. Prefer to use `~>` over `->`.
### **->>**
Syntax:
Alias for `~>>`. Prefer to use `~>>` over `->>`.
### **and**
Syntax:
If no `expr`s are provided, the the result is #t.

If a single `expr` is provided, then it is in tail position, so the results of
the `and` expression are the results of the `expr`.

Otherwise, the first `expr` is evaluated. If it produces `#f`, the result
of the `and` expression is `#f`. Otherwise, the result is the same as an
`and` expression with the remaining `expr`s in tail position with
respect to the original `and` form.

#### Examples
```scheme
(and) ;; => #t
(and 1) ;; => 1
(and #f (error "should not get here")) ;; => #f
(and #t 5) ;; => 5
```
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
### **flatten**
Recursively flatten an arbitray structure of pairs into a single list.

(flatten any/c) -> list?

#### Examples

```scheme
(flatten '(a (b (c . d)) e ())) ;; => '(a b c d e)
(flatten 'a) => '(a)
```
### **for-each**
Applies a procedure to all elements of a list

(for-each procedure? list?) ;; => void?

#### Examples

```scheme
> (for-each (λ (x) (println x)) '(a b c))
'a
'b
'c
```
### **let\***
Syntax:

```scheme
(let* ([id val-expr] ...) body ...)
```

Like `let`, but evaluates the `val-expr`s one by one.
Each id is bound in the remaining `val-expr` as well
as the `body`s. The `id`s do not need to be distinct;
later bindings will shadow earlier bindings.

#### Examples
```scheme
(let* ([x 1]
       [y (+ x 1)])
    (list y x)) ;; => '(2 1)
```
### **letrec**
Syntax:

```scheme
(letrec ([id val-expr] ...) body ...)
```

Let `let`, but the identifiers are created first, meaning
`id`s within `val-expr`s can reference later `id`s in the
letrec.

#### Examples
```scheme
(letrec ([is-even? (lambda (n)
                      (or (zero? n)
                          (is-odd? (sub1 n))))]
          [is-odd? (lambda (n)
                     (and (not (zero? n))
                          (is-even? (sub1 n))))])
   (is-odd? 11)) ;; => #t
```
### **letrec\***

Syntax:

Alias for `letrec`.
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
### **memv**
Return the first tail of the list, where the car is `eqv?` to the given obj.
Returns `#f`, if no element is found.

This procedure is equivalent to `member`, but using `eqv?` instead of `equal?`.

(memv obj lst) -> (or/c list? #f)

* obj : any/c
* lst : list?

```scheme
(memv #\c '(#\a #\b #\c #\d #\e)) ;; => '(#\c #\d #\e)
(memv 5 '(0 1 2 3 4)) ;; => #f
```
### **or**
Syntax:
If no exprs are provided, then the result is `#false`

If a single expr is provided, then it is in tail position, so the results
of the `or` expressions are the results of the `expr`.

Otherwise, the first `expr` is evaluated. If it produces a value other
than `#f`, that result is the result of the `or` expression. Otherwise,
the result is the same as an `or` expression witih
the remaining `expr`s in tail position with respect to the original
`or` form.

#### Examples
```scheme
(or) ;; => #f
(or 1) ;; => `
(or 5 (error "should not get here")) ;; => 5
(or #f 5) ;; => 5
```
### **unless**
Syntax:

Equivalent to:
```scheme
(when (not test-expr) body ...)
```
### **when**
Syntax:

```scheme
(when test-expr body ...)
```

Evaluates `test-expr`. If the result is `#f`, then the result of the `when`
expression is `#<void>`. Otherwise, the `body`s are evaluated, and the
last `body` is in tail position with respect to the `when` form.

#### Examples
```scheme
(when (positive? -f)
    "found positive") ;; => #<void>

(when (positive? 5)
     10
     20) ;; => 20
```
### **while**
Syntax:

```scheme
(while test body ...)
```

A while loop. Each iteration of the loop evaluates the test
expression, and if it evaluates to a true value, the
body expressions are evaluates sequentially.

```scheme
(while #t (displayln "hello world"))
```
### **~>**
Syntax:

This can be read as "thread-first". It is used to pipe expressions
through to the first argument of the next expression in order to avoid
nesting.

#### Examples
```scheme
(~> 10) ;; => 10
(~> 10 list) ;; equivalent to (list 10)
(~> 10 list car) ;; equivalent to (car (list 10))
(~> 10 list ((lambda (m) (map add1 m)))) ;; => '(11)
```
### **~>>**
Syntax:

This can be read as "thread-last". It is used to pipe expressions
through to the last argument of the next expression in order to avoid
nesting.

#### Examples
```scheme
(~>> 10) ;; => 10
(~>> 10 list) ;; equivalent to (list 10)
(~>> 10 list car) ;; equivalent to (car (list 10))
(~>> 10 list (map add1)) ;; => '(11)
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
