;; Arithmetic on operands the jit knows are fixnums skips the tag check and, on
;; overflow, exits to the interpreter at that instruction instead of calling a
;; slow path. These check the exit leaves the interpreter exactly where it
;; would have been: operands back on the stack, let slots intact, and nothing
;; before the operation run twice.

(define big 9223372036854775807)
(define small (- 0 9223372036854775807))

;; ---- no overflow: plain results --------------------------------------------------
(define (add-merged c) (let ([x (if c 5 7)]) (+ x 3)))
(define (sub-merged c) (let ([x (if c 5 7)]) (- x 10)))
(define (mul-merged c) (let ([x (if c 5 7)]) (* x 3)))
(assert! (equal? (add-merged #t) 8))
(assert! (equal? (sub-merged #f) -3))
(assert! (equal? (mul-merged #f) 21))

;; results feed the next operation, which also knows they are fixnums
(define (chain c)
  (let* ([a (if c 3 4)]
         [b (+ a 1)]
         [d (* b b)])
    (- d 1)))
(assert! (equal? (chain #t) 15))
(assert! (equal? (chain #f) 24))

;; comparisons
(define (compare c)
  (let ([x (if c 5 7)])
    (list (< x 6) (<= x 5) (> x 6) (>= x 7) (= x 5))))
(assert! (equal? (compare #t) '(#t #t #f #f #t)))
(assert! (equal? (compare #f) '(#f #f #t #t #f)))

;; ---- overflow exits to the interpreter -------------------------------------------------
(define (add-overflow c) (let ([x (if c big 1)]) (+ x 1)))
(define (sub-overflow c) (let ([x (if c small 1)]) (- x 2)))
(define (mul-overflow c) (let ([x (if c big 1)]) (* x 2)))
(assert! (equal? (add-overflow #t) 9223372036854775808))
(assert! (equal? (add-overflow #f) 2))
(assert! (equal? (sub-overflow #t) -9223372036854775809))
(assert! (equal? (mul-overflow #t) 18446744073709551614))
(assert! (equal? (mul-overflow #f) 2))

;; the operation after an overflowed one runs in the interpreter too
(define (overflow-then-more c)
  (let* ([x (if c big 1)]
         [y (+ x 1)])
    (- y 5)))
(assert! (equal? (overflow-then-more #t) 9223372036854775803))

;; a side effect before the overflowing operation happens exactly once
(define counter 0)
(define (bump!) (set! counter (+ counter 1)))
(define (effect-then-overflow c)
  (let ([x (if c big 1)])
    (bump!)
    (+ x 1)))
(set! counter 0)
(assert! (equal? (effect-then-overflow #t) 9223372036854775808))
(assert! (equal? counter 1))

;; overflow inside nested let scopes: the interpreter needs both slots
(define (nested-overflow c)
  (let ([x (if c big 1)])
    (let ([y (if c 2 3)])
      (let ([z (+ x y)])
        (list x y z)))))
(assert! (equal? (nested-overflow #t) (list big 2 9223372036854775809)))
(assert! (equal? (nested-overflow #f) '(1 3 4)))

;; operands already pending on the stack when the overflow happens
(define (pending-overflow c)
  (let ([x (if c big 1)])
    (list 'a "b" (+ x 1) 'd)))
(assert! (equal? (pending-overflow #t) (list 'a "b" 9223372036854775808 'd)))

;; overflow while building a call's arguments
(define (three a b c) (list a b c))
(define (argument-overflow c)
  (let ([x (if c big 1)])
    (three 1 (* x 2) 3)))
(assert! (equal? (argument-overflow #t) (list 1 18446744073709551614 3)))

;; the overflowing operation's result is compared afterwards
(define (overflow-compare c)
  (let ([x (if c big 1)])
    (< 0 (+ x 1))))
(assert! (equal? (overflow-compare #t) #t))

;; inside a loop, so the exit happens part way through
(define (loop-overflow n acc)
  (if (= n 0)
      acc
      (loop-overflow (- n 1) (let ([m (if (> n 1) 4611686018427387904 1)]) (+ m m)))))
(assert! (equal? (loop-overflow 3 0) 2))

;; ---- range facts that rule overflow out ---------------------------------------------
;; under (< x 0) being false, x - 1 cannot overflow, so no exit is emitted
(define (guarded-sub c)
  (let ([x (if c 5 small)])
    (if (< x 0) x (- x 1))))
(assert! (equal? (guarded-sub #t) 4))
(assert! (equal? (guarded-sub #f) small))

;; under (< x 100) being true, x + 1 cannot overflow
(define (guarded-add c)
  (let ([x (if c 5 big)])
    (if (< x 100) (+ x 1) x)))
(assert! (equal? (guarded-add #t) 6))
(assert! (equal? (guarded-add #f) big))

;; the fact only holds on its own side of the test
(define (unguarded-side c)
  (let ([x (if c big 5)])
    (if (< x 100) x (+ x 1))))
(assert! (equal? (unguarded-side #t) 9223372036854775808))
