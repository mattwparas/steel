;; A callee that assigns its own parameter, inlined into a caller that passes
;; one of its variables: the assignment must stay on the callee's copy. Copy
;; propagation used to replace the copy with the caller's variable, so the
;; `set!` landed on the caller's `i`.
(define (inc! i)
  (set! i (+ i 1))
  i)

(define (caller i)
  (list i (inc! i) i))

;; The same once copy propagation has already moved the `set!` onto the
;; parameter: `y` is a copy of `i`.
(define (boxed-inc i)
  (let ([y i])
    (set! y (+ y 1))
    y))

(define (caller2 i)
  (list i (boxed-inc i) i))

;; Opaque to constant folding, so the callers really run.
(define x (string->number (list->string (list #\1 #\0))))

(assert! (equal? (caller x) '(10 11 10)))
(assert! (equal? (caller2 x) '(10 11 10)))

;; And in a loop, where it skipped every other iteration.
(define (count-up i n acc)
  (if (< i n)
      (count-up (+ i 1) n (cons (boxed-inc i) acc))
      (reverse acc)))

(assert! (equal? (count-up 0 5 '()) '(1 2 3 4 5)))

;; A copy must not see a later assignment to its source either.
(define (snapshot i)
  (let ([before i])
    (set! i (* i 2))
    (list before i)))

(assert! (equal? (snapshot x) '(10 20)))
