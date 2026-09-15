;; Type facts the jit carries across merges and spills. A fixnum fact lets a
;; fast arm unbox without checking the tag, so every shape here pairs a merge
;; or reload that *does* agree on `Int` with one that must not.

(define big 9223372036854775807)

;; ---- if-merges ---------------------------------------------------------------

;; both arms are fixnums
(define (merge-int c) (let ([x (if c 1 2)]) (+ x 10)))
(assert! (equal? (merge-int #t) 11))
(assert! (equal? (merge-int #f) 12))

;; the arms disagree, so the merge is not a fixnum
(define (merge-int-float c) (let ([x (if c 1 2.5)]) (+ x 10)))
(assert! (equal? (merge-int-float #t) 11))
(assert! (equal? (merge-int-float #f) 12.5))

;; one arm is a value of unknown type
(define (merge-int-arg c y) (let ([x (if c 1 y)]) (+ x 1)))
(assert! (equal? (merge-int-arg #t 'unused) 2))
(assert! (equal? (merge-int-arg #f 1.5) 2.5))
(assert! (equal? (merge-int-arg #f big) 9223372036854775808))
(assert! (equal? (with-handler (lambda (e) 'caught) (merge-int-arg #f "s")) 'caught))

;; nested merges, and the merged value used on either side of an operator
(define (merge-nested a b)
  (let ([x (if a (if b 1 2) (if b 3 4))])
    (- 100 x)))
(assert! (equal? (merge-nested #t #t) 99))
(assert! (equal? (merge-nested #f #f) 96))

(define (merge-nested-mixed a b)
  (let ([x (if a (if b 1 2) (if b 3 4.0))])
    (- x 100)))
(assert! (equal? (merge-nested-mixed #t #f) -98))
(assert! (equal? (merge-nested-mixed #f #f) -96.0))

;; a merged fixnum that overflows still has to become a bignum
(define (merge-overflow c) (let ([x (if c big 1)]) (+ x 1)))
(assert! (equal? (merge-overflow #t) 9223372036854775808))
(assert! (equal? (merge-overflow #f) 2))

(define (merge-underflow c) (let ([x (if c (- 0 big) 1)]) (- x 2)))
(assert! (equal? (merge-underflow #t) -9223372036854775809))

;; comparisons against a merged value
(define (merge-compare c y) (let ([x (if c 5 y)]) (< x 10)))
(assert! (equal? (merge-compare #t 'unused) #t))
(assert! (equal? (merge-compare #f 20.5) #f))
;; (not asserting on a non-number here: `<` returns #false for a string instead
;; of raising, in the interpreter too - a separate, pre-existing bug)

;; one arm leaves through a tail call, so only the other reaches the merge
(define (merge-one-arm n)
  (+ 1 (if (> n 0) (merge-one-arm (- n 1)) 0)))
(assert! (equal? (merge-one-arm 5) 6))

;; ---- values spilled across a call and reloaded -----------------------------------

(define (spill-merged f g) (+ (if (f) 1 2) (g)))
(assert! (equal? (spill-merged (lambda () #t) (lambda () 10)) 11))
(assert! (equal? (spill-merged (lambda () #f) (lambda () 1.5)) 3.5))
(assert! (equal? (spill-merged (lambda () #t) (lambda () big)) 9223372036854775808))
(assert! (equal? (with-handler (lambda (e) 'caught) (spill-merged (lambda () #t) (lambda () "s"))) 'caught))

;; the reloaded operand is not a fixnum
(define (spill-unknown f g) (+ (if (f) 1.5 2) (g)))
(assert! (equal? (spill-unknown (lambda () #t) (lambda () 1)) 2.5))
(assert! (equal? (spill-unknown (lambda () #f) (lambda () 1)) 3))

;; two calls, so the first result is spilled across the second
(define (spill-two f) (- (f 1) (f 2)))
(assert! (equal? (spill-two (lambda (x) x)) -1))
(assert! (equal? (spill-two (lambda (x) (* x 1.5))) -1.5))

;; ---- let slots -------------------------------------------------------------------

;; a let slot bound to a fixnum, then retyped by set!
(define (let-retype) (let ([x 1]) (set! x 1.5) (+ x 1)))
(assert! (equal? (let-retype) 2.5))
(define (let-retype-compare) (let ([x 1]) (set! x "s") (string? x)))
(assert! (equal? (let-retype-compare) #t))

;; a fixnum let slot as the right hand side of a comparison against a register
(define (let-rhs-compare a) (let ([b 5]) (< a b)))
(assert! (equal? (let-rhs-compare 1) #t))
(assert! (equal? (let-rhs-compare 7.5) #f))
;; (not asserting on a non-number here: `<` returns #false for a string instead
;; of raising, in the interpreter too - a separate, pre-existing bug)

;; the same slot index reused by a later scope with a different type
(define (let-reuse)
  (let ([a (let ([x 1]) (+ x 1))])
    (let ([y 2.5]) (+ a y))))
(assert! (equal? (let-reuse) 4.5))

;; ---- inside loops, where the facts are reused every iteration ----------------------

(define (loop-merge n acc)
  (if (= n 0)
      acc
      (loop-merge (- n 1) (+ acc (if (even? n) 1 2)))))
(assert! (equal? (loop-merge 10 0) 15))

(define (loop-merge-mixed n acc)
  (if (= n 0)
      acc
      (loop-merge-mixed (- n 1) (+ acc (if (even? n) 1 0.5)))))
(assert! (equal? (loop-merge-mixed 4 0) 3.0))

;; ---- merges join, and only keep what every path established ------------------------
;; an int arm and a float arm meet at "number"
(define (join-number c y) (let ([x (if c 1 (* y 1.5))]) (+ x 1)))
(assert! (equal? (join-number #t 2) 2))
(assert! (equal? (join-number #f 2) 4.0))

;; a type learned on one path only must not survive the merge
(define (one-path-string c x)
  (if c (string-length x) 0)
  (string? x))
(assert! (equal? (one-path-string #t "abc") #t))
(assert! (equal? (one-path-string #f 'sym) #f))

(define (one-path-symbol c x)
  (if c (symbol->string x) "")
  (symbol? x))
(assert! (equal? (one-path-symbol #t 'a) #t))
(assert! (equal? (one-path-symbol #f "not-a-symbol") #f))

;; a let slot's type does not leak into the next scope that reuses the slot
(define (scope-reuse c)
  (let ([a (let ([s "str"]) (string-length s))])
    (let ([t (if c 'sym "str")])
      (list a (symbol? t) (string? t)))))
(assert! (equal? (scope-reuse #t) '(3 #t #f)))
(assert! (equal? (scope-reuse #f) '(3 #f #t)))

;; ---- producers that only sometimes give the type they look like ------------------------
;; read-char gives the eof object at the end of input, not a char
(define (count-chars port n)
  (let ([c (read-char port)])
    (if (eof-object? c) n (count-chars port (+ n 1)))))
(assert! (equal? (count-chars (open-input-string "abc") 0) 3))

(define (count-non-space port n)
  (let ([c (read-char port)])
    (cond [(eof-object? c) n]
          [(char-whitespace? c) (count-non-space port n)]
          [else (count-non-space port (+ n 1))])))
(assert! (equal? (count-non-space (open-input-string "a1 b2") 0) 4))

;; cons onto a non-list is a pair, not a list
(define (cons-kind tail) (let ([p (cons 1 tail)]) (list (list? p) (pair? p))))
(assert! (equal? (cons-kind '(2)) '(#t #t)))
(assert! (equal? (cons-kind 2) '(#f #t)))
