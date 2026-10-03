;; `exact-integer?`, `integer?` and `vector?` compile to tag checks. Run hot so
;; the jitted versions are the ones checked.
(define values-to-test
  (list 0 -7 123456789012345678901234567890 1.0 -2.0 1.5 +inf.0 -inf.0 +nan.0 -0.0 1/2 (/ 4 2)
        (vector 1 2) (mutable-vector 1 2) (make-vector 3 0) "str" '(1 2) 'sym #\a #t))

(define (classes x)
  (list (exact-integer? x) (integer? x) (vector? x)))

(define expected
  '((#t #t #f) (#t #t #f) (#t #t #f) (#f #t #f) (#f #t #f) (#f #f #f) (#f #f #f) (#f #f #f)
    (#f #f #f) (#f #t #f) (#f #f #f) (#t #t #f) (#f #f #t) (#f #f #t) (#f #f #t) (#f #f #f)
    (#f #f #f) (#f #f #f) (#f #f #f) (#f #f #f)))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1) (map classes values-to-test))))
(assert! (equal? (loop 3000 '()) expected))

;; As an `if` test, which takes the unboxed result directly.
(define (kind x)
  (cond
    [(exact-integer? x) 'exact]
    [(integer? x) 'integral]
    [(vector? x) 'vector]
    [else 'other]))
(define (loop-kind n acc)
  (if (= n 0)
      acc
      (loop-kind (- n 1) (map kind values-to-test))))
(assert! (equal? (loop-kind 3000 '())
                 '(exact exact exact integral integral other other other other integral other exact
                   vector vector vector other other other other other)))
