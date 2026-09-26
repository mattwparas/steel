;; `apply` in tail position through the paths other than a direct global tail
;; call: a function value off the stack, and a global tail call with too many
;; arguments to pass in registers. `apply` replaces the calling frame, so its
;; result has to come back through the interpreter, not be dropped.
;; `#%black-box` keeps the inliner from resolving `apply` at compile time.

(define (many . xs) xs)

(define (via-value f n)
  (if (> n 1000000)
      (via-value f (- n 1))
      (f + (list n 2))))

(define ap (#%black-box apply))

(define (via-spilled-global a n)
  (if (> n 1000000)
      (via-spilled-global a (- n 1))
      (ap many a a a a a a a a a a a a a a (list a))))

(define (warm n)
  (when (> n 0)
    (via-value ap 1000010)
    (via-spilled-global 1 1000010)
    (warm (- n 1))))

(warm 500)

(assert! (equal? (via-value ap 1000010) 1000002))
(assert! (equal? (+ 1 (via-value ap 1000010)) 1000003))
(assert! (equal? (length (via-spilled-global 2 1000010)) 15))
(assert! (equal? (+ 1 (length (via-spilled-global 2 1000010))) 16))
(assert! (equal? (list (via-value ap 1000010) (car (via-spilled-global 5 1000010)))
                 '(1000002 5)))
