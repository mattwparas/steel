;; equal? compares vectors element by element, the same whichever kinds of
;; vector they are, through nesting and cycles.

(define nan (/ 0.0 0.0))

(assert! (equal? (mutable-vector 1 #\a "s" 's #t) (immutable-vector 1 #\a "s" 's #t)))
(assert! (equal? (immutable-vector 1 2) (mutable-vector 1 2)))
(assert! (not (equal? (mutable-vector 1 2) (mutable-vector 1 2.0))))
(assert! (not (equal? (mutable-vector 1 2) (mutable-vector 1 2 3))))
(assert! (not (equal? (mutable-vector "a") (mutable-vector "b"))))
(assert! (not (equal? (mutable-vector nan) (mutable-vector nan))))
(assert! (equal? (mutable-vector (list 1 2) (immutable-vector 3)) (immutable-vector (list 1 2) (mutable-vector 3))))
(assert! (not (equal? (mutable-vector (list 1 2)) (mutable-vector (list 1 3)))))
(assert! (equal? (make-vector 400 #\x) (make-vector 400 #\x)))
(assert! (not (equal? (make-vector 400 0) (let ([v (make-vector 400 0)]) (vector-set! v 399 1) v))))

(define shared (mutable-vector 1 2))
(assert! (equal? (mutable-vector shared shared) (mutable-vector (mutable-vector 1 2) (mutable-vector 1 2))))

(define (cycle second)
  (let ([v (mutable-vector 1 second #f)])
    (vector-set! v 2 v)
    v))
(assert! (equal? (cycle 2) (cycle 2)))
(assert! (not (equal? (cycle 2) (cycle 3))))
