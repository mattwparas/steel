;; vector->string on mutable and immutable vectors, with and without a range.
(assert! (equal? (vector->string (vector #\a #\b #\c)) "abc"))
(assert! (equal? (vector->string (vector #\a #\b #\c #\d) 1 3) "bc"))
(assert! (equal? (vector->string (vector #\λ #\x)) "λx"))
(assert! (equal? (immutable-vector->string (immutable-vector #\a #\b #\c)) "abc"))
(assert! (equal? (immutable-vector->string (immutable-vector #\a #\b #\c) 1) "bc"))
(assert! (equal? (with-handler (lambda (e) 'raised) (vector->string (vector #\a 1))) 'raised))

;; Equal float keys hash alike, including -0.0 (computed, so the reader
;; can't have folded it).
(define negative-zero (* -1.0 0.0))
(define h (hash 0.0 'zero 1.5 'one-and-a-half 2.0 'two))
(assert! (equal? (hash-ref h negative-zero) 'zero))
(assert! (equal? (hash-ref h (/ 3.0 2.0)) 'one-and-a-half))
(assert! (equal? (hash-ref h (+ 1.0 1.0)) 'two))
(assert! (not (hash-contains? h 2)))
(assert! (equal? (hash-ref (hash (list 0.0 1.0) 'pair) (list negative-zero 1.0)) 'pair))

;; Dropping deep structures still works with the immediates filtered out of the
;; drop queue.
(define (deep-list n acc) (if (= n 0) acc (deep-list (- n 1) (list n "s" 1.5 #\c acc))))
(define big (deep-list 200000 '()))
(set! big #f)
(define (deep-vector n acc) (if (= n 0) acc (deep-vector (- n 1) (vector n 'sym 2.5 acc))))
(define bigv (deep-vector 200000 '()))
(set! bigv #f)
(assert! (equal? big #f))
