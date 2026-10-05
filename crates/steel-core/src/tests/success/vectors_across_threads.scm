;; One thread grows, shrinks, overwrites and fills a vector while another reads
;; its first eight elements, which are always there.
(define (triple n) (list n n n))
(define (whole? v) (and (list? v) (= (length v) 3) (equal? (car v) (caddr v))))

(define shared (mutable-vector))
(define (seed n) (when (< n 8) (vector-push! shared (triple n)) (seed (+ n 1))))
(seed 0)
(define scratch (mutable-vector))

(define (grow n) (when (> n 0) (vector-push! shared (triple n)) (vector-push! scratch (triple n)) (grow (- n 1))))
(define (shrink) (when (> (vector-length shared) 8) (mutable-vector-pop! shared) (shrink)))
(define (overwrite n) (when (> n 0) (vector-set! shared (modulo n 8) (triple n)) (overwrite (- n 1))))
(define (writer rounds)
  (when (> rounds 0)
    (grow 1500)
    (overwrite 500)
    (vector-fill! shared (triple rounds) 0 8)
    (shrink)
    (mutable-vector->clear scratch)
    (writer (- rounds 1))))

(define (reader n ok)
  (if (= n 0)
      ok
      (reader (- n 1) (and ok (whole? (vector-ref shared (modulo n 8))) (>= (vector-length scratch) 0)))))

(define other (spawn-native-thread (lambda () (writer 20))))
(define ok (reader 400000 #t))
(thread-join! other)
(assert! ok)
(assert! (= (vector-length shared) 8))
