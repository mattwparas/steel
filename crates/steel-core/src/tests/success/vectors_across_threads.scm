;; One thread grows, shrinks, overwrites and fills a vector while another reads
;; its first eight elements, which are always there.
;; Values of every kind that needs releasing, so that a read which took the
;; tag of one and the rest of another does not still look like a value.
(struct tagged (n twice) #:mutable)
(define (payload n)
  (let ([kind (modulo n 6)])
    (cond
      [(= kind 0) (list n n n)]
      [(= kind 1) (string-append (number->string n) ":" (number->string n))]
      [(= kind 2) (lambda () (list n n))]
      [(= kind 3) (mutable-vector n n)]
      [(= kind 4) (tagged n (* 2 n))]
      [else (box (list n n))])))
(define (whole? v)
  (cond
    [(list? v) (and (= (length v) 3) (equal? (car v) (caddr v)))]
    [(string? v) (> (string-length v) 2)]
    [(function? v)
     (let ([made (v)])
       (equal? (car made) (cadr made)))]
    [(vector? v) (and (= (vector-length v) 2) (equal? (vector-ref v 0) (vector-ref v 1)))]
    [(tagged? v) (= (tagged-twice v) (* 2 (tagged-n v)))]
    ;; Only a box is left.
    [else
     (let ([inner (unbox v)])
       (equal? (car inner) (cadr inner)))]))

(define shared (mutable-vector))
(define (seed n) (when (< n 8) (vector-push! shared (payload n)) (seed (+ n 1))))
(seed 0)
(define scratch (mutable-vector))

(define (grow n) (when (> n 0) (vector-push! shared (payload n)) (vector-push! scratch (payload n)) (grow (- n 1))))
(define (shrink) (when (> (vector-length shared) 8) (mutable-vector-pop! shared) (shrink)))
(define (overwrite n) (when (> n 0) (vector-set! shared (modulo n 8) (payload n)) (overwrite (- n 1))))
(define (writer rounds)
  (when (> rounds 0)
    (grow 1500)
    (overwrite 500)
    (vector-fill! shared (payload rounds) 0 8)
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
