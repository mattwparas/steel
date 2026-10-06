;; Values serialized on one thread and read on another, while a third keeps
;; collecting, which is what a thread reading one has to wait out safely.
(struct point (x y))
(struct counter (n) #:mutable)

(define (make-value n)
  (let ([shared (box n)])
    (list n
          (number->string n)
          shared
          (mutable-vector shared (point n (counter n)))
          (lambda (m) (+ n m))
          (hash 'n n))))

(define (value-ok? v n)
  (and (= (car v) n)
       (equal? (cadr v) (number->string n))
       (= (unbox (caddr v)) n)
       (eq? (caddr v) (vector-ref (list-ref v 3) 0))
       (= (counter-n (point-y (vector-ref (list-ref v 3) 1))) n)
       (= ((list-ref v 4) 1) (+ n 1))
       (= (hash-ref (list-ref v 5) 'n) n)))

(define stop (box #f))
(define collector
  (spawn-native-thread (lambda ()
                         (let loop ([n 0])
                           (if (unbox stop)
                               n
                               (begin
                                 (#%gc-collect)
                                 (box n)
                                 (loop (+ n 1))))))))

;; Serialized here, read there.
(define (read-on-another-thread from to)
  (let* ([serialized (map (lambda (n) (serialize-value (make-value n))) (range from to))]
         [reader (spawn-native-thread
                  (lambda ()
                    (let loop ([left serialized] [n from] [ok #t])
                      (if (null? left)
                          ok
                          (loop (cdr left) (+ n 1) (and ok (value-ok? (deserialize-value (car left)) n)))))))])
    (assert! (thread-join! reader))))
(read-on-another-thread 0 60)

;; Serialized there, as bytes, and read here.
(define (written-on-another-thread from to)
  (let* ([writer (spawn-native-thread
                  (lambda ()
                    (map (lambda (n) (serialized->bytes (serialize-value (make-value n)))) (range from to))))]
         [all-bytes (thread-join! writer)])
    (let loop ([left all-bytes] [n from])
      (unless (null? left)
        (assert! (value-ok? (deserialize-value (bytes->serialized (car left))) n))
        (loop (cdr left) (+ n 1))))))
(written-on-another-thread 0 60)

;; Several threads at it at once, each with its own values.
(define (round-trip-many from to)
  (let loop ([n from] [ok #t])
    (if (= n to)
        ok
        (loop (+ n 1)
              (and ok
                   (value-ok? (deserialize-value (bytes->serialized (serialized->bytes (serialize-value (make-value n)))))
                              n))))))
(define workers
  (list (spawn-native-thread (lambda () (round-trip-many 0 80)))
        (spawn-native-thread (lambda () (round-trip-many 100 180)))
        (spawn-native-thread (lambda () (round-trip-many 200 280)))))
(assert! (round-trip-many 300 380))
(for-each (lambda (worker) (assert! (thread-join! worker))) workers)

(set-box! stop #t)
(assert! (>= (thread-join! collector) 0))
