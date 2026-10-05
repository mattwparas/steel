;; Values made by one thread and dropped by another, in both directions, and
;; by a thread that outlives the one that made them.
(struct record (name items) #:mutable)

(define (make-value n)
  (list (range 0 20)
        (string-append "value-" (number->string n))
        (lambda (x) (+ x n))
        (mutable-vector n n n)
        (record n (list n n))
        (hash 'n n)))

(define (value-ok? v n)
  (and (= (length (car v)) 20)
       (equal? (cadr v) (string-append "value-" (number->string n)))
       (= ((caddr v) 1) (+ n 1))
       (= (vector-ref (list-ref v 3) 2) n)
       (= (record-name (list-ref v 4)) n)
       (= (hash-ref (list-ref v 5) 'n) n)))

(define slots (make-vector 64 #f))

;; Threads fill the slots and exit, and this thread checks and drops them.
(define (fill from to)
  (when (< from to)
    (vector-set! slots from (make-value from))
    (fill (+ from 1) to)))
(define (check-and-empty i ok)
  (if (= i 64)
      ok
      (let ([v (vector-ref slots i)])
        (vector-set! slots i #f)
        (check-and-empty (+ i 1) (and ok (value-ok? v i))))))

(define (filled-by-threads rounds)
  (when (> rounds 0)
    (let ([fillers (list (spawn-native-thread (lambda () (fill 0 16)))
                         (spawn-native-thread (lambda () (fill 16 32)))
                         (spawn-native-thread (lambda () (fill 32 48)))
                         (spawn-native-thread (lambda () (fill 48 64))))])
      (for-each thread-join! fillers)
      (assert! (check-and-empty 0 #t)))
    (filled-by-threads (- rounds 1))))
(filled-by-threads 30)
(#%gc-collect)

;; This thread fills them, and the threads check and drop them.
(define (check-range from to ok)
  (if (= from to)
      ok
      (let ([v (vector-ref slots from)])
        (vector-set! slots from #f)
        (check-range (+ from 1) to (and ok (value-ok? v from))))))

(define (emptied-by-threads rounds)
  (when (> rounds 0)
    (fill 0 64)
    (let ([checkers (list (spawn-native-thread (lambda () (check-range 0 16 #t)))
                          (spawn-native-thread (lambda () (check-range 16 32 #t)))
                          (spawn-native-thread (lambda () (check-range 32 48 #t)))
                          (spawn-native-thread (lambda () (check-range 48 64 #t))))])
      (for-each (lambda (thread) (assert! (thread-join! thread))) checkers))
    (emptied-by-threads (- rounds 1))))
(emptied-by-threads 30)
(#%gc-collect)

;; What a thread returns is made there and used here after it is gone.
(define (returned rounds)
  (when (> rounds 0)
    (let* ([thread (spawn-native-thread (lambda () (map make-value (range 0 50))))]
           [values (thread-join! thread)])
      (assert! (= (length values) 50))
      (assert! (value-ok? (list-ref values 49) 49)))
    (returned (- rounds 1))))
(returned 30)

;; Passed along a chain of threads, each of which keeps it while the one
;; before exits.
(define (relay value hops)
  (if (= hops 0)
      value
      (thread-join! (spawn-native-thread (lambda () (relay value (- hops 1)))))))
(assert! (value-ok? (relay (make-value 7) 12) 7))
(#%gc-collect)
