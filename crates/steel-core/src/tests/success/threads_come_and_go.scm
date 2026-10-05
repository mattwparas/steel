;; Many short threads, each writing shared cells and exiting with values it
;; swapped out still waiting to be dropped, while longer ones keep reading.
(define (triple n) (list n n n))
(define (whole? v) (and (list? v) (= (length v) 3) (equal? (car v) (caddr v))))
(struct slot (value) #:mutable)

(define shared-box (box (triple 0)))
(define shared-vector (mutable-vector (triple 0) (triple 0)))
(define shared-struct (slot (triple 0)))
(define stop (box #f))

(define (write n)
  (when (> n 0)
    (set-box! shared-box (triple n))
    (vector-set! shared-vector (modulo n 2) (triple n))
    (set-slot-value! shared-struct (triple n))
    (write (- n 1))))

(define (read ok)
  (if (unbox stop)
      ok
      (read (and ok
                 (whole? (unbox shared-box))
                 (whole? (vector-ref shared-vector 0))
                 (whole? (vector-ref shared-vector 1))
                 (whole? (slot-value shared-struct))))))

(define readers
  (list (spawn-native-thread (lambda () (read #t)))
        (spawn-native-thread (lambda () (read #t)))))

;; One at a time.
(define (one-after-another n)
  (when (> n 0)
    (thread-join! (spawn-native-thread (lambda () (write 500))))
    (one-after-another (- n 1))))
(one-after-another 40)

;; Several at a time, with a collection between the groups.
(define (in-groups n)
  (when (> n 0)
    (let ([group (list (spawn-native-thread (lambda () (write 500)))
                       (spawn-native-thread (lambda () (write 500)))
                       (spawn-native-thread (lambda () (write 500)))
                       (spawn-native-thread (lambda () (write 500))))])
      (write 200)
      (for-each thread-join! group))
    (#%gc-collect)
    (in-groups (- n 1))))
(in-groups 15)

;; Threads that start threads.
(define (tree depth)
  (write 200)
  (when (> depth 0)
    (let ([left (spawn-native-thread (lambda () (tree (- depth 1))))]
          [right (spawn-native-thread (lambda () (tree (- depth 1))))])
      (thread-join! left)
      (thread-join! right))))
(tree 4)

(set-box! stop #t)
(for-each (lambda (reader) (assert! (thread-join! reader))) readers)
(#%gc-collect)
(assert! (whole? (unbox shared-box)))
(assert! (whole? (slot-value shared-struct)))
