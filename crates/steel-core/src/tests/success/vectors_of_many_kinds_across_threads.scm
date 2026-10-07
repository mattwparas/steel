;; A vector shared by several threads whose elements are of every kind that
;; needs releasing. A read that got the tag of one value and the rest of
;; another shows up here, where a vector of one kind of value would hide it.
;; Elements are also copied from slot to slot, so a value read without its
;; reference taken ends up freed while a slot still holds it.
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
(define (seed n)
  (when (< n 8)
    (vector-push! shared (payload n))
    (seed (+ n 1))))
(seed 0)

(define (write n)
  (when (> n 0)
    (vector-set! shared (modulo n 8) (payload n))
    (vector-set! shared (modulo (+ n 3) 8) (vector-ref shared (modulo n 8)))
    (vector-set! shared (modulo (+ n 5) 8) (vector-ref shared (modulo (+ n 1) 8)))
    (write (- n 1))))
(define (read n ok)
  (if (= n 0)
      ok
      (read (- n 1)
            (and ok (whole? (vector-ref shared (modulo n 8))) (whole? (vector-ref shared (modulo (+ n 4) 8)))))))

;; Run alone first, so the compiled code is older than the threads.
(write 2000)
(assert! (read 2000 #t))

(define writers
  (list (spawn-native-thread (lambda () (write 20000)))
        (spawn-native-thread (lambda () (write 20000)))
        (spawn-native-thread (lambda () (write 20000)))))
(define readers
  (list (spawn-native-thread (lambda () (read 30000 #t)))
        (spawn-native-thread (lambda () (read 30000 #t)))))
(assert! (read 30000 #t))
(write 2000)
(for-each thread-join! writers)
(for-each (lambda (reader) (assert! (thread-join! reader))) readers)
(#%gc-collect)
(assert! (read 100 #t))

;; The same while the vector grows and shrinks past its first eight slots,
;; which moves its storage under the readers.
(define (grow n)
  (when (> n 0)
    (vector-push! shared (vector-ref shared (modulo n 8)))
    (grow (- n 1))))
(define (shrink)
  (when (> (vector-length shared) 8)
    (mutable-vector-pop! shared)
    (shrink)))
(define (reshape rounds)
  (when (> rounds 0)
    (grow 700)
    (write 200)
    (shrink)
    (reshape (- rounds 1))))

(define reshaper (spawn-native-thread (lambda () (reshape 30))))
(define overwriter (spawn-native-thread (lambda () (write 20000))))
(define steady-readers
  (list (spawn-native-thread (lambda () (read 30000 #t)))
        (spawn-native-thread (lambda () (read 30000 #t)))))
(assert! (read 30000 #t))
(thread-join! reshaper)
(thread-join! overwriter)
(for-each (lambda (reader) (assert! (thread-join! reader))) steady-readers)
(#%gc-collect)
(assert! (read 100 #t))
