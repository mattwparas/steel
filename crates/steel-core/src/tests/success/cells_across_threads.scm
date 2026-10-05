;; A box and a mutable vector that two threads use at once. Every value either
;; thread stores is a three element list, so every read has to see one whole,
;; and each list is made on one thread and dropped on the other.
(define (triple n) (list n n n))
(define (whole? v) (and (list? v) (= (length v) 3) (equal? (car v) (caddr v))))

(define shared-box (box (triple 0)))
(define (box-writer n)
  (when (> n 0)
    (set-box! shared-box (triple n))
    (box-writer (- n 1))))
(define (box-reader n ok)
  (if (= n 0) ok (box-reader (- n 1) (and ok (whole? (unbox shared-box))))))

(define shared-vector (mutable-vector (triple 0)))
(define (vector-writer n)
  (when (> n 0)
    (vector-set! shared-vector 0 (triple n))
    (vector-writer (- n 1))))
(define (vector-reader n ok)
  (if (= n 0) ok (vector-reader (- n 1) (and ok (whole? (vector-ref shared-vector 0))))))

(define box-thread (spawn-native-thread (lambda () (box-writer 20000))))
(assert! (box-reader 20000 #t))
(box-writer 2000)
(thread-join! box-thread)
(assert! (box-reader 100 #t))

(define vector-thread (spawn-native-thread (lambda () (vector-writer 20000))))
(assert! (vector-reader 20000 #t))
(vector-writer 2000)
(thread-join! vector-thread)
(assert! (vector-reader 100 #t))
