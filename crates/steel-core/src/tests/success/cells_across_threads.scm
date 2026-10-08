;; A box and a mutable vector that two threads use at once. Every read has to
;; see one whole value, and each value is made on one thread and dropped on
;; the other.
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

(define shared-box (box (payload 0)))
(define (box-writer n)
  (when (> n 0)
    (set-box! shared-box (payload n))
    (box-writer (- n 1))))
(define (box-reader n ok)
  (if (= n 0) ok (box-reader (- n 1) (and ok (whole? (unbox shared-box))))))

(define shared-vector (mutable-vector (payload 0)))
(define (vector-writer n)
  (when (> n 0)
    (vector-set! shared-vector 0 (payload n))
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
