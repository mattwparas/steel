;; Vectors that threads append to, copy into, swap within and replace whole,
;; while others read them, including through a vector of vectors.
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
(define (row n) (mutable-vector (payload n) (payload n) (payload n) (payload n)))

(define wide (row 0))
(define source (row 1))
(define outer (mutable-vector (row 2) (row 3)))
(define growing (mutable-vector (payload 0)))
(define scratch (mutable-vector))

(define (reshape n)
  (when (> n 0)
    (vector-swap! wide 0 3)
    (vector-swap! wide 1 2)
    (vector-copy! wide 0 source 0 4)
    (vector-fill! source (payload n))
    (vector-set! outer (modulo n 2) (row n))
    (let ([extra (row n)])
      (vector-append! growing extra))
    (when (= 0 (modulo n 500))
      (mutable-vector->clear scratch))
    (vector-push! scratch (payload n))
    (when (> (vector-length growing) 64)
      (mutable-vector-pop! growing)
      (mutable-vector-pop! growing))
    (reshape (- n 1))))

(define (read n ok)
  (if (= n 0)
      ok
      (let ([i (modulo n 4)])
        (read (- n 1)
              (and ok
                   (whole? (vector-ref wide i))
                   (whole? (vector-ref source i))
                   (whole? (vector-ref (vector-ref outer (modulo n 2)) i))
                   (whole? (vector-ref growing 0))
                   (>= (vector-length scratch) 0))))))

;; Run alone first, so the compiled code is older than the threads.
(reshape 1000)
(assert! (read 1000 #t))

(define reshapers
  (list (spawn-native-thread (lambda () (reshape 15000)))
        (spawn-native-thread (lambda () (reshape 15000)))))
(define readers
  (list (spawn-native-thread (lambda () (read 60000 #t)))
        (spawn-native-thread (lambda () (read 60000 #t)))))
(assert! (read 60000 #t))
(for-each thread-join! reshapers)
(for-each (lambda (reader) (assert! (thread-join! reader))) readers)
(assert! (read 100 #t))
(assert! (= (vector-length wide) 4))

;; Two threads that each move one vector's elements into the other's, in
;; opposite directions, and copy between them the same way.
(define left (mutable-vector (payload 1) (payload 2)))
(define right (mutable-vector (payload 3) (payload 4)))
(define (shuttle from to n)
  (when (> n 0)
    (vector-append! to from)
    (vector-push! from (payload n))
    ;; The other thread may have just emptied the source.
    (with-handler (lambda (e) #f) (vector-copy! from 0 to 0 1))
    (shuttle from to (- n 1))))
(define shuttler (spawn-native-thread (lambda () (shuttle left right 2000))))
(shuttle right left 2000)
(thread-join! shuttler)
(assert! (= (+ (vector-length left) (vector-length right)) 4004))
(assert! (eq? 'refused (with-handler (lambda (e) 'refused) (vector-append! left left))))

