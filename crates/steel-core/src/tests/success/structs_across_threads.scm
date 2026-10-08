;; Threads read and write the fields of the same mutable structs. Every read
;; has to see one whole value, and most are made on one thread and dropped on
;; another.
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

(struct node (left right tag) #:mutable)
(struct holder (current) #:mutable)

(define shared (node (payload 0) (payload 0) (payload 0)))
(define (write-fields n)
  (when (> n 0)
    (set-node-left! shared (payload n))
    (set-node-right! shared (payload n))
    (set-node-tag! shared (payload n))
    (write-fields (- n 1))))
(define (read-fields n ok)
  (if (= n 0)
      ok
      (read-fields (- n 1)
                   (and ok
                        (whole? (node-left shared))
                        (whole? (node-right shared))
                        (whole? (node-tag shared))))))

;; Run alone first, so the compiled accessors are older than the threads.
(write-fields 3000)
(assert! (read-fields 3000 #t))

(define field-writers
  (list (spawn-native-thread (lambda () (write-fields 30000)))
        (spawn-native-thread (lambda () (write-fields 30000)))))
(define field-readers
  (list (spawn-native-thread (lambda () (read-fields 30000 #t)))
        (spawn-native-thread (lambda () (read-fields 30000 #t)))))
(assert! (read-fields 30000 #t))
(for-each thread-join! field-writers)
(for-each (lambda (reader) (assert! (thread-join! reader))) field-readers)
(assert! (read-fields 100 #t))

;; The struct itself is what gets replaced here, so its last handle can be
;; dropped by a thread that didn't make it while another is reading a field.
(define held (holder (node (payload 0) (payload 0) (payload 0))))
(define (replace-node n)
  (when (> n 0)
    (set-holder-current! held (node (payload n) (payload n) (payload n)))
    (replace-node (- n 1))))
(define (read-through n ok)
  (if (= n 0)
      ok
      (let ([current (holder-current held)])
        (read-through (- n 1)
                      (and ok (node? current) (whole? (node-left current)) (whole? (node-tag current)))))))

(define replacers
  (list (spawn-native-thread (lambda () (replace-node 20000)))
        (spawn-native-thread (lambda () (replace-node 20000)))))
(assert! (read-through 40000 #t))
(for-each thread-join! replacers)

;; A ring of structs that the threads keep relinking while one collects.
(define a (node #f #f (payload 1)))
(define b (node a #f (payload 2)))
(define c (node b a (payload 3)))
(set-node-left! a c)
(set-node-right! a b)
(set-node-right! b c)
(define (relink n)
  (when (> n 0)
    (let ([fresh (node a b (payload n))])
      (set-node-left! a fresh)
      (set-node-right! fresh fresh)
      (set-node-left! b (node-left a)))
    (relink (- n 1))))
(define (walk n at ok)
  (if (= n 0)
      ok
      (let ([next (node-left at)])
        (walk (- n 1) (if (node? next) next a) (and ok (whole? (node-tag at)))))))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

(define relinkers
  (list (spawn-native-thread (lambda () (relink 15000)))
        (spawn-native-thread (lambda () (relink 15000)))))
(define walker (spawn-native-thread (lambda () (walk 60000 a #t))))
(collect 30)
(for-each thread-join! relinkers)
(assert! (thread-join! walker))
(#%gc-collect)
(assert! (walk 1000 a #t))
