;; Mutable structs shared by several threads whose fields hold values of every
;; kind that needs releasing. A read that got the tag of one value and the
;; rest of another shows up here, where fields of one kind of value would hide
;; it. Fields are also copied into each other, so a value read without its
;; reference taken ends up freed while a field still holds it.
(struct tagged (n twice) #:mutable)
(struct node (left right tag) #:mutable)
(struct holder (current) #:mutable)

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

(define shared (node (payload 0) (payload 1) (payload 2)))
(define (write n)
  (when (> n 0)
    (set-node-left! shared (payload n))
    (set-node-right! shared (node-left shared))
    (set-node-tag! shared (node-right shared))
    (write (- n 1))))
(define (read n ok)
  (if (= n 0)
      ok
      (read (- n 1)
            (and ok (whole? (node-left shared)) (whole? (node-right shared)) (whole? (node-tag shared))))))

;; Run alone first, so the compiled accessors are older than the threads.
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

;; The struct itself is what gets replaced here, with fields taken from the
;; one it replaces.
(define held (holder (node (payload 0) (payload 1) (payload 2))))
(define (replace n)
  (when (> n 0)
    (let ([current (holder-current held)])
      (set-holder-current! held (node (payload n) (node-left current) (node-right current))))
    (replace (- n 1))))
(define (read-through n ok)
  (if (= n 0)
      ok
      (let ([current (holder-current held)])
        (read-through (- n 1)
                      (and ok
                           (node? current)
                           (whole? (node-left current))
                           (whole? (node-right current))
                           (whole? (node-tag current)))))))

(define replacers
  (list (spawn-native-thread (lambda () (replace 20000)))
        (spawn-native-thread (lambda () (replace 20000)))))
(define through-readers
  (list (spawn-native-thread (lambda () (read-through 30000 #t)))
        (spawn-native-thread (lambda () (read-through 30000 #t)))))
(assert! (read-through 30000 #t))
(for-each thread-join! replacers)
(for-each (lambda (reader) (assert! (thread-join! reader))) through-readers)
(#%gc-collect)
(assert! (read-through 100 #t))

;; Values that move between a struct, a vector and a box, so each kind of
;; cell hands the others what it read.
(define slots (mutable-vector (payload 3) (payload 4) (payload 5)))
(define cell (box (payload 6)))
(define (shuffle n)
  (when (> n 0)
    (vector-set! slots (modulo n 3) (node-left shared))
    (set-box! cell (vector-ref slots (modulo (+ n 1) 3)))
    (set-node-tag! shared (unbox cell))
    (set-node-left! shared (payload n))
    (shuffle (- n 1))))
(define (read-all n ok)
  (if (= n 0)
      ok
      (read-all (- n 1)
                (and ok
                     (whole? (vector-ref slots (modulo n 3)))
                     (whole? (unbox cell))
                     (whole? (node-tag shared))
                     (whole? (node-left shared))))))

(define shufflers
  (list (spawn-native-thread (lambda () (shuffle 20000)))
        (spawn-native-thread (lambda () (shuffle 20000)))))
(define all-readers
  (list (spawn-native-thread (lambda () (read-all 30000 #t)))
        (spawn-native-thread (lambda () (read-all 30000 #t)))))
(assert! (read-all 30000 #t))
(for-each thread-join! shufflers)
(for-each (lambda (reader) (assert! (thread-join! reader))) all-readers)
(#%gc-collect)
(assert! (read-all 100 #t))
