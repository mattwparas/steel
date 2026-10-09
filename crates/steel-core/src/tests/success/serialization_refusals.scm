;; What can't be serialized is refused with an error, and so are bytes that
;; were never a serialized value. None of it brings the process down.
(struct point (x y))

(define (refused? thunk)
  (eq? 'refused (with-handler (lambda (e) 'refused) (thunk))))

(assert! (refused? (lambda () (serialize-value (make-weak-box (list 1))))))
(assert! (refused? (lambda () (serialize-value (list 1 (make-weak-box 2))))))
(assert! (refused? (lambda () (serialize-value (box (make-weak-box 2))))))
(assert! (refused? (lambda () (serialize-value (mutable-vector (make-weak-box 2))))))
(assert! (refused? (lambda () (serialize-value (point 1 (make-weak-box 2))))))
(assert! (refused? (lambda () (serialize-value (let ([w (make-weak-box 2)]) (lambda () w))))))
(assert! (refused? (lambda () (serialize-value (spawn-native-thread (lambda () 1))))))

;; Bytes that are not a serialized value.
(assert! (refused? (lambda () (bytes->serialized (bytes)))))
(assert! (refused? (lambda () (bytes->serialized (bytes 1 2 3)))))
(assert! (refused? (lambda () (bytes->serialized (bytes 255 255 255 255 255 255 255 255)))))

;; A serialized value cut short at every length.
(define whole (serialized->bytes (serialize-value (list 1 "two" (box 3) (point 4 5) (lambda (x) x)))))
(define (prefix n)
  (let ([out (bytes)])
    (let loop ([i 0])
      (when (< i n)
        (bytes-push! out (bytes-ref whole i))
        (loop (+ i 1))))
    out))
(define (cut-short n)
  (when (< n (bytes-length whole))
    (assert! (refused? (lambda () (deserialize-value (bytes->serialized (prefix n))))))
    (cut-short (+ n 7))))
(cut-short 0)

;; Bytes changed after they were written. Whatever each change does, reading
;; them is an error or a value.
(struct wide (a b c) #:mutable)
(define (reads-or-refuses? bs)
  (with-handler (lambda (e) #t)
                (begin
                  (deserialize-value (bytes->serialized bs))
                  #t)))
(define (with-byte bs at value)
  (let ([out (bytes)])
    (let loop ([i 0])
      (when (< i (bytes-length bs))
        (bytes-push! out (if (= i at) value (bytes-ref bs i)))
        (loop (+ i 1))))
    out))
(define (change-each-byte bs)
  (let loop ([i 0])
    (when (< i (bytes-length bs))
      (assert! (reads-or-refuses? (with-byte bs i 255)))
      (assert! (reads-or-refuses? (with-byte bs i 0)))
      (assert! (reads-or-refuses? (with-byte bs i (modulo (+ 1 (bytes-ref bs i)) 256))))
      (loop (+ i 1)))))
;; A struct's functions, which name a field by its index.
(change-each-byte (serialized->bytes (serialize-value (list point-y point point?))))
(change-each-byte (serialized->bytes (serialize-value (list (point 1 2) (wide 1 2 3)))))
;; A function, whose body names constants, captures and places in itself.
(change-each-byte (serialized->bytes (serialize-value (let ([n 5])
                                                        (lambda (m)
                                                          (if (> m n) (list "more" m) 'less))))))

;; A hash map with a key that holds itself can't be made, so one in bytes
;; is refused. This is a box that holds itself, and a set of it.
(define self-box (let ([b (box 1)]) (set-box! b b) b))
(define a-set-of-a-box (serialized->bytes (serialize-value (list (hashset (box 1)) self-box))))
(assert! (reads-or-refuses? a-set-of-a-box))

;; A struct type is sent with its layout, which whoever reads it checks
;; against the type it has. The layout is the last thing in the bytes: whether
;; the type is mutable, how many fields it has, and their names. Here it is
;; changed to stand for a type that is laid out some other way where it was
;; sent from.
(define (layout-refused? bs from-end value)
  (and (not (refused? (lambda () (deserialize-value (bytes->serialized bs)))))
       (refused? (lambda ()
                   (deserialize-value (bytes->serialized
                                       (with-byte bs (- (bytes-length bs) from-end) value)))))))
;; `wide` has three fields with names of one letter: nine bytes for how many
;; names and nine for each, after nine for the count and one for mutable.
(for-each (lambda (bs)
            (assert! (layout-refused? bs 46 0))
            (assert! (layout-refused? bs 44 2))
            (assert! (layout-refused? bs 1 100)))
          (list (serialized->bytes (serialize-value (wide 1 2 3)))
                (serialized->bytes (serialize-value wide))
                (serialized->bytes (serialize-value wide-c))
                (serialized->bytes (serialize-value set-wide-a!))))
;; `point` has two.
(for-each (lambda (bs)
            (assert! (layout-refused? bs 37 1))
            (assert! (layout-refused? bs 35 3))
            (assert! (layout-refused? bs 1 100)))
          (list (serialized->bytes (serialize-value (point 1 2)))
                (serialized->bytes (serialize-value point))
                (serialized->bytes (serialize-value point?))
                (serialized->bytes (serialize-value point-y))))

;; Things that are not serialized values at all.
(assert! (refused? (lambda () (deserialize-value 5))))
(assert! (refused? (lambda () (deserialize-value (list 1 2)))))
(assert! (refused? (lambda () (serialized->bytes "not serialized"))))
(assert! (refused? (lambda () (bytes->serialized "not bytes"))))

;; And after all that, serializing still works.
(assert! (equal? (deserialize-value (serialize-value (list 1 2 3))) (list 1 2 3)))
