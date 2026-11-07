(define-syntax assert-equal!
  (syntax-rules ()
    [(_ expected actual)
     (let ([ok (equal? expected actual)])
       (when (not ok)
         (displayln "Expected value " expected " but got " actual ".")
         (assert! ok)))]))

(assert-equal! (bytevector 65 112 112 108 101) (string->bytes "Apple"))
(assert-equal! (bytes->list (string->bytes "Apple")) (list 65 112 112 108 101))
(assert-equal! (bytes-ref (string->bytes "Apple") 0) 65)
(assert! (bytes? (bytes 0 1 2 3 4)))
(assert! (byte? 19))
(assert! (not (byte? 1000000)))

(define my-bytes (bytes 10 20 30 40))

(bytes-set! my-bytes 0 100)
(assert-equal! (bytes-ref my-bytes 0) 100)

(assert! (equal? (bytevector 10 20 30) (bytes 10 20 30)))
(assert! (eq? my-bytes my-bytes))
(assert! (not (eq? my-bytes (bytes))))

(assert-equal! (bytes-append (bytes 0 1 2) (bytes 3 4 5)) (bytes 0 1 2 3 4 5))

(assert-equal! (list->bytes (list 0 1 2 3 4 5)) (bytes 0 1 2 3 4 5))

(define copied (bytes 0 1 2 3 4))
(bytevector-copy! copied 1 (bytes 10 20))
(assert-equal! copied (bytes 0 10 20 3 4))

(bytevector-copy! copied 0 (bytes 0 1 2 3 4 5) 3 5)
(assert-equal! copied (bytes 3 4 20 3 4))

;; The same bytevector, with the ranges overlapping
(define overlapping (bytes 0 1 2 3 4))
(bytevector-copy! overlapping 1 overlapping 0 4)
(assert-equal! overlapping (bytes 0 0 1 2 3))

(bytevector-copy! overlapping 0 overlapping)
(assert-equal! overlapping (bytes 0 0 1 2 3))

(assert-equal! 'raised (with-handler (lambda (err) 'raised) (bytevector-copy! (bytes 0 1) 1 (bytes 0 1))))
(assert-equal! 'raised (with-handler (lambda (err) 'raised) (bytevector-copy! (bytes 0 1) 0 (bytes 0 1) 1 0)))
(assert-equal! 'raised (with-handler (lambda (err) 'raised) (bytevector-copy! (bytes 0 1) 0 (bytes 0 1) 0 3)))
