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

;; Things that are not serialized values at all.
(assert! (refused? (lambda () (deserialize-value 5))))
(assert! (refused? (lambda () (deserialize-value (list 1 2)))))
(assert! (refused? (lambda () (serialized->bytes "not serialized"))))
(assert! (refused? (lambda () (bytes->serialized "not bytes"))))

;; And after all that, serializing still works.
(assert! (equal? (deserialize-value (serialize-value (list 1 2 3))) (list 1 2 3)))
