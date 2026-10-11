;; Only a collection makes a will ready, and here nothing runs one. The call
;; that waits for a will runs one itself first.

(define executor (make-will-executor))
(define ran (box 0))

(define (register)
  (will-register executor (list 1 2 3) (lambda (value) (set-box! ran (+ 1 (unbox ran)))))
  #t)

(register)
(will-execute executor)
(assert! (= (unbox ran) 1))

(register)
(register)
(will-execute executor)
(will-execute executor)
(assert! (= (unbox ran) 3))
