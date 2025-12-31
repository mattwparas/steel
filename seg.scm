; (define (foldr2 func accum lst)
;   (stdout-simple-displayln "calling foldr2")
;   (if (null? lst) accum (func (car lst) (foldr2 func accum (cdr lst)))))

; (set! foldr2 foldr2)

; (define (longest lst)
;   (foldr2 comparison '() lst))

; (set! longest longest)


(define (fake-length x) (length x))
(set! fake-length fake-length)

(define comparison
  (λ (a b)
     (stdout-simple-displayln "calling foldr lambda function")
     ;; So we're saying that its already on the stack:
     ;; then we should just pop the value off?
     (if (= (length a) (length b)) a b)

     ; (length a)


     ))

; (set! comparison comparison)

; (foldr2 comparison '() (list (range 0 2) (range 0 4)))


(define (test)
  (comparison (list)
              (comparison (list)
                          (list))))

; (set! test test)


(stdout-simple-displayln "Calling longest...")

(test)
