; (let ([f void])
;   (let ([ft (let ([g void])
;               (let ([gt (let ([x 5])
;                               (lambda () ...))])
;                     (set! g gt))
;                   (lambda () ... g ...))])
;   (set! f ft))
;   f)

;; TODO this transformation is how I should implement defines
;; Do compile time transformation of AST -> this form
(let ((is-even? void) (is-odd? void))
  (let ((is-even?-prime 
          (lambda (n) (or (zero? n) 
                          (is-odd? (sub1 n)))))
        
        (is-odd?-prime
          (lambda (n) (and (not (zero? n))
                           (is-even? (sub1 n))))))

        (set! is-even? is-even?-prime)
        (set! is-odd? is-odd?-prime))
  (is-odd? 12))
                          
                          
; (letrec ([is-even? (lambda (n)
;                        (or (zero? n)
;                            (is-odd? (sub1 n))))]
;            [is-odd? (lambda (n)
;                       (and (not (zero? n))
;                            (is-even? (sub1 n))))])
;     (is-odd? 11))

; (define (outer))