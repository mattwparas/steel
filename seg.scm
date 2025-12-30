(define (foldr2 func accum lst)
  (if (null? lst) accum (func (car lst) (foldr2 func accum (cdr lst)))))

(define (longest lst)
  (foldr2 (λ (a b) (if (> (length a) (length b)) a b)) '() lst))

; (define (longest2 lst)
;   (foldr2 (λ (a b) (if (> a b) a b)) '() lst))

(longest (list (range 0 2) (range 0 4)))
