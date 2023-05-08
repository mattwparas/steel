(define (map func lst)
  (if (empty? lst)
    '()
    (cons (func (first lst)) (map func (rest lst)))))

;; To do the tail recursion modulo cons, we'd need a special op code for tail recursive cons, where
;; the second argument to cons is recursive with respect to itself
;; 
;; So in this case:
;; (cons (func (first lst)) (map func (rest lst)))
;;                          ^^^^ is the recursive call to itself, which occurs in the second position
;; 
;; This can be lifted up into something like this:
(define (map2 func lst)
  (define (map2-trmc func lst accum)
    (if (empty? lst)
        accum
        (map2-trmc func (cdr lst) (begin (vector-push! accum (func (first lst)))
                                         accum))
    
    ))
  (mutable-vector->list (map2-trmc func lst (mutable-vector))))

