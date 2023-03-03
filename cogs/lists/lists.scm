(provide split-last flatten)

(define (split-last-loop accum lst)
    (if (empty? (cdr lst))
        (list (reverse accum) (car lst))
        (loop (cons (car lst) accum) (cdr lst))))

;; Given a list, splits off the last argument, returns as a pair
(define (split-last lst)
  (split-last-loop '() lst))


(define (flatten lst) (transduce lst (flattening) (into-list)))

;; Need default arguments here
; (define (remove v lst [proc ]))