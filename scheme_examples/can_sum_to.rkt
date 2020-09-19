(define (some pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) #t]
        [else (some pred (cdr lst))]))

(define (can-sum-to target lst)
  (or (equal? target 0)
      (and
       (> target 0)
       (some
        (lambda (m) (can-sum-to (- target m) lst))
        lst))))


(can-sum-to 7 '(2 3 11))
(can-sum-to 9 '(2 4 6))
;; (can-sum-to 1 '(0 2 3 4))
;; (can-sum-to 15 '(0 1 2 3 4 5))
