;; TODO this should get fixed when lists are fully implemented using the new backing
(define result (apply map (list (lambda (x) 10) (list 1 2 3 4))))
(assert! (equal? result (list 10 10 10 10)))