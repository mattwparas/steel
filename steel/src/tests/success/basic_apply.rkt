;; TODO this should get fixed when lists are implemented using the new backing
(define result (apply + (list 1 2 3 4)))
(assert! (equal? 10 result))