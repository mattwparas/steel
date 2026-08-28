;;; Char predicates Steel does not provide.
;;;
;;; ASCII-only, which is all these benchmarks feed them.

(provide char-alphabetic?
         char-numeric?)

(define (char-alphabetic? c)
  (let ([n (char->integer c)])
    (or (and (>= n 65) (<= n 90)) (and (>= n 97) (<= n 122)))))

(define (char-numeric? c)
  (let ([n (char->integer c)])
    (and (>= n 48) (<= n 57))))
