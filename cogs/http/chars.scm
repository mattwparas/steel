(provide every-char?)

(define (every-char? str ok?)
  (equal?
   0
   (transduce str (compose (filtering (lambda (char) (not (ok? char)))) (taking 1)) (into-count))))
