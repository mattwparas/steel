(define r (range 0 100000))

(define results (mutable-vector))

(define (loop x)
  (if (= x 100)
      void
      (begin
        (vector-push! results (map (lambda (x) (+ x 1)) r))
        (loop (+ x 1)))))

(provide run)
(define (run)
  (loop 0))

; (define lst (map (lambda (x) (+ x 1)) r))

; (displayln (length lst))
