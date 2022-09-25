(define (binary-search value vector)
  (define (helper low high)
      (if (< high low)
        #f
        (let ((middle (quotient (+ low high) 2)))
          (cond ((> (vector-ref vector middle) value)
                 (helper low (- middle 1)))
                ((< (vector-ref vector middle) value)
                 (helper (+ middle 1) high))
                (else middle)))))

  (helper 0 (- (vector-length vector) 1)))


(assert! (equal? 4 (binary-search 6 (vector 1 3 4 5 6 7 8 9 10))))
(assert! (not (binary-search 2 (vector 1 3 4 5 6 7 8 9 10))))