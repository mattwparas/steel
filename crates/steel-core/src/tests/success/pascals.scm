(define (next-row row)
  (map + (cons 0 row) (append row '(0))))

(define (triangle row rows)
  (if (= rows 0) '() (cons row (triangle (next-row row) (- rows 1)))))

(triangle (list 1) 5)

(assert! (equal? (triangle (list 1) 5) '((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))))
