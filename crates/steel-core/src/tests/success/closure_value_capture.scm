(define (build-list-of-functions n i list)
  (if (< i n)
      (build-list-of-functions n (+ i 1) (cons (lambda () (* (- n i) (- n i))) list))
      list))

(define list-of-functions (build-list-of-functions 10 1 '()))

(assert! (equal?
            (map (lambda (f) (f)) list-of-functions)
            '(1 4 9 16 25 36 49 64 81)))

(assert! (equal? ((list-ref list-of-functions 8))
                 81))