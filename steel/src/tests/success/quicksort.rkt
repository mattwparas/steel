;; Current append only accepts 2 arguments, need to make it accept 3
(define (append-three one two three)
    (-> one
        (append two)
        (append three)))

(define (quicksort l gt?)
  (if (null? l)
      '()
      (append-three (quicksort (filter (lambda (x) (gt? (car l) x)) (cdr l)) gt?)
              (list (car l))
              (quicksort (filter (lambda (x) (not (gt? (car l) x))) (cdr l)) gt?))))

(define output (quicksort '(1 3 5 7 9 8 6 4 2) >))

(assert! (equal? output '(1 2 3 4 5 6 7 8 9)))