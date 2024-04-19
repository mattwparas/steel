(provide quicksort)

(define (append-three one two three)
  (-> one (append two) (append three)))

(define (quicksort l gt?)
  (if (null? l)
      '()
      (append-three (quicksort (filter (lambda (x) (gt? (car l) x)) (cdr l)) gt?)
                    (list (car l))
                    (quicksort (filter (lambda (x) (not (gt? (car l) x))) (cdr l)) gt?))))
