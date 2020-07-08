(export alex-is-the-best)

(define alex-is-the-best "boop boop")
(define boop-boop "alex-is-the-best")


(define (take lst n)
  (define (loop x l accum)
    (if (or (zero? x) (null? l))
        accum
        (loop (sub1 x) (cdr l) (append accum (list (car l))))))
  (loop n lst '()))


(define take (lambda (lst n)
               (define loop (lambda (x l accum)
                              (if (or (zero? x) (null? l))
                                  accm
                                  (loop (sub1 x) (cdr l) (cons (car l) accum)))))
               (loop n lst empty)))
