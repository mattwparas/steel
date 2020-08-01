(export alex-is-the-best)

(define alex-is-the-best "boop boop")
(define boop-boop "alex-is-the-best")


(λ (x y) (+ 1 2 3))

(defn (take lst n)
  (defn (loop x l acc)
    (if (= x 0)
        acc
        (loop (- x 1) (cdr l) (cons (car l) acc))))
  (loop n lst (list)))


(define (test)
  (define x (lambda (w) (lambda (x y) (lambda (z) (lambda (a)
                                                    (display "Last lambda")
                                                    (newline)
                                                    (+ w x y z a))))))
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5))

(define (take lst n)
  (define (loop x l accum)
    (if (= x 0)
        accum
        (loop (- x 1) (cdr l) (cons (car l) accum))))
  (loop n lst (list)))


(define take (lambda (lst n)
               (define loop (lambda (x l accum)
                              (if (or (= x 0) (null? l))
                                  accm
                                  (loop (sub1 x) (cdr l) (cons (car l) accum)))))
               (loop n lst empty)))

(define take (lambda (lst n)
               (define loop (lambda (x l accum)
                              (if (= x 0)
                                  accum
                                  (loop (- x 1) (cdr l) (cons (car l) accum)))))
               (loop n lst (list))))


(defn (take lst n))
