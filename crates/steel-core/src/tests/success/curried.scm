(define (split-at lst pos)
  (values (take lst pos) (drop lst pos)))

(define (associative-join f zero xs)
  ; (f x0 x1 ...) = (f (f2 x0 x1) (f2 x2 x3) ...)
  (cond
    [(null? xs) zero]
    [(null? (cdr xs)) (car xs)]
    [(null? (cddr xs)) (f (car xs) (cadr xs))]
    [else
     (define n (length xs))
     (define n/2 (quotient n 2))
     (define-values (l r) (split-at xs n/2))
     (f (associative-join f zero l) (associative-join f zero r))]))

(associative-join + 0 '(1 2 3 4))
(associative-join * 1 '(1 2 3 5))
(associative-join string-append "" '("a" "b" "c"))

(define ((merger == <<) xs ys)
  (lambda (xs ys)
    (let loop ([xs xs]
               [ys ys])
      (cond
        [(null? xs) ys]
        [(null? ys) xs]
        [(== (car xs) (car ys)) (loop (cdr xs) ys)]
        [(<< (car xs) (car ys)) (cons (car xs) (loop (cdr xs) ys))]
        [else (cons (car ys) (loop xs (cdr ys)))]))))

(associative-join (merger = <) '() (map list '(1 3 2 3 2 1)))
