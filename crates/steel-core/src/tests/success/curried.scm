(define (split-at lst pos)
  (values (take lst pos) (drop lst pos)))

(define (associative-join f zero xs)
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

(define ((pair-with a) b)
  (list a b))
(assert! (equal? ((pair-with 1) 2) '(1 2)))
(define ((two-then-one a b) c)
  (list a b c))
(assert! (equal? ((two-then-one 1 2) 3) '(1 2 3)))
(define ((rest-then-one a . r) b)
  (list a r b))
(assert! (equal? ((rest-then-one 1 2 3) 4) '(1 (2 3) 4)))
(define (((three-deep a) b) c)
  (list a b c))
(assert! (equal? (((three-deep 1) 2) 3) '(1 2 3)))
