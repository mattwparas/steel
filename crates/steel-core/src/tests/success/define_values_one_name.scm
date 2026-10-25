;; define-values and let-values with one name take the one value, which can
;; itself be a list.

(define packed 10)
(define-values (a b) (values 1 2))
(assert! (equal? (list packed a b) '(10 1 2)))
(define-values (e) (list 1 2))
(assert! (equal? e '(1 2)))
(define-values (one) (values 7))
(assert! (= one 7))

(define (in-a-body)
  (define-values (x y) (values 5 6))
  (define-values (z) (values 7))
  (define-values (w) (list 8 9))
  (list x y z w))
(assert! (equal? (in-a-body) '(5 6 7 (8 9))))

(assert! (equal? (let-values ([(a b) (values 1 2)] [(c) (values 3)]) (list a b c)) '(1 2 3)))
(assert! (equal? (let-values ([(l) (list 1 2)]) l) '(1 2)))
(assert! (eq? (let ([produced 'outer]) (let-values ([(a) (values produced)]) a)) 'outer))
(assert! (equal? (let*-values ([(a b) (values 1 2)] [(c) (values (+ a b))]) (list a b c)) '(1 2 3)))
(assert! (equal? (let ([x 1]) (let-values ([(x) (values 2)] [(y) (values x)]) (list x y))) '(2 1)))
