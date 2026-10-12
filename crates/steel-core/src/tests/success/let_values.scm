(assert! (equal? (let-values ([(a b) (values 1 2)]) (list a b)) '(1 2)))
(assert! (equal? (let-values ([(a b) (values 1 2)] [(c d) (values 3 4)]) (list a b c d)) '(1 2 3 4)))
(assert! (equal? (let-values ([(a . rest) (values 1 2 3)] [all (values 4 5)]) (list a rest all))
                 '(1 (2 3) (4 5))))
(assert! (eq? (let-values () 'empty) 'empty))
(assert! (= (let-values ([(x) 5] [(y) (values 6)]) (+ x y)) 11))
(assert! (equal? (let-values ([(a) 1]) (list a)) '(1)))

;; Every expression is evaluated before anything is bound
(define a 'outer)
(assert! (equal? (let-values ([(a) (values 1)] [(b) (values a)]) (list a b)) '(1 outer)))
(define order '())
(define (note x)
  (set! order (cons x order))
  x)
(let-values ([(p) (note 1)] [(q) (note 2)] [(r) (note 3)])
  (assert! (equal? (list p q r) '(1 2 3))))
(assert! (equal? order '(3 2 1)))

;; In turn
(assert! (equal? (let*-values ([(a) (values 1)] [(b c) (values a (+ a 1))]) (list a b c)) '(1 1 2)))
(assert! (equal? (let*-values ([(a . rest) (values 1 2 3)] [all (values a rest)]) (list a rest all))
                 '(1 (2 3) (1 (2 3)))))
(assert! (equal? (let*-values ([(a) 1]) (list a)) '(1)))
(assert! (eq? (let*-values () 'empty) 'empty))

;; Names the expansion uses
(assert! (equal? (let-values ([(car cdr) (values 1 2)] [(apply produced) (values 3 4)])
                   (vector car cdr apply produced))
                 (vector 1 2 3 4)))

(define (in-a-function p)
  (let-values ([(q r) (values p (* p 2))] [(s) (values (+ p 1))])
    (list q r s)))
(define (many n acc)
  (if (= n 0) acc (many (- n 1) (+ acc (apply + (in-a-function n))))))
(assert! (= (many 3000 0) 18009000))
