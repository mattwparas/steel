;; `=>` passes the test's value to the clause's function, a clause with no body gives the test's value
(define (classify x)
  (cond [(assv x '((1 . one) (2 . two))) => cdr]
        [(memv x '(3 4))]
        [(> x 10) 'big 'really-big]
        [else 'other]))
(assert! (equal? (list (classify 1) (classify 3) (classify 20) (classify 5))
                 '(one (3 4) really-big other)))
(assert! (equal? (cond [#f 1] [(+ 1 2)]) 3))
(assert! (equal? (cond [1 => (lambda (v) (list v v))]) '(1 1)))

(define (in-a-function x)
  (cond [(assq x '((a . 1))) => (lambda (p) (cdr p))]
        [else 'none]))
(assert! (equal? (list (in-a-function 'a) (in-a-function 'b)) '(1 none)))

;; A `=>` clause whose test is false doesn't evaluate its function
(define evaluated #f)
(assert! (equal? (cond [#f => (begin (set! evaluated #t) car)] [else 'else-ran]) 'else-ran))
(assert! (not evaluated))

;; A local `=>` is a variable, not the keyword
(assert! (equal? (let ([=> #f]) (cond [#t => 'x])) 'x))
