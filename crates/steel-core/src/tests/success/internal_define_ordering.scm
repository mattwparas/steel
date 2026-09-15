(define x (box 0))
(define (f)
  (set-box! x 1)
  (define (g)
    0)
  (define y (unbox x))
  y)

(assert! (= (f) 1))

(define (ordering)
  (define log '())
  (define (push! v)
    (set! log (cons v log)))
  (define a (push! 'a))
  (push! 'expr)
  (define b (push! 'b))
  (define c
    (begin
      (push! 'c)
      (push! 'c)))
  (define d (push! 'd))
  (reverse log))

(assert! (equal? (ordering) '(a expr b c c d)))
