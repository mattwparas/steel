(define (foo . x)
  x)

(define-syntax test/c
  (syntax-rules (=>)
    [(test/c (r (x => (y) ...)) ...) (list (foo (list x ...) (list y ...) r) ...)]))

(assert! (equal? (test/c (1 (10 => (500))) (2 (10 => (700)))) '(((10 10) (500) 1) ((10 10) (700) 2))))

(define-syntax test2
  (syntax-rules (=>)
    [(test2 (r (z) ...)) (list (list r z) ...)]))

(assert! (equal? (test2 (10 (20) (30) (40))) '((10 20) (10 30) (10 40))))
