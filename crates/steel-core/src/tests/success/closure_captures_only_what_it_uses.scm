;; A mutex can't be serialized, so a closure that serializes did not capture it.
(define (sendable? f)
  (with-handler (lambda (_) #f)
                (serialize-value f)
                #t))

(define (use held n)
  (if held (* 10 n) n))

(define (around-a-lambda held)
  (define parts (map (lambda (n) (use held n)) (range 0 3)))
  (lambda () (map (lambda (x) (+ x 1)) parts)))

(define (two-deep held)
  (define parts (map (lambda (n) (use held n)) (range 0 3)))
  (lambda () (lambda () (map (lambda (x) (+ x 1)) parts))))

(define (through-a-lambda held)
  (define parts (list 1 2 3))
  (lambda () (map (lambda (x) (use held x)) parts)))

(assert! (sendable? (around-a-lambda (mutex))))
(assert! (equal? ((around-a-lambda (mutex))) '(1 11 21)))
(assert! (sendable? (two-deep (mutex))))
(assert! (equal? (((two-deep (mutex)))) '(1 11 21)))
(assert! (not (sendable? (through-a-lambda (mutex)))))
(assert! (equal? ((through-a-lambda (mutex))) '(10 20 30)))
