;; Continuations, dynamic-wind and handlers crossing the jit boundary.
(define trace '())
(define (note x) (set! trace (cons x trace)))
(define (esc f) (call/cc (lambda (return) (f return))))

(assert! (equal? (esc (lambda (r) (r 'early) 'late)) 'early))
(assert! (equal? (esc (lambda (r) 'no-escape)) 'no-escape))

(define (find-first pred lst)
  (call/cc (lambda (return)
             (for-each (lambda (x) (if (pred x) (return x) void)) lst)
             #f)))

(assert! (equal? (find-first even? '(1 3 5 6 7)) 6))
(assert! (equal? (find-first even? '(1 3 5 7)) #false))

(dynamic-wind (lambda () (note 'in)) (lambda () (note 'body)) (lambda () (note 'out)))
(assert! (equal? (reverse trace) '(in body out)))

(assert! (equal? (with-handler (lambda (e) 'caught) (car '())) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (+ 1 (car '()))) 'caught))
