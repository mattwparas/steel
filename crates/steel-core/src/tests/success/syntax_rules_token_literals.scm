;; Names the reader makes tokens (quote, if, define, ...) listed as syntax-rules
;; literals, which Racket allows. Expected values are Racket's.
(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

(define-syntax probe
  (syntax-rules (quote)
    [(_ (key (quote value))) (list 'quoted 'key (quote value))]
    [(_ (key (inner-key v) ...)) (list 'table 'key '(inner-key ...))]
    [(_ (key value)) (list 'plain 'key value)]))

(assert-equal! '(quoted args ()) (probe (args '())))
(assert-equal! '(quoted args ("fmt" "-i")) (probe (args '("fmt" "-i"))))
(assert-equal! '(quoted args (a b)) (probe (args (quote (a b)))))
(assert-equal! '(table config (experimental)) (probe (config (experimental (hash 'a 1)))))
(assert-equal! '(plain command "raco") (probe (command "raco")))

(define-syntax which
  (syntax-rules (if define)
    [(_ (if a b c)) 'an-if]
    [(_ (define x v)) 'a-define]
    [(_ other) 'something-else]))

(assert-equal! 'an-if (which (if 1 2 3)))
(assert-equal! 'a-define (which (define y 5)))
(assert-equal! 'something-else (which (f 1)))

;; Without the literal, a (quote x) pattern still matches a quoted form only
(define-syntax unlisted
  (syntax-rules ()
    [(_ (key (quote value))) 'quoted]
    [(_ (key value)) 'plain]))

(assert-equal! 'quoted (unlisted (args '())))
(assert-equal! 'plain (unlisted (args (f x))))
