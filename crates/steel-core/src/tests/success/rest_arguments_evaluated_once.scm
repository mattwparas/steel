;; An argument passed to a case-lambda or variadic function is evaluated once,
;; however deep the call ends up after inlining.

(define calls 0)
(define (tick x)
  (set! calls (+ calls 1))
  x)

(define other "x")
(define (in-dir file)
  (string-append "./" file))

(define one-or-none
  (case-lambda
    [() "none"]
    [(x) (string-append x "!")]))

(define (collect . xs)
  xs)

(define (go)
  (one-or-none (tick (in-dir (string-append other ".addr")))))

;; The call is inlined into the argument of another variadic call.
(define result (collect (go)))
(assert! (equal? result '("./x.addr!")))
(assert! (= calls 1))

(define (go-nested)
  (one-or-none (one-or-none (tick (in-dir (string-append other ".addr"))))))

(define nested (collect (go-nested) (go-nested)))
(assert! (equal? nested '("./x.addr!!" "./x.addr!!")))
(assert! (= calls 3))

(define (go-list)
  (collect (tick (in-dir other)) (tick (in-dir other))))

(define listed (collect (one-or-none (car (go-list)))))
(assert! (equal? listed '("./x!")))
(assert! (= calls 5))
