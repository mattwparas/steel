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

;; The arguments are evaluated where the call is written, in order, whatever
;; the function does with its rest list afterwards.

(define seen '())
(define (note! x)
  (set! seen (cons x seen))
  x)

(define-syntax check
  (syntax-rules ()
    [(_ expr expected-value expected-order)
     (begin
       (set! seen '())
       (let ([value expr])
         (assert! (equal? value expected-value))
         (assert! (equal? (reverse seen) expected-order))))]))

;; The rest list is applied behind a test that fails.
(define (behind-if y . args)
  (if (note! #f)
      (let ()
        (apply (lambda (a b) (list a b y)) args))
      'no))
(check (behind-if (note! 0) (note! 1) (note! 2)) 'no '(0 1 2 #f))

(define (behind-and y . args)
  (and (note! #f)
       (let ()
         (apply (lambda (a b) (list a b y)) args))))
(check (behind-and (note! 0) (note! 1) (note! 2)) #f '(0 1 2 #f))

(define (behind-or y . args)
  (or (note! #f)
      (let ()
        (apply (lambda (a b) (list a b y)) args))))
(check (behind-or (note! 0) (note! 1) (note! 2)) '(1 2 0) '(0 1 2 #f))

(define (in-one-arm y . args)
  (case y
    [(0)
     (let ()
       (apply (lambda (a b) (list a b y)) args))]
    [else 'no]))
(check (list (in-one-arm (note! 0) (note! 1) (note! 2)) (in-one-arm (note! 5) (note! 1) (note! 2)))
       '((1 2 0) no)
       '(0 1 2 5 1 2))

;; Something else in the body runs first.
(define (after-an-effect y . args)
  (note! 'body)
  (let ()
    (apply (lambda (a b) (list a b y)) args)))
(check (after-an-effect (note! 0) (note! 1) (note! 2)) '(1 2 0) '(0 1 2 body))

(define (two-lets-down y . args)
  (let ([z (note! 'z)])
    (begin
      (note! 'pre)
      (let ()
        (apply (lambda (a b) (list a b y z)) args)))))
(check (two-lets-down (note! 0) (note! 1) (note! 2)) '(1 2 0 z) '(0 1 2 z pre))

(define (in-a-binding y . args)
  (let ([k (if (note! #t)
               (let ()
                 (apply (lambda (a b) (list a b y)) args))
               'no)])
    (list k (note! 'after))))
(check (in-a-binding (note! 0) (note! 1) (note! 2)) '((1 2 0) after) '(0 1 2 #t after))

;; The applied function uses what the let around it binds.
(define (uses-a-parameter y . args)
  (apply (lambda (a b) (list a b y)) args))
(check (uses-a-parameter (note! 0) (note! 1) (note! 2)) '(1 2 0) '(0 1 2))

(define (beside-a-binding y . args)
  (let ([x (note! 'x)])
    (apply (lambda (a b) (list a b y)) args)))
(check (beside-a-binding (note! 0) (note! 1) (note! 2)) '(1 2 0) '(0 1 2 x))

(define (shadowing-a-binding y . args)
  (let ([a (note! 'shadowed)])
    (let ()
      (apply (lambda (a b) (list a b y)) args))))
(check (shadowing-a-binding (note! 0) (note! 1) (note! 2)) '(1 2 0) '(0 1 2 shadowed))

;; An argument is read before the body changes what it named.
(define cell 0)
(define (changes-the-cell y . args)
  (set! cell 9)
  (let ()
    (apply (lambda (a) (list a y cell)) args)))
(check (changes-the-cell 0 cell) '(0 0 9) '())

;; A keyword argument beside a rest list.
(define (keyword-and-rest #:y y . r)
  (list y r))
(check (keyword-and-rest (note! 0) #:y (note! 'ky)) '(ky (0 #:y ky)) '(0 ky))

;; Functions with rest parameters called where they are written.
(assert! (equal? ((lambda (x . r) ((lambda (y) (list x y r)) 5)) 0 1 2) '(0 5 (1 2))))
(assert! (equal? ((lambda (x) ((lambda (y . r) (list x y r)) 5 6 7)) 0) '(0 5 (6 7))))
(assert! (equal? ((lambda args
                    (let ([x 'x])
                      (apply (lambda (a b) (list a b x)) args)))
                  1
                  2)
                 '(1 2 x)))
