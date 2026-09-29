;; Internal define of a macro name should only shadow it inside the body
(define-syntax my-mac
  (syntax-rules ()
    [(_ x) (list x)]))

(define (shadows-my-mac)
  (define my-mac 1)
  my-mac)

(define (uses-my-mac)
  (my-mac 10))

(assert! (equal? (shadows-my-mac) 1))
(assert! (equal? (uses-my-mac) '(10)))

;; with-handler expands to reset
(define (shadows-reset)
  (define reset 1)
  reset)

(define (uses-with-handler thunk)
  (with-handler (lambda (e) 'err) (thunk)))

(assert! (equal? (shadows-reset) 1))
(assert! (equal? (uses-with-handler (lambda () 42)) 42))
(assert! (equal? (uses-with-handler (lambda () (error "boom"))) 'err))


;; Locals shadow a macro from the same file
(define (param-shadows-my-mac my-mac)
  (my-mac 5))

(define (define-shadows-my-mac)
  (define (my-mac x) (* x 3))
  (my-mac 5))

(define (let-shadows-my-mac)
  (let ([my-mac (lambda (x) (+ x 1))])
    (my-mac 5)))

(assert! (equal? (param-shadows-my-mac (lambda (x) (* x 2))) 10))
(assert! (equal? (define-shadows-my-mac) 15))
(assert! (equal? (let-shadows-my-mac) 6))
(assert! (equal? (uses-my-mac) '(10)))


;; Local reset next to with-handler
(define (with-handler-beside-local-reset)
  (define reset 'mine)
  (list reset (with-handler (lambda (e) 'err) (error "boom"))))

(assert! (equal? (with-handler-beside-local-reset) '(mine err)))

(define (with-handler-beside-reset-param reset)
  (list reset (with-handler (lambda (e) 'err) 42)))

(assert! (equal? (with-handler-beside-reset-param 'mine) '(mine 42)))

;; Template that binds the macro name itself should use its own binding
(define-syntax call-with-local-my-mac
  (syntax-rules ()
    [(_ v) (let ([my-mac (lambda (x) (* x 100))]) (my-mac v))]))

(define (template-binder-beside-local my-mac)
  (list my-mac (call-with-local-my-mac 2)))

(assert! (equal? (template-binder-beside-local 'mine) '(mine 200)))
