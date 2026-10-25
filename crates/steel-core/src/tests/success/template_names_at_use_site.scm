;; A template's free names mean what they meant where the macro was defined,
;; whatever the use site binds

(define (later x)
  (list 'global x))

(define-syntax first-of
  (syntax-rules ()
    [(_ l) (car l)]))

(define-syntax call-later
  (syntax-rules ()
    [(_ v) (later v)]))

(define-syntax call-defined-after
  (syntax-rules ()
    [(_ v) (defined-after v)]))

;; Builtins
(define (first-under-let lst)
  (let ([car cdr])
    (list (car lst) (first-of lst))))
(assert! (equal? (first-under-let '(1 2)) '((2) 1)))

(define (first-under-param car lst)
  (first-of lst))
(assert! (equal? (first-under-param cdr '(1 2)) 1))

(define (first-under-define lst)
  (define car cdr)
  (first-of lst))
(assert! (equal? (first-under-define '(1 2)) 1))

(define (first-under-lambda lst)
  ((lambda (car) (first-of lst)) cdr))
(assert! (equal? (first-under-lambda '(1 2)) 1))

(define (first-under-named-let lst)
  (let loop ([car cdr]
             [n 1])
    (if (= n 0)
        (first-of lst)
        (loop car (- n 1)))))
(assert! (equal? (first-under-named-let '(1 2)) 1))

;; Globals defined before and after the macro
(define (later-under-let v)
  (let ([later (lambda (x) 'local)])
    (list (later v) (call-later v))))
(assert! (equal? (later-under-let 1) '(local (global 1))))

(define (later-under-param later)
  (call-later later))
(assert! (equal? (later-under-param 5) '(global 5)))

(define (defined-after x)
  (list 'after x))

(define (defined-after-under-let v)
  (let ([defined-after (lambda (x) 'local)])
    (call-defined-after v)))
(assert! (equal? (defined-after-under-let 1) '(after 1)))

;; A use site that sets its own variable of the same name
(define (later-under-set v)
  (let ([later 1])
    (set! later 2)
    (list later (call-later v))))
(assert! (equal? (later-under-set 3) '(2 (global 3))))

;; Variables the template binds don't capture the use site's
(define-syntax swap!
  (syntax-rules ()
    [(_ a b)
     (let ([tmp a])
       (set! a b)
       (set! b tmp))]))

(define (swap-tmp)
  (let ([tmp 1]
        [other 2])
    (swap! tmp other)
    (list tmp other)))
(assert! (equal? (swap-tmp) '(2 1)))

(define-syntax my-or
  (syntax-rules ()
    [(_ a b)
     (let ([t a])
       (if t t b))]))

(define (or-with-t t)
  (my-or #f t))
(assert! (equal? (or-with-t 7) 7))

;; A template that uses a builtin and also binds a variable named after one
(define-syntax sum-firsts
  (syntax-rules ()
    [(_ a b)
     (let ([list (car a)])
       (+ list (car b)))]))

(define (sum-firsts-under-let a b)
  (let ([car cdr])
    (sum-firsts a b)))
(assert! (equal? (sum-firsts-under-let '(1 2) '(10 20)) 11))

;; Macros used in other macros' templates
(define-syntax first-of-first
  (syntax-rules ()
    [(_ l) (first-of (first-of l))]))

(define (nested-under-let lst)
  (let ([car cdr])
    (first-of-first lst)))
(assert! (equal? (nested-under-let '((1 2) 3)) 1))

;; A macro defined in a body refers to the body's locals
(define (local-macro-under-let lst)
  (define-syntax local-first
    (syntax-rules ()
      [(_ l) (car l)]))
  (let ([car cdr])
    (local-first lst)))
(assert! (equal? (local-macro-under-let '(1 2)) 1))

(define (local-macro-captures x)
  (define-syntax add-x
    (syntax-rules ()
      [(_ v) (+ v x)]))
  (let ([x 100])
    (add-x 1)))
(assert! (equal? (local-macro-captures 10) 11))

;; Names a caller passes in mean what they mean at the use site
(define-syntax apply-to
  (syntax-rules ()
    [(_ f v) (f v)]))

(define (passed-in v)
  (let ([car (lambda (x) 'local)])
    (apply-to car v)))
(assert! (equal? (passed-in '(1 2)) 'local))

;; Quoted names are the names as written
(define-syntax quoted-car
  (syntax-rules ()
    [(_) 'car]))

(define (quoted-under-let)
  (let ([car 1])
    (quoted-car)))
(assert! (equal? (quoted-under-let) 'car))

;; A struct written by a template, used where the names are locals
(define-syntax with-cell
  (syntax-rules ()
    [(_ v)
     (let ()
       (struct cell (value))
       (cell-value (cell v)))]))

(define (cell-under-let cell cell-value)
  (list cell cell-value (with-cell 5)))
(assert! (equal? (cell-under-let 1 2) '(1 2 5)))

(define-syntax define-named-cell
  (syntax-rules ()
    [(_ name) (struct name (value))]))

(define-named-cell box-of)
(assert! (equal? (box-of-value (box-of 3)) 3))
