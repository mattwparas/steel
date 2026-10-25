;; Transformers that are plain code rather than one syntax-case.

(define-syntax (my-or2 stx)
  (define parts (syntax-e stx))
  (if (= (length parts) 3)
      (datum->syntax stx (list 'if (cadr parts) (cadr parts) (caddr parts)))
      (error "bad")))
(assert! (= (my-or2 #f 5) 5))

(define-syntax swap-args
  (lambda (stx)
    (let ([e (syntax-e stx)])
      (datum->syntax stx (list (cadr e) (caddr (cdr e)) (caddr e))))))
(assert! (= (swap-args - 1 10) 9))

(define-syntax (nest stx)
  (define inner (cadr (syntax-e stx)))
  (syntax-case inner ()
    [(a b) #'(+ a b)]))
(assert! (= (nest (3 4)) 7))

;; A failed fender goes on to the next clause
(define-syntax (ff stx)
  (syntax-case stx ()
    [(_ y) #f #'1]
    [(_ x) #'x]))
(assert! (= (ff 42) 42))

;; A function made in one expansion keeps that expansion's pattern variables
(begin-for-syntax
  (define saved #f))
(define-syntax (save stx)
  (syntax-case stx ()
    [(_ x)
     (begin
       (set! saved (lambda () #'(quote x)))
       #''saved)]))
(define-syntax (use stx)
  (syntax-case stx ()
    [(_ x) (saved)]))
(assert! (eq? (save first) 'saved))
(assert! (eq? (use second) 'first))

;; datum->syntax takes its context from a list too
(define-syntax (cap-whole stx)
  (syntax-case stx ()
    [(_ body)
     (with-syntax ([self (datum->syntax (cadr (syntax-e stx)) 'self)])
       #'(let ([self 42]) body))]))
(assert! (= (cap-whole (+ self 1)) 43))
(define-syntax (cap-key stx)
  (syntax-case stx ()
    [(_ k body)
     (with-syntax ([self (datum->syntax #'k 'self)])
       #'(let ([self 42]) body))]))
(assert! (= (cap-key x (+ self 1)) 43))
(define-syntax (cap-body stx)
  (syntax-case stx ()
    [(_ body)
     (with-syntax ([self (datum->syntax #'body 'self)])
       #'(let ([self 42]) body))]))
(assert! (= (cap-body (+ self 1)) 43))
