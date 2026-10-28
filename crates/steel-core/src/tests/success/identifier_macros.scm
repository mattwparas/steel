;; A procedural macro named alone is given the identifier
(define-syntax (tens s)
  (syntax-case s ()
    [(_ a) #'(list 'call a)]
    [_ #'(lambda (x) (* x 10))]))
(assert! (equal? (map tens '(1 2)) '(10 20)))
(assert! (equal? (tens 5) '(call 5)))
(define tens-value tens)
(assert! (equal? (tens-value 3) 30))

;; A local of the name hides it, as a parameter does
(assert! (equal? (let ([tens 1]) tens) 1))
(define (takes tens) tens)
(assert! (equal? (takes 7) 7))
(assert! (equal? 'tens 'tens))

;; From a template too
(define-syntax gives-tens (syntax-rules () [(_) tens]))
(assert! (equal? ((gives-tens) 4) 40))

;; A function's macro, which hides the parameter of its name
(define (local-id m)
  (define-syntax (m s) (syntax-case s () [(_) #''macro] [_ #''id]))
  (list m (m)))
(assert! (equal? (local-id 1) '(id macro)))
(define (through-rules m)
  (define-syntax (m s) (syntax-case s () [(_) #''macro] [_ #''id]))
  (define-syntax call-m (syntax-rules () [(_) (list (m) m)]))
  (call-m))
(assert! (equal? (through-rules 1) '(macro id)))

;; One clause for every form
(define-syntax (whole s)
  (syntax-case s () [x (identifier? #'x) #''ident] [(_ . r) #''form]))
(assert! (equal? (list whole (whole) (whole 1 2)) '(ident form form)))

;; R6RS's identifier-syntax
(define-syntax forty-two (identifier-syntax 42))
(assert! (equal? forty-two 42))
(define (first-of y)
  (define-syntax head (identifier-syntax (car y)))
  (list head (let ([y 'shadow]) head)))
(assert! (equal? (first-of '(1 2)) '(1 1)))
(define-syntax add1* (identifier-syntax (lambda (v) (+ v 1))))
(assert! (equal? (map add1* '(1 2)) '(2 3)))
(assert! (equal? (add1* 5) 6))

;; A set! transformer is given a set! of its name too
(define store (box 0))
(define-syntax cell
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! _ v) #'(set-box! store v)]
       [(_ a) #'(list 'called a)]
       [id (identifier? #'id) #'(unbox store)]))))
(set! cell 5)
(assert! (equal? (list cell (cell 1)) '(5 (called 1))))
(define (local-set-transformer x)
  (define-syntax loc
    (make-variable-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! _ v) #'(set! x (* v 2))]
         [id (identifier? #'id) #'x]))))
  (set! loc 21)
  loc)
(assert! (equal? (local-set-transformer 0) 42))
(assert! (equal? (let ([cell 1]) (set! cell 2) cell) 2))

;; And identifier-syntax's two clauses make one
(define slot-store (vector 0))
(define-syntax slot
  (identifier-syntax
   [id (vector-ref slot-store 0)]
   [(set! id e) (vector-set! slot-store 0 e)]))
(set! slot 9)
(assert! (equal? (list slot (+ slot 1)) '(9 10)))
(define-syntax set-slot (syntax-rules () [(_ e) (set! slot e)]))
(set-slot 77)
(assert! (equal? slot 77))
(define (local-slot)
  (define w 1)
  (define-syntax ww (identifier-syntax [i w] [(set! i e) (set! w (+ e 1000))]))
  (set! ww 5)
  (list ww w))
(assert! (equal? (local-slot) '(1005 1005)))
