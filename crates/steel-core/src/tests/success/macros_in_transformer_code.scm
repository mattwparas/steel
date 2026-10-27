;; A procedural macro's output can define a procedural macro
(define-syntax (make-constant stx)
  (syntax-case stx ()
    [(_ name v) #'(define-syntax (name s) (syntax-case s () [(_) #'(quote v)]))]))
(make-constant seven 7)
(assert! (equal? (seven) 7))

(define (in-a-body)
  (define-syntax (make-local stx)
    (syntax-case stx () [(_ n v) #'(define-syntax (n s) (syntax-case s () [(_) #'(quote v)]))]))
  (make-local local-seven 7)
  (local-seven))
(assert! (equal? (in-a-body) 7))

;; With a quasisyntax template two levels deep
(define-syntax (make-adder stx)
  (syntax-case stx ()
    [(_ n k) #`(define-syntax (n s) (syntax-case s () [(_ x) #`(list x #,k)]))]))
(make-adder with-42 42)
(assert! (equal? (with-42 1) '(1 42)))

;; A transformer's internal define of a prelude macro's name is a variable
(define-syntax (shadows-when stx)
  (define (when x) (list 'fn-when x))
  (datum->syntax stx (list 'quote (when 1))))
(assert! (equal? (shadows-when) '(fn-when 1)))

(define-syntax (shadows-arrow stx)
  (let ()
    (define (-> a b) (list 'fn-arrow a b))
    (datum->syntax stx (list 'quote (-> 1 2)))))
(assert! (equal? (shadows-arrow) '(fn-arrow 1 2)))

;; A transformer's own syntax-rules macro can expand to a prelude macro
(define-syntax (second-of-list stx)
  (let ()
    (define-syntax second-of (syntax-rules () [(_ e) (match e [(list a b) b])]))
    (datum->syntax stx (list 'quote (second-of (list 1 2))))))
(assert! (equal? (second-of-list) 2))
