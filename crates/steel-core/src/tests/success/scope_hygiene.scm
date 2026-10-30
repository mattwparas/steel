;; A template's procedural macro means what its template wrote: the binder the same template
;; defines, in a body and at the top level
(define (f)
  (define-syntax gen
    (syntax-rules ()
      [(_ name) (begin (define hidden 'h)
                       (define-syntax (name stx) (syntax-case stx () [(_) #'hidden])))]))
  (gen foo)
  (foo))
(assert! (equal? (f) 'h))

(define-syntax gen-top
  (syntax-rules ()
    [(_ name) (begin (define hidden 1)
                     (define-syntax (name stx) (syntax-case stx () [(_) #'hidden])))]))
(gen-top top-foo)
(assert! (equal? (top-foo) 1))

;; A global passed through two procedural macros isn't caught by the outer one's binder
(define-syntax (mid s) (syntax-case s () [(_ e) #'(let ([t 2]) (list e))]))
(define-syntax (outer s) (syntax-case s () [(_ e) #'(let ([t 1]) (mid e))]))
(define t 'user-t)
(assert! (equal? (outer t) '(user-t)))

;; A procedural template's syntax-rules helper is its expansion's own, at the top level and in
;; a body
(define-syntax (def-with-helper stx)
  (syntax-case stx ()
    [(_ name v) #'(begin
                    (define-syntax helper (syntax-rules () [(_) v]))
                    (define (name) (helper)))]))
(def-with-helper a 1)
(def-with-helper b 2)
(assert! (equal? (list (a) (b)) '(1 2)))
(define (g)
  (def-with-helper c 3)
  (def-with-helper d 4)
  (list (c) (d)))
(assert! (equal? (g) '(3 4)))

;; free-identifier=? and syntax-case literals ask what a name means where it's used
(define-syntax (is-else? stx)
  (syntax-case stx ()
    [(_ x) (if (free-identifier=? #'x #'else) #''yes #''no)]))
(assert! (equal? (list (is-else? else) (let ([else 1]) (is-else? else))) '(yes no)))
(define-syntax (m-lit stx)
  (syntax-case stx (otherwise)
    [(_ otherwise) #''matched]
    [(_ x) #''not]))
(assert! (equal? (list (m-lit otherwise) (let ([otherwise 1]) (m-lit otherwise))) '(matched not)))

;; A transformer that returns syntax is hygienic without syntax-case
(define-syntax (my-or2 stx)
  (let ([parts (syntax-e stx)])
    (quasisyntax (let ([t #,(cadr parts)]) (if t t #,(caddr parts))))))
(define t2 5)
(assert! (equal? (my-or2 #f t2) 5))

;; with-syntax in phase 1 code sees the other phase 1 definitions
(define (rt-helper x) (list 'rt x))
(begin-for-syntax
  (define (private-scale n) (* n 10))
  (define (make-call n) (with-syntax ([v (private-scale n)]) #'(rt-helper v))))
(define-syntax (scaled s) (syntax-case s () [(_ n) (make-call (syntax->datum #'n))]))
(assert! (equal? (scaled 4) '(rt 40)))


;; datum->syntax takes its context from a literal, a list or () of the input as the input's,
;; and from a list a template built as the template's
(define it 1)
(define-syntax (with-it stx)
  (syntax-case stx ()
    [(_ ctx body) (with-syntax ([it (datum->syntax #'ctx 'it)]) #'(let ([it 10]) body))]))
(assert! (equal? (list (with-it 5 it) (with-it "s" it) (with-it (1 b) it) (with-it () it))
                 '(10 10 10 10)))
(define-syntax (with-template-it stx)
  (syntax-case stx ()
    [(_ k body) (with-syntax ([it (datum->syntax #'(k z) 'it)]) #'(let ([it 10]) body))]))
(assert! (equal? (with-template-it foo it) 1))

;; In a transformer a template writes, datum->syntax is the procedure, not the template's
;; identifier joining
(define-syntax phase1-getter
  (syntax-rules ()
    [(_ get v)
     (begin
       (begin-for-syntax
         (define p1 v))
       (define-syntax (get stx)
         (datum->syntax stx p1)))]))
(phase1-getter get-p1 1)
(assert! (equal? (get-p1) 1))
(define-syntax pattern-getter
  (syntax-rules ()
    [(_ get v) (define-syntax (get stx) (datum->syntax stx v))]))
(pattern-getter get-v 3)
(assert! (equal? (get-v) 3))
