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

;; A list datum->syntax builds around template syntax keeps the template's scopes
(define-syntax (let-five stx)
  (syntax-case stx ()
    [(_ b) (datum->syntax stx (list #'let (list (list #'x 5)) #'b))]))
(define x 'outer)
(assert! (equal? (let-five x) 'outer))

;; Unquotes in a quasiquoted vector a template writes mean the template's binders
(define-syntax (vector-twice stx)
  (syntax-case stx ()
    [(_ e) #'(let ([x e]) `#(,x ,x))]))
(assert! (equal? (vector-twice 1) #(1 1)))
(define (vector-twice-of y)
  (vector-twice y))
(assert! (equal? (vector-twice-of 2) #(2 2)))

;; A transformer's own definitions don't shadow what the kernel generates around them
(define-syntax (own-case stx)
  (define (case x)
    (list 'k x))
  (syntax-case stx ()
    [(_) (datum->syntax stx (list 'quote (case 2)))]))
(assert! (equal? (own-case) '(k 2)))

;; So is a let-syntax transformer or a begin-for-syntax helper a template writes
(define-syntax def-let-syntax
  (syntax-rules ()
    [(_ m v body) (let-syntax ([m (lambda (stx) (datum->syntax stx v))]) body)]))
(assert! (equal? (def-let-syntax m 5 (+ (m) 1)) 6))
(define-syntax with-helper
  (syntax-rules ()
    [(_ name v)
     (begin
       (begin-for-syntax
         (define (name-helper s)
           (datum->syntax s v)))
       (define-syntax (name s)
         (name-helper s)))]))
(with-helper seven 7)
(assert! (equal? (seven) 7))

;; A template's binders keep their names, so case datums, struct fields and syntax->datum see
;; what the template wrote
(define-syntax classify
  (syntax-rules ()
    [(_ v)
     (let ([red 'r]
           [s v])
       (case s
         [(red) (list 'is-red red)]
         [else 'other]))]))
(assert! (equal? (list (classify 'red) (classify 'blue)) '((is-red r) other)))
(define-syntax field-named-like-binder
  (syntax-rules ()
    [(_)
     (let ([a 1])
       (struct pt3 (a))
       (pt3-a (pt3 a)))]))
(assert! (equal? (field-named-like-binder) 1))
(define-syntax (show-name stx)
  (syntax-case stx ()
    [(_ e) (with-syntax ([d (syntax->datum #'e)]) #''d)]))
(define-syntax binder-name
  (syntax-rules ()
    [(_) (let ([x 1]) (show-name x))]))
(assert! (equal? (binder-name) 'x))

;; datum->syntax with a template's context captures the template's binder of that name
(define-syntax (aif stx)
  (syntax-case stx ()
    [(k c t e) (with-syntax ([it (datum->syntax #'k 'it)]) #'(let ([it c]) (if it t e)))]))
(define-syntax aif-user
  (syntax-rules ()
    [(_ v) (let ([it 'outer]) (aif v (list 'got it) it))]))
(assert! (equal? (aif-user 7) '(got 7)))

;; A template's local shadows a local macro of its name for what the template wrote
(define (shadow-local-macro)
  (define-syntax m (syntax-rules () [(_ x) (list 'macro x)]))
  (define-syntax use (syntax-rules () [(_) (let ([m (lambda (x) (list 'func x))]) (m 1))]))
  (use))
(assert! (equal? (shadow-local-macro) '(func 1)))

;; A function a template defines at the top level is the template's, and a macro of its name
;; still works elsewhere
(define-syntax tagger (syntax-rules () [(_ x) (list 'macro x)]))
(define-syntax def-tagger
  (syntax-rules ()
    [(_ name)
     (begin
       (define (tagger x)
         (list 'func x))
       (define (name)
         (tagger 1)))]))
(def-tagger tag-go)
(assert! (equal? (list (tag-go) (tagger 2)) '((func 1) (macro 2))))

;; In transformer code, a binder named like a pattern variable is a variable
(define-syntax (names-of stx)
  (syntax-case stx ()
    [(_ (k v) ...)
     (let ([n (map (lambda (k) (syntax->datum k)) (syntax->list #'(k ...)))])
       #`(quote #,n))]))
(assert! (equal? (names-of (a 1) (b 2)) '(a b)))

;; A body's own recursive define hides an outer constant of its name
(define (inner-loop)
  (let ([loop 1])
    (define (loop i)
      (if (= i 4) i (loop (+ i 1))))
    (loop 0)))
(assert! (equal? (inner-loop) 4))
(define-syntax rep
  (syntax-rules ()
    [(_ n) (let loop ([i 0] [acc 0]) (if (= i n) acc (loop (+ i 1) (+ acc i))))]))
(define (rep-in-loop)
  (let ([loop 1])
    (list (rep 4) loop)))
(assert! (equal? (rep-in-loop) '(6 1)))

;; A local macro's literal that names a local matches that same local
(define (literal-local foo)
  (define-syntax m (syntax-rules (foo) [(_ foo) 'lit] [(_ y) 'not]))
  (list (m foo) (let ([foo 2]) (m foo))))
(assert! (equal? (literal-local 1) '(lit not)))

;; syntax->datum and syntax-e raise on what isn't syntax
(assert! (equal? (with-handler (lambda (e) 'caught) (syntax->datum 5)) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (syntax-e 5)) 'caught))

;; A transformer can be any expression that makes one, evaluated once
(define-syntax six
  (let ([k 6])
    (lambda (stx) (datum->syntax stx k))))
(assert! (equal? (six) 6))
(define-syntax counted
  (let ([n 0])
    (lambda (stx)
      (set! n (+ n 1))
      (datum->syntax stx n))))
(assert! (equal? (list (counted) (counted)) '(1 2)))

;; The syntax functions exist at run time too
(define stx-abc #'(a b c))
(assert! (equal? (map syntax-e (syntax->list stx-abc)) '(a b c)))
(assert! (identifier? (car (syntax->list stx-abc))))
(assert! (not (identifier? 5)))
(assert! (equal? (syntax->datum (datum->syntax stx-abc '(x y))) '(x y)))
(assert! (equal? (syntax->datum (quote-syntax (x . y))) '(x . y)))

;; A depth-1 variable under two ellipses repeats with the inner one only
(define-syntax pair-each
  (syntax-rules ()
    [(_ (a ...) (b ...)) '((a b ...) ...)]))
(assert! (equal? (pair-each (1 2) (x y z)) '((1 x y z) (2 x y z))))

;; generate-temporaries and define-for-syntax
(define-syntax (let-two stx)
  (syntax-case stx ()
    [(_ (a b) e body)
     (with-syntax ([(t1 t2) (generate-temporaries #'(a b))])
       #'(call-with-values (lambda () e) (lambda (t1 t2) (let ([a t1] [b t2]) body))))]))
(assert! (equal? (let-two (x y) (values 1 2) (list x y)) '(1 2)))
(define-for-syntax (twice-at-phase1 x)
  (* 2 x))
(define-syntax (doubled stx)
  (syntax-case stx ()
    [(_ n) (datum->syntax stx (twice-at-phase1 (syntax->datum #'n)))]))
(assert! (equal? (doubled 21) 42))

;; match repeats a list pattern over the rest of a list
(assert! (equal? (match '((1 2) (3 4)) [(list (list a b) ...) (list a b)] [_ 'no]) '((1 3) (2 4))))
(assert! (equal? (match '((1 2) (3)) [(list (list a ...) ...) a] [_ 'no]) '((1 2) (3))))

;; A variable the transformer binds hides a pattern variable of its name in templates too
(define-syntax (shadowed-in-template stx)
  (syntax-case stx ()
    [(_ x) (let ([f (lambda (x) #'x)]) #`(quote #,(f 1)))]))
(assert! (equal? (shadowed-in-template hello) 'x))
(define-syntax (shadowed-by-let stx)
  (syntax-case stx ()
    [(_ x) (let ([x 5]) #'(quote x))]))
(assert! (equal? (shadowed-by-let hello) 'x))

;; datum->syntax in a template is always the procedure, whatever its arguments
(define-syntax datum-of
  (syntax-rules ()
    [(_ c d) (syntax->datum (datum->syntax c d))]))
(define here-context #'here)
(define some-datum '(1 2))
(assert! (equal? (datum-of here-context some-datum) '(1 2)))
