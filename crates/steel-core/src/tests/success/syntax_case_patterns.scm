;; Nested patterns followed by an ellipsis
(define-syntax (swapped stx)
  (syntax-case stx ()
    [(_ (a b) ...) #'(quote ((b a) ...))]))
(assert! (equal? (swapped (1 2) (3 4)) '((2 1) (4 3))))

(define-syntax (my-let stx)
  (syntax-case stx ()
    [(_ ([n v] ...) body) #'((lambda (n ...) body) v ...)]))
(assert! (equal? (my-let ([x 1] [y 2]) (+ x y)) 3))

;; An ellipsis in the macro's input is data
(define-syntax spliced
  (syntax-rules ()
    [(_ e ...) (begin e ...)]))
(assert! (equal? (spliced (quote ((1 2) ...))) '((1 2) ...)))

;; Quasisyntax templates have ellipses, and #, and #,@ anywhere
(define-syntax (qs-pairs stx)
  (syntax-case stx ()
    [(_ (a b) ...) #`(quote ((a b) ...))]))
(assert! (equal? (qs-pairs (1 2) (3 4)) '((1 2) (3 4))))

(define-syntax (qs-sums stx)
  (syntax-case stx ()
    [(_ (a b) ...) #`(list (+ a b) ... #,(+ 1 2))]))
(assert! (equal? (qs-sums (1 2) (3 4)) '(3 7 3)))

(define-syntax (qs-doubled stx)
  (syntax-case stx ()
    [(_ a ...) #`(list #,@(map (lambda (x) (* 2 (syntax-e x))) (syntax-e #'(a ...))))]))
(assert! (equal? (qs-doubled 1 2 3) '(2 4 6)))

(define-syntax (qs-empty-splices stx)
  (syntax-case stx ()
    [(_ x) #`(list #,@'() x #,@'())]))
(assert! (equal? (qs-empty-splices 9) '(9)))

;; Vector templates
(define-syntax rules-vector
  (syntax-rules ()
    [(_ x ...) '#(1 x ...)]))
(assert! (equal? (rules-vector q r) #(1 q r)))

(define-syntax (case-vector stx)
  (syntax-case stx ()
    [(_ x ...) #'(quote #(1 x ...))]))
(assert! (equal? (case-vector q r) #(1 q r)))

(define-syntax (qs-vector stx)
  (syntax-case stx ()
    [(_ x) #`(quote #(1 #,@(list 2 3) x))]))
(assert! (equal? (qs-vector q) #(1 2 3 q)))

;; List patterns match special forms and quotes in the input
(define-syntax (applied? stx)
  (syntax-case stx ()
    [(_ (h rest ...)) #''matched]
    [(_ x) #''nomatch]))
(assert! (equal? (list (applied? (lambda (x) x)) (applied? (if 1 2 3)) (applied? (quote a)) (applied? 5))
                 '(matched matched matched nomatch)))

;; Fenders
(define-syntax (kind stx)
  (syntax-case stx ()
    [(_ x) (identifier? #'x) #''ident]
    [(_ x) (number? (syntax-e #'x)) #''num]
    [(_ x) #''other]))
(assert! (equal? (list (kind a) (kind 1) (kind "s")) '(ident num other)))

;; Dotted patterns and improper constants
(define-syntax (dotted stx)
  (syntax-case stx ()
    [(_ . rest) #'(list . rest)]))
(assert! (equal? (dotted 1 2) '(1 2)))

(define-syntax (pair-constant stx)
  (syntax-case stx ()
    [(_) (let ([p '(a . 1)]) (cdr p))]))
(assert! (equal? (pair-constant) 1))

;; The keyword position is a pattern variable too, so datum->syntax can capture with it
(define-syntax (loop stx)
  (syntax-case stx ()
    [(k e ...)
     (with-syntax ([break (datum->syntax #'k 'break)])
       #'(call-with-current-continuation (lambda (break) (let f () e ... (f)))))]))
(define (count-to-four)
  (let ([n 0])
    (loop (set! n (+ n 1)) (when (> n 3) (break n)))))
(assert! (equal? (count-to-four) 4))

;; let-syntax and letrec-syntax
(assert! (equal? (let-syntax ([f (syntax-rules () [(_ x) 'ls])]) (f 1)) 'ls))
(define (with-local-macro y)
  (let-syntax ([f (syntax-rules () [(_ x) (list x y)])]) (f 1)))
(assert! (equal? (with-local-macro 2) '(1 2)))
(assert! (letrec-syntax ([ev? (syntax-rules () [(_ n) (if (= n 0) #t (od? (- n 1)))])]
                         [od? (syntax-rules () [(_ n) (if (= n 0) #f #t)])])
           (ev? 2)))

;; A local macro names the parameter it sees, even one named like a global
(define (local-list list)
  (define-syntax mk (syntax-rules () [(_ a) (list a)]))
  (mk 3))
(assert! (equal? (local-list (lambda (x) (cons 'mine x))) '(mine . 3)))

(define (local-case x)
  (define-syntax (getx stx) (syntax-case stx () [(_) #'x]))
  (let ([x 'a])
    (let ([x 'b])
      (list x (getx)))))
(assert! (equal? (local-case 'outer) '(b outer)))

;; Identifier comparisons and syntax->list
(define-syntax (same? stx)
  (syntax-case stx ()
    [(_ a b) (if (bound-identifier=? #'a #'b) #''same #''diff)]))
(assert! (equal? (list (same? x x) (same? x y)) '(same diff)))

(define-syntax (introduced? stx)
  (syntax-case stx ()
    [(_ a) (if (bound-identifier=? #'a #'tmp) #''same #''diff)]))
(assert! (equal? (introduced? tmp) 'diff))

(define-syntax (else? stx)
  (syntax-case stx ()
    [(_ a) (if (free-identifier=? #'a #'else) #''else #''other)]))
(assert! (equal? (list (else? else) (else? foo)) '(else other)))

(define-syntax (counted stx)
  (syntax-case stx ()
    [(_ a ...) #`(quote #,(length (syntax->list #'(a ...))))]))
(assert! (equal? (counted 1 2 3) 3))

;; with-syntax patterns
(define-syntax (tens stx)
  (syntax-case stx ()
    [(_ a ...)
     (with-syntax ([(b ...) (map (lambda (x) (datum->syntax x (* 10 (syntax-e x))))
                                 (syntax->list #'(a ...)))])
       #'(list b ...))]))
(assert! (equal? (tens 1 2 3) '(10 20 30)))

(define-syntax (flipped stx)
  (syntax-case stx ()
    [(_ (n v) ...)
     (with-syntax ([((nn vv) ...) #'((v n) ...)])
       #'(quote ((nn . vv) ...)))]))
(assert! (equal? (flipped (a 1) (b 2)) '((1 . a) (2 . b))))

;; A function in a body refers to a later definition, and a transparent struct prints
;; with its getters
(define (forward y)
  (define k (list 1 2))
  (define opts (hash 'a (lambda (obj) (list pa obj))))
  (define pa (list-ref k 0))
  ((hash-ref opts 'a) y))
(assert! (equal? (forward 1) '(1 1)))

(define (local-struct y)
  (struct pt (a b) #:transparent)
  (pt-b (pt 1 y)))
(assert! (equal? (local-struct 7) 7))


;; What a local syntax-case macro makes can use other macros
(define (local-kernel-output)
  (define-syntax m (lambda (stx) (syntax-case stx () [(_) #'(cond [else (when #t 1)])])))
  (m))
(assert! (equal? (local-kernel-output) 1))

;; A with-syntax pattern shadows the clause's pattern variables, and `_` is a wildcard
(define-syntax (shadowed stx)
  (syntax-case stx ()
    [(_ x y) (with-syntax ([(x) #'(y)]) #'(quote x))]))
(assert! (equal? (shadowed a b) 'b))
(define-syntax (wild stx)
  (syntax-case stx ()
    [(_) (with-syntax ([(_ b) #'(1 2)]) #'(list b))]))
(assert! (equal? (wild) '(2)))

;; Quotes before an ellipsis, and quoted input against a list pattern
(define-syntax quoted-each (syntax-rules () [(_ x ...) (list 'x ...)]))
(assert! (equal? (quoted-each a b c) '(a b c)))
(define-syntax (quoted-arg stx)
  (syntax-case stx () [(_ (a ...)) #'(quote (a ...))]))
(assert! (equal? (quoted-arg '5) ''5))

;; #, in a dotted tail, an improper #,@, a #,@ inside an ellipsis template, a mutable vector
(define-syntax (dotted-hole stx) (syntax-case stx () [(_) #`(quote (1 . #,(+ 1 1)))]))
(assert! (equal? (dotted-hole) '(1 . 2)))
(define-syntax (improper-splice stx) (syntax-case stx () [(_) #`(quote (a #,@(cons 1 2)))]))
(assert! (equal? (improper-splice) '(a 1 . 2)))
(define-syntax (splice-each stx)
  (syntax-case stx () [(_ a ...) #`(list (list a #,@(list 8 9)) ...)]))
(assert! (equal? (splice-each 1 2) '((1 8 9) (2 8 9))))
(define-syntax (vector-hole stx) (syntax-case stx () [(_) #`(quote #,(vector 1 2))]))
(assert! (equal? (vector-hole) #(1 2)))

;; A local macro doesn't take over a name a global macro's template introduces
(define-syntax my-unless (syntax-rules () [(_ c e) (when (not c) e)]))
(assert! (equal? (let-syntax ([when (syntax-rules () [(_ . r) 'hijacked])]) (my-unless #f 1)) 1))

;; A local procedural macro hides a global one of its name, also one named like a prelude
;; macro
(define-syntax shadow-me (syntax-rules () [(_) 1]))
(define (local-procedural)
  (let ()
    (define-syntax (shadow-me stx) (syntax-case stx () [(_) #'2]))
    (shadow-me)))
(assert! (equal? (list (local-procedural) (shadow-me)) '(2 1)))
(define (local-when)
  (define-syntax when (lambda (stx) (syntax-case stx () [(_ c e ...) #''local-when])))
  (list (when 1 2) (case 1 [(1) 'one])))
(assert! (equal? (local-when) '(local-when one)))

;; A local macro's free name means the global, not a variable where it's used
(define bare-global 'global)
(define (bare-template)
  (define-syntax m (syntax-rules () [(_) bare-global]))
  (let ([bare-global 'inner]) (m)))
(assert! (equal? (bare-template) 'global))

;; Keyword argument defaults are locals a local macro sees
(define (with-scale #:scale [scale 2] x)
  (define-syntax scaled (syntax-rules () [(_ a) (* scale a)]))
  (scaled x))
(assert! (equal? (list (with-scale 3) (with-scale 3 #:scale 10)) '(6 30)))

;; A transparent struct works at phase 1, where display isn't defined
(begin-for-syntax
  (struct phase1-info (name count) #:transparent)
  (define phase1-counter (phase1-info 'c 4)))
(define-syntax (phase1-count stx)
  (syntax-case stx () [(_) (with-syntax ([n (phase1-info-count phase1-counter)]) #'(list n))]))
(assert! (equal? (phase1-count) '(4)))

;; #,@ splices inside an ellipsis escape, and `x ... ...` flattens
(define-syntax (escaped-splice stx)
  (syntax-case stx () [(_) #`(quote (... (a #,@(list 1 2) b)))]))
(assert! (equal? (escaped-splice) '(a 1 2 b)))
(define-syntax flat-rules (syntax-rules () [(_ ((a b ...) ...)) (quote (b ... ...))]))
(assert! (equal? (flat-rules ((1 2 3) (4 5))) '(2 3 5)))
(define-syntax (flat-case stx)
  (syntax-case stx ()
    [(_ x) (with-syntax ([((a b ...) ...) #'x]) #'(quote (b ... ...)))]))
(assert! (equal? (flat-case ((1 2 3) (4 5))) '(2 3 5)))

;; with-syntax evaluates its right hand sides before binding any pattern
(define-syntax (parallel-bindings stx)
  (syntax-case stx () [(_) (with-syntax ([a #'1] [c #'a]) #'(quote (a c)))]))
(assert! (equal? (parallel-bindings) '(1 a)))

;; Pattern variables are lexical, not seen by a helper the clause calls
(define-syntax (helper-template stx)
  (define (make) #'(quote x))
  (syntax-case stx () [(_ x) (make)]))
(assert! (equal? (helper-template 5) 'x))

;; syntax-case as an expression in a transformer
(define-syntax (nested-case stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-syntax ([(r ...) (map (lambda (e)
                                   (syntax-case e (=>)
                                     [(=> v) #''arrow]
                                     [(a b ...) (identifier? #'a) #'(list 'id b ...)]
                                     [_ #''other]))
                                 (syntax->list #'(x ...)))])
       #'(list r ...))]))
(assert! (equal? (nested-case (=> 1) (f 1 2) 5) '(arrow (id 1 2) other)))

;; Macros defined in what a procedural macro makes, or in its input
(define-syntax (make-dbl stx)
  (syntax-case stx () [(_ name) #'(define-syntax name (syntax-rules () [(_ x) (list x x)]))]))
(make-dbl dbl)
(assert! (equal? (dbl 1) '(1 1)))
(define-syntax (pass stx) (syntax-case stx () [(_ body) #'body]))
(assert! (equal? (pass (let-syntax ([foo (syntax-rules () [(_ x) (list x x)])]) (foo 2))) '(2 2)))

;; A begin-for-syntax a macro makes, each with its own template variables
(define-syntax def-phase1
  (syntax-rules () [(_ name v) (begin-for-syntax (define tmp v) (define (name) tmp))]))
(def-phase1 phase1-a 1)
(def-phase1 phase1-b 2)
(define-syntax (phase1-both stx)
  (syntax-case stx () [(_) (with-syntax ([v (list (phase1-a) (phase1-b))]) #''v)]))
(assert! (equal? (phase1-both) '(1 2)))

;; define-values keeps what a template's free name means
(define getx-value 'global)
(define-syntax getx (syntax-rules () [(_) getx-value]))
(define (values-of getx-value) (define-values (p q) (values 1 (getx))) q)
(assert! (equal? (values-of 'param) 'global))

;; let-syntax templates see the macros outside it, letrec-syntax ones its own
(define-syntax outer-a (syntax-rules () [(_) 'outer]))
(define (non-recursive)
  (let-syntax ([outer-a (syntax-rules () [(_) 'inner])]
               [b (syntax-rules () [(_) (list 'b (outer-a))])])
    (list (outer-a) (b))))
(assert! (equal? (non-recursive) '(inner (b outer))))
(assert! (equal? (letrec-syntax ([ev? (syntax-rules () [(_) #t] [(_ x . r) (od? . r)])]
                                 [od? (syntax-rules () [(_) #f] [(_ x . r) (ev? . r)])])
                   (list (ev? 1 2) (od? 1 2 3)))
                 '(#t #t)))

;; Vectors made at expansion time become vector syntax
(define-syntax (made-vector stx)
  (syntax-case stx () [(_) #`(quote #,(datum->syntax stx (vector 1 2)))]))
(assert! (equal? (made-vector) #(1 2)))
(define-syntax (nested-vector stx)
  (syntax-case stx () [(_ a) (datum->syntax stx (list 'quote (vector (vector 1) #'a)))]))
(assert! (equal? (nested-vector q) #(#(1) q)))

;; A pattern inside a clause binds its own variables, also ones the clause binds
(define-syntax (rebinds-with-syntax stx)
  (syntax-case stx () [(_ a) (with-syntax ([(a ...) #'(1 2 3)]) #'(list a ...))]))
(assert! (equal? (rebinds-with-syntax 7) '(1 2 3)))
(define-syntax (rebinds-syntax-case stx)
  (syntax-case stx () [(_ a ...) (syntax-case #'(x y) () [(p a) #'(quote (p a))])]))
(assert! (equal? (rebinds-syntax-case 7 8 9) '(x y)))

;; A macro that makes a begin of a definition splices it into the body it's in
(define-syntax def-named
  (syntax-rules () [(_ name v) (begin (define-syntax name (syntax-rules () [(_) v])))]))
(define (spliced-definition)
  (def-named loc 42)
  (+ 1 (loc)))
(assert! (equal? (spliced-definition) 43))

;; let-syntax templates mean what the name means outside, a variable too
(define (outer-fn x) (list 'outer x))
(assert! (equal? (let-syntax ([outer-fn (syntax-rules () [(_) (outer-fn 1)])]) (outer-fn))
                 '(outer 1)))
(define (outer-thunk) 'outer-thunk)
(assert! (equal? (let-syntax ([inner-one (syntax-rules () [(_) 'inner])]
                              [outer-thunk (syntax-rules () [(_) (list 'mac (outer-thunk))])])
                   (outer-thunk))
                 '(mac outer-thunk)))

;; What a local macro's template names is where the macro is defined
(define (definition-site)
  (define-syntax loc (syntax-rules () [(_) 'loc]))
  (define-syntax uses-loc (syntax-rules () [(_) (loc)]))
  (define (inner)
    (define-syntax loc (syntax-rules () [(_) 'inner-loc]))
    (uses-loc))
  (inner))
(assert! (equal? (definition-site) 'loc))
(define (template-local-macro)
  (define-syntax lm (syntax-rules () [(_ e) (list 'lm e)]))
  (define-syntax use-lm (syntax-rules () [(_ e) (lm e)]))
  (let ([lm 0]) (use-lm 'ok)))
(assert! (equal? (template-local-macro) '(lm ok)))

;; An optional parameter hides a procedural macro of its name
(define-syntax (param-named stx) (syntax-case stx () [(_ x) #''macro]))
(define (with-default [param-named (lambda (x) (list 'default x))]) (param-named 1))
(assert! (equal? (with-default) '(default 1)))

;; define-syntax-rule, at both phases
(define-syntax-rule (sq x) (* x x))
(define-syntax-rule (my-list e ...) (list e ...))
(begin-for-syntax (define-syntax-rule (cube x) (* x x x)))
(define-syntax (use-cube stx) (syntax-case stx () [(_) (datum->syntax stx (cube 3))]))
(assert! (equal? (list (sq 3) (my-list 1 2) (use-cube)) '(9 (1 2) 27)))

;; A definition in an expression's begin, like the one when makes, stays in it
(define-syntax which (syntax-rules () [(_) 'global-which]))
(define (in-when c)
  (define inside (when c (define-syntax which (syntax-rules () [(_) 'inner-which])) (which)))
  (list inside (which)))
(assert! (equal? (in-when #t) '(inner-which global-which)))

;; A macro a body's macro uses can come from a begin a macro makes later in the body
(define-syntax spliced-b (syntax-rules () [(_) 'global-b]))
(define-syntax def-b (syntax-rules () [(_ n) (begin (define-syntax n (syntax-rules () [(_) 'local-b])))]))
(define (later-group)
  (define-syntax uses-b (syntax-rules () [(_) (spliced-b)]))
  (def-b spliced-b)
  (list (uses-b) (spliced-b)))
(assert! (equal? (later-group) '(local-b local-b)))

;; What a macro-defining macro's template names means what it does there
(define-syntax hygiene-helper (syntax-rules () [(_) 'global-helper]))
(define hygiene-x 'global-x)
(define-syntax make-getters
  (syntax-rules ()
    [(_ h v) (begin (define-syntax h (syntax-rules () [(_) (hygiene-helper)]))
                    (define-syntax v (syntax-rules () [(_) hygiene-x])))]))
(define (made-in-a-body)
  (define-syntax hygiene-helper (syntax-rules () [(_) 'local-helper]))
  (define hygiene-x 'local-x)
  (make-getters get-h get-x)
  (list (get-h) (get-x)))
(assert! (equal? (made-in-a-body) '(global-helper global-x)))

;; Names in a dotted part of a macro's input are the user's
(define-syntax (split-pair stx) (syntax-case stx () [(_ (a . b)) #'(list a b)]))
(define (pair-of p q) (split-pair (p . q)))
(assert! (equal? (pair-of 1 2) '(1 2)))

;; A transformer can match something other than its input, or not use syntax-case last
(define-syntax (second-part stx)
  (define e (cadr (syntax-e stx)))
  (syntax-case e () [(a b) #'(quote (got a b))]))
(assert! (equal? (second-part (1 2)) '(got 1 2)))
(define-syntax (plain-transformer stx) (datum->syntax stx (list 'quote 5)))
(assert! (equal? (plain-transformer) 5))

;; After a fender says no, the next clause doesn't see what that clause bound
(define-syntax (fender-scope stx)
  (syntax-case stx () [(_ (x y)) #f #'(quote matched)] [_ #'(quote x)]))
(assert! (equal? (fender-scope (1 2)) 'x))

;; A let-syntax macro's template means the global macro of its name
(define-syntax same-name (syntax-rules () [(_) 'outer]))
(assert! (equal? (let-syntax ([same-name (syntax-rules () [(_) (list 'inner (same-name))])]) (same-name))
                 '(inner outer)))

;; define-values at phase 1 takes one value as it is
(begin-for-syntax (define-values (one-at-phase1) 8))
(define-syntax (read-phase1 stx) (syntax-case stx () [(_) (datum->syntax stx one-at-phase1)]))
(assert! (equal? (read-phase1) 8))

;; Quoting a macro definition in a procedural macro's output is data
(define-syntax (show-form stx) (syntax-case stx () [(_ form) #''form]))
(assert! (equal? (show-form (define-syntax q (syntax-rules () [(_ a) a])))
                 '(define-syntax q (syntax-rules () ((_ a) a)))))
