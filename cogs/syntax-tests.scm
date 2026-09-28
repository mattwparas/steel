(require-builtin steel/process)
(require-builtin steel/transducers)
(require-builtin steel/meta)

(require "tests/unit-test.scm")

(define-syntax check-syntax-error?
  (syntax-rules (skip)
    [(_ skip name input expected) (skip-compile (check-syntax-error? name input expected))]
    [(_ name input expected) (check-syntax-error-impl? name input expected)]))

(define (check-syntax-error-impl? name input expected)
  (define error-message (~> (run! (Engine::new) input) Err->value error-object-message))
  (define message-assert
    (or (string-contains? error-message expected) `(message= ,error-message expected= ,expected)))
  (check-equal? (string-join `(,name " [compilation fails]")) message-assert #t))

(check-syntax-error? "empty transformer"
                     '((define-syntax no-body
                         (syntax-rules ()
                           [(_ a)])))
                     "syntax-rules requires only one pattern to one body")

(check-syntax-error? "repeated pattern variables"
                     '((define-syntax repeated-vars
                         (syntax-rules ()
                           [(_ a (b a)) a])))
                     "repeated pattern variable a")

(check-syntax-error? "repeated pattern variables, macro name"
                     '((define-syntax repeated-vars-macro-name
                         (syntax-rules ()
                           [(foo a (b foo)) a])))
                     "repeated pattern variable foo")

(check-syntax-error? "multiple ellipsis"
                     '((define-syntax many-ellipsis
                         (syntax-rules ()
                           [(_ (a ... b ...)) (a ...)])))
                     "pattern with more than one ellipsis")

(check-syntax-error? "ellipsis in cdr"
                     '((define-syntax ellipsis-tail
                         (syntax-rules ()
                           [(_ (a . ...)) (a ...)])))
                     "ellipsis cannot appear as list tail")

(check-syntax-error? "ellipsis in nested cdr"
                     '((define-syntax ellipsis-tail-nested
                         (syntax-rules ()
                           [(_ ((a) . ...)) (a ...)])))
                     "ellipsis cannot appear as list tail")

(check-syntax-error? "ellipsis in macro name"
                     '((define-syntax ellipsis-name
                         (syntax-rules ()
                           [(ellipsis-name ...) 1])))
                     "cannot bind pattern to ellipsis")

(check-syntax-error? "ellipsis in dummy macro name"
                     '((define-syntax ellipsis-alt-name
                         (syntax-rules ()
                           [(potato ...) 1])))
                     "macro name cannot be followed by ellipsis")

(check-syntax-error? "ellipsis in macro wildcard"
                     '((define-syntax ellipsis-name-wildcard
                         (syntax-rules ()
                           [(_ ...) 1])))
                     "macro name cannot be followed by ellipsis")

(define-syntax alt-name
  (syntax-rules ()
    [(potato a) a]))

(check-equal? "macro name in pattern is irrelevant" (alt-name 1) 1)

(check-syntax-error? "macro name in pattern cannot be used"
                     '((define-syntax alt-name
                         (syntax-rules ()
                           [(potato a) a]))
                       (potato 1))
                     "Cannot reference an identifier before its definition: potato")

(check-syntax-error? "macro name in pattern does not capture"
                     '((define-syntax alt-name2
                         (syntax-rules ()
                           [(potato a) a]
                           [(potato a b) (potato b)]))
                       (potato 1 2))
                     "Cannot reference an identifier before its definition: potato")

(check-syntax-error? "bad spread"
                     '((define-syntax bad-spread
                         (syntax-rules ()
                           [(_ a ...)
                            a ...])))
                     "syntax-rules requires only one pattern to one body")

(define-syntax multiple-ellipsis
  (syntax-rules ()
    [(_ (a ...) ...) '((a ...) ...)]))

(check-equal? "multiple, nested ellipsis" (multiple-ellipsis (1) (a b)) '((1) (a b)))

(define-syntax multiple-ellipsis-vectors
  (syntax-rules ()
    [(_ #(a ...) ...) #((a ...) ...)]))

(skip-compile (check-equal? "multiple, nested ellipsis, vectors"
                            (multiple-ellipsis-vectors #(1) #(a b))
                            #((1) (a b))))

(define-syntax vector-spread
  (syntax-rules ()
    [(_ a ...) #(a ...)]))

(skip-compile (check-equal? "ellipsis spread in vector" (vector-spread 1 2 3) #(1 2 3)))

(define-syntax vector-spread-multiple
  (syntax-rules ()
    [(_ (a ...) ...) '(#((a #f) ...) ...)]))

(skip-compile (check-equal? "ellipsis spread in vector, nested"
                            (vector-spread-multiple (1) (2 3))
                            '(#((1 #f)) #((2 #f) (3 #f)))))

(define-syntax catchall
  (syntax-rules ()
    [(_ a ...) '(a ...)]))

(check-equal? "catch-all, 0 args" (catchall) '())
(check-equal? "catch-all, 1 arg" (catchall a) '(a))
(check-equal? "catch-all, 2 args" (catchall a b) '(a b))

(define-syntax catchall-but-one
  (syntax-rules ()
    [(_ a ... b) b]))

(check-equal? "catch-all but one, 1 arg" (catchall-but-one 'x) 'x)
(check-equal? "catch-all but one, 2 args" (catchall-but-one x '(y)) '(y))
(check-equal? "catch-all but one, 3 arg" (catchall-but-one x y '(z)) '(z))

(define-syntax wildcards
  (syntax-rules ()
    [(_ a _ (b _)) '(a b)]))

(check-equal? "wildcards" (wildcards x ignored (y (ignored2))) '(x y))

(check-syntax-error? "wildcards in expansion"
                     '((define-syntax wildcard-vars
                         (syntax-rules ()
                           [(wildcard-vars _) _]))
                       (wildcard-vars 1))
                     "Cannot reference an identifier before its definition: _")

(check-syntax-error? "catch-all but one, 0 args"
                     '((define-syntax catchall-but-one
                         (syntax-rules ()
                           [(_ a ... b) b]))
                       (catchall-but-one))
                     "unable to match case")

(define-syntax catchall-list
  (syntax-rules ()
    [(_ (a b) ...) (quote (a ...))]))

(check-equal? "catch-all lists, 0 args" (catchall-list) '())
(check-equal? "catch-all lists, 1 arg" (catchall-list (a b)) '(a))
(check-equal? "catch-all lists, 2 args" (catchall-list (a b) (c d)) '(a c))

(define bound-x 3)

(define-syntax lexical-capture
  (syntax-rules ()
    [(_) bound-x]))

;; TODO: Fix lexical captures so that the values are
;; actually yoinked properly at _definition_ time.
(let ([bound-x 'inner]) (check-equal? "hygiene, lexical capture" (lexical-capture) 3))

(check-equal? "improper lists in syntax"
              ;; equivalent to (let [(x 1)] x)
              (let . ([(x . (1 . ()))
                       . ()]
                      . [x
                         . ()])
                )
              1)

(check-syntax-error? "improper list pattern, constant mismatch"
                     '((define-syntax const-tail
                         (syntax-rules ()
                           [(_ (a . #t)) a]))
                       (const-tail (x . 1)))
                     "unable to match case")

(check-syntax-error? "improper list does not match proper list patterns"
                     '((define-syntax const-tail
                         (syntax-rules ()
                           [(_ (a #t)) a]))
                       (const-tail (x . #t)))
                     "unable to match case")

(check-syntax-error? "improper list pattern, tail pattern does not match"
                     '((define-syntax const-tail
                         (syntax-rules ()
                           [(_ (a . (b))) b]))
                       (const-tail (x)))
                     "unable to match case")

(check-syntax-error? "improper list pattern, ellipsis make pattern greedy"
                     '((define-syntax const-tail
                         (syntax-rules ()
                           [(_ (a (b) ... c . rest)) a]))
                       (const-tail (a? c? d?)))
                     "unable to match case")

(define-syntax improper-tail
  (syntax-rules ()
    [(_ a . b) (quote b)]))

(check-equal? "improper list pattern, tail arguments, 0 args" (improper-tail x) '())
(check-equal? "improper list pattern, tail arguments, 1 args" (improper-tail x y) '(y))
(check-equal? "improper list pattern, tail arguments, 2 args" (improper-tail x y z) '(y z))
(check-equal? "improper list pattern, tail arguments, improper list in cdr"
              (improper-tail x y . z)
              '(y . z))
(check-equal? "improper list pattern, tail arguments, constant in cdr" (improper-tail x . "y") "y")

(define-syntax improper-tail-const
  (syntax-rules ()
    [(_ a . #t) (quote a)]))

(check-equal? "improper list pattern, constant tail" (improper-tail-const x . #t) 'x)

(define-syntax improper-tail-nested
  (syntax-rules ()
    [(_ (a . b)) (quote b)]))

(check-equal? "improper list pattern, nested, tail arguments, 0 args" (improper-tail-nested (x)) '())
(check-equal? "improper list pattern, nested, tail arguments, 1 args"
              (improper-tail-nested (x y))
              '(y))
(check-equal? "improper list pattern, nested, tail arguments, 2 args"
              (improper-tail-nested (x y z))
              '(y z))
(check-equal? "improper list pattern, nested, tail arguments, constant in cdr"
              (improper-tail-nested (x . "y"))
              "y")

(define-syntax improper-tail-collapsed
  (syntax-rules ()
    [(_ a . (b)) (quote b)]))

(check-equal? "improper list pattern, collapsed" (improper-tail-collapsed x y) 'y)

(define-syntax non-list-as-list
  (syntax-rules ()
    [(_ (a ... . b)) b]))

(check-equal? "improper list pattern, collapses to non-list" (non-list-as-list "hello") "hello")

(define-syntax non-list-as-list-nested
  (syntax-rules ()
    [(_ ((a) ... . b)) b]))

(check-equal? "improper list pattern, nested, collapses to non-list"
              (non-list-as-list-nested "hello")
              "hello")

(define-syntax non-list-as-list-multiple
  (syntax-rules ()
    [(_ ((a) ... . b) ...) (quote (b ...))]))

(check-equal? "improper list pattern, multiple, collapses to non-list"
              (non-list-as-list-multiple "hello" "world")
              '("hello" "world"))

(define-syntax many-literals
  (syntax-rules ()
    [(_ #t ...) 1]))

(check-equal? "ellipsis after literal" (many-literals #t #t #t) 1)

(check-syntax-error? "ellipsis tail, with non-nested"
                     '((define-syntax ellipsis-tail-literals
                         (syntax-rules ()
                           [(_ (1 . ...)) #f])))
                     "ellipsis cannot appear as list tail")

(define-syntax t
  (syntax-rules ()
    [(t a)
     (begin
       (define/contract (_t b)
         (->/c number? number?)
         (add1 b))

       (_t a))]))

(check-equal? "macro expansion correctly works within another syntax rules" (t 10) 11)

(define-syntax with-u8
  (syntax-rules ()
    [(_ #u8 (1) a) a]))

(check-equal? "bytevector patterns" (with-u8 #u8 (1) #(a b)) #(a b))

(check-syntax-error? "bytevector patterns, unmatched constant"
                     '((define-syntax with-u8
                         (syntax-rules ()
                           [(_ #u8 (1) a) a]))
                       (with-u8 #u8 (3) 1))
                     "macro expansion unable to match case")

(define-syntax with-vec
  (syntax-rules ()
    [(_ #(a)) 'a]))

(check-equal? "vector pattern" (with-vec #((x y))) '(x y))

(define-syntax into-vec
  (syntax-rules ()
    [(_ a b) #(b)]))

(check-equal? "vector pattern replacement" (into-vec x y) #(y))

(check-equal? "vector quasiquoting" `#(,(list 'a)) #((a)))

(check-syntax-error?
 "invalid repetitions"
 '((define-syntax invalid-repetitions
     (syntax-rules ()
       [(_ a ...) a])))
 "missing ellipsis: pattern variable needs at least 1 levels of repetition, found 0")

(check-syntax-error?
 "invalid repetitions, 2 levels"
 '((define-syntax invalid-repetitions
     (syntax-rules ()
       [(_ (a ...) ...) (a ...)])))
 "missing ellipsis: pattern variable needs at least 2 levels of repetition, found 1")

(check-syntax-error?
 "invalid repetitions, many levels"
 '((define-syntax invalid-repetitions
     (syntax-rules ()
       [(_ (#(b (x (c ...))) ...) ...) c])))
 "missing ellipses: pattern variable needs at least 3 levels of repetition, found 0")

(check-syntax-error? "ellipsis in template cdr"
                     '((define-syntax ellipsis-tail
                         (syntax-rules ()
                           [(_ (a ...)) (a . ...)])))
                     "ellipsis cannot appear as list tail")

(check-syntax-error? "ellipsis in nested template cdr"
                     '((define-syntax ellipsis-tail
                         (syntax-rules ()
                           [(_ (a ...)) (c (b . ...))])))
                     "ellipsis cannot appear as list tail")

(check-syntax-error? "ellipsis as pattern variable"
                     '((define-syntax ellipsis-tail
                         (syntax-rules ()
                           [(_ a) (... a 1)])))
                     "ellipses are not a valid identifier in templates")

;; https://github.com/mattwparas/steel/issues/706

(define-syntax multiple-value-set!
  (syntax-rules ()
    [(_ variables values-form) (gen-temps-and-sets variables () () values-form)]))

(define-syntax gen-temps-and-sets
  (syntax-rules ()
    [(_ () (temps ...) (assignments ...) values-form)
     (emit-cwv-form (temps ...) (assignments ...) values-form)]
    [(_ (variable . more) (temps ...) (assignments ...) values-form)
     (gen-temps-and-sets more (temps ... temp) (assignments ... (set! variable temp)) values-form)]))

(define-syntax emit-cwv-form
  (syntax-rules ()
    [(_ (temps ...) (assignments ...) values-form)
     (call-with-values (lambda () values-form)
                       (lambda (temps ...)
                         assignments ...))]))

(define mvs-a 0)
(define mvs-b 0)
(define mvs-c 0)
(multiple-value-set! (mvs-a mvs-b mvs-c) (values 1 2 3))

(check-equal? "hygiene, temporaries from recursive expansion" (list mvs-a mvs-b mvs-c) '(1 2 3))

(check-equal? "hygiene, temporaries from recursive expansion, local variables"
              (let ([x 0]
                    [y 0])
                (multiple-value-set! (x y) (values 'x 'y))
                (list x y))
              '(x y))

(define-syntax gen-thunks
  (syntax-rules ()
    [(_ () (tmps ...) (vals ...)) ((lambda (tmps ...) (list (lambda () tmps) ...)) vals ...)]
    [(_ (v . vs) (tmps ...) (vals ...)) (gen-thunks vs (tmps ... tmp) (vals ... v))]))

(check-equal? "hygiene, temporaries from recursive expansion, closures"
              (map (lambda (thunk) (thunk)) (gen-thunks (1 2 3) () ()))
              '(1 2 3))

(define-syntax sum-chain
  (syntax-rules ()
    [(_ () e) e]
    [(_ (x . xs) e) (let ([t x]) (sum-chain xs (+ t e)))]))

(check-equal? "hygiene, nested bindings from recursive expansion" (sum-chain (1 2) 0) 3)

(define-syntax bind-inner
  (syntax-rules ()
    [(_ e) (let ([t 2]) e)]))

(define-syntax bind-outer
  (syntax-rules ()
    [(_) (let ([t 1]) (bind-inner t))]))

(check-equal? "hygiene, bindings from different macros" (bind-outer) 1)

(define-syntax define-temps
  (syntax-rules ()
    [(_ () (names ...)) (list names ...)]
    [(_ (v . vs) (names ...))
     (begin
       (define tmp v)
       (define-temps vs (names ... tmp)))]))

(check-equal? "hygiene, internal defines from recursive expansion"
              ((lambda () (define-temps (1 2 3) ())))
              '(1 2 3))

(define-syntax set-via-temp
  (syntax-rules ()
    [(_ var val) (bind-with (hyg-temp) (set! var hyg-temp) val)]))

(define-syntax bind-with
  (syntax-rules ()
    [(_ (x) body val) ((lambda (x) body) val)]))

(define hyg-temp 0)
(set-via-temp hyg-temp 5)

(check-equal? "hygiene, global not captured by macro binding" hyg-temp 5)

(check-equal? "hygiene, local not captured by macro temporaries"
              (let ([temp 0]
                    [b 0])
                (multiple-value-set! (temp b) (values 1 2))
                (list temp b))
              '(1 2))

(define-syntax gen-through-kernel
  (syntax-rules ()
    [(_ r () (tmps ...) (vals ...))
     ((lambda (tmps ...)
        (define-values (r) (values (list tmps ...)))
        r)
      vals ...)]
    [(_ r (v . vs) (tmps ...) (vals ...)) (gen-through-kernel r vs (tmps ... tmp) (vals ... v))]))

(check-equal? "hygiene, temporaries used inside kernel macro"
              (gen-through-kernel result (1 2 3) () ())
              '(1 2 3))

(define-syntax sum-twice
  (syntax-rules ()
    [(_ e a b)
     (let ([t e])
       (define-values (a b) (values t t))
       (+ a b))]))

(check-equal? "hygiene, template binding used inside kernel macro" (sum-twice 21 x y) 42)

;; -------------- Report ------------------

(define stats (get-test-stats))

(displayln "Passed: " (hash-ref stats 'success-count))
(displayln "Skipped compilation: " (hash-ref stats 'failed-to-compile))
(displayln "Failed: " (hash-ref stats 'failure-count))

(assert! (= 0 (hash-ref stats 'failure-count)))
