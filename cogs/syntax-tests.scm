(require-builtin steel/process)
(require-builtin steel/transducers)
(require-builtin steel/meta)

(require "tests/unit-test.scm"
         (for-syntax "tests/unit-test.scm"))

(define (check-syntax-error? name input expected . rest)
  (define (impl name input expected)
    (define error-message (~> (run! (Engine::new) input) Err->value error-object-message))
    (define message-assert
      (or (string-contains? error-message expected) `(message= ,error-message expected= ,expected)))
    (check-equal? (string-join `(,name " [compilation fails]")) message-assert #t))
  (if (eq? name 'skip) (skip-compile #f) (impl name input expected)))

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

(check-syntax-error? 'skip
                     "no-spread"
                     '((define-syntax no-spread
                         (syntax-rules ()
                           [(_ a ...) a])))
                     "syntax-rules requires only one pattern to one body")

(check-syntax-error? "bad spread"
                     '((define-syntax bad-spread
                         (syntax-rules ()
                           [(_ a ...)
                            a ...])))
                     "syntax-rules requires only one pattern to one body")

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

(let ([bound-x 'inner]) (check-equal? "hygiene, lexical capture" (lexical-capture) 3))

(define-syntax improper-rest
  (syntax-rules ()
    [(_ (a . b)) 'b]))

 (check-equal? "improper-rest, 0 args" (improper-rest (1)) '())
 (check-equal? "improper-rest, 1 arg" (improper-rest (1 2)) '(2))
 (check-equal? "improper-rest, 2 args" (improper-rest (1 a (b))) '(a (b)))

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

(skip-compile (check-equal? "improper list pattern, nested, collapses to non-list"
                            (non-list-as-list-multiple "hello" "world")
                            '("hello" "world")))

(skip-compile (define-syntax many-literals
                (syntax-rules ()
                  [(_ #t ...) 1])))

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
    [(_ #u8(1) a) a]))

(check-equal? "bytevector patterns" (with-u8 #u8(1) #(a b)) #(a b))

(check-syntax-error? "bytevector patterns, unmatched constant"
                     '((define-syntax with-u8
                         (syntax-rules ()
                           [(_ #u8(1) a) a]))
                       (with-u8 #u8(3) 1))
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

;; -------------- Report ------------------

(define stats (get-test-stats))

(displayln "Passed: " (hash-ref stats 'success-count))
(displayln "Skipped compilation: " (hash-ref stats 'failed-to-compile))
(displayln "Failed: " (hash-ref stats 'failure-count))
