(provide caar
         cadr
         cdar
         cddr
         caaar
         caadr
         cadar
         caddr
         cdaar
         cdadr
         cddar
         cdddr
         caaaar
         caadar
         caaddr
         cadaar
         cadadr
         caddar
         cadddr
         cdaaar
         cdadar
         cdaddr
         cddaar
         cddadr
         cdddar
         cdddr
         id
         flip
         curry
         curry2
         foldl
         map
         foldr
         unfold
         fold
         reduce
         max
         min
         mem-helper
         memv
         memf
         contains?
         assoc
         assq
         assv
         assf
         findf
         filter
         even-rec?
         odd-rec?
         sum
         add1
         sub1
         zero?
         drop
         slice
         flatten
         for-each
         *meta-continuation*
         *abort
         *reset
         *shift
         force
         values
         call-with-values
         #%register-struct-finalizer
         #%start-will-executor
         with-finalizer)

(require-builtin steel/base)

; (define-syntax steel/base
;   (syntax-rules ()
;     [(steel/base) (require-builtin steel/base)]))

; (define-syntax quasiquote
;   (syntax-rules (unquote unquote-splicing)
;     ((_ ((unquote x) . xs))          (cons x (quasiquote xs)))
;     ((_ ((unquote-splicing x) . xs)) (append x (quasiquote xs)))
;     ((_ (unquote x))                 x)
;     ((_ (x  . xs))                   (cons (quasiquote x) (quasiquote xs)))
;     ((_ x)                           (quote x))))

(define (#%syntax-bindings)
  (hash))
(define (#%syntax-binding-kind)
  (hash))

(define #%global-will-executor (#%prim.make-will-executor))

(define #%will-executor-running (box #f))

(define (#%start-will-executor)
  (unless (unbox #%will-executor-running)
    (#%run-will-executor)))

(define (#%run-will-executor)
  (define (loop)
    ; (stdout-simple-displayln "Running loop")
    (will-execute #%global-will-executor)
    (loop))
  (define will (spawn-native-thread loop))
  (set-box! #%will-executor-running #true))

(define (#%register-struct-finalizer value finalizer)
  ; (stdout-simple-displayln "Registering finalizer")
  (#%prim.will-register #%global-will-executor value finalizer)
  ; (stdout-simple-displayln "Done registering")
  value)

(define with-finalizer #%register-struct-finalizer)

;; Note: The syntax-bindings and binding-kind will get updated in the kernel
(define-syntax syntax
  (syntax-rules (#%syntax/raw)

    ;; HACK: This makes it so that quasisyntax is happy.
    [(syntax (#%syntax/raw x ...))
     (#%expand-syntax-case (#%syntax/raw x ...) (#%syntax-bindings) (#%syntax-binding-kind))]

    [(syntax (quote x))
     (#%expand-syntax-case (quote (quote x)) (#%syntax-bindings) (#%syntax-binding-kind))]

    ;; Otherwise, if its not quoted, just quote it
    ;; Quasisyntax isn't quite right here. We actually just want the syntax raw behavior without really any unquote?
    [(syntax x) (#%expand-syntax-case (quote x) (#%syntax-bindings) (#%syntax-binding-kind))]))

(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing #%unquote #%unquote-splicing #%quote)

    [(quasiquote ((quote x) xs ...)) (cons (list 'quote (quasiquote x)) (quasiquote (xs ...)))]

    [(quasiquote (quote x)) (list 'quote (quasiquote x))]

    [(quasiquote ((unquote x) xs ...)) (cons (list 'unquote (quasiquote x)) (quasiquote (xs ...)))]
    [(quasiquote (unquote x)) (list 'unquote (quasiquote x))]

    ; [(quasiquote ((unquote x) xs ...)) (cons x (quasiquote (xs ...)))]
    ; [(quasiquote (unquote x)) x]

    ; ((quasiquote ((#%unquote x) xs ...))          (cons x (quasiquote (xs ...))))

    [(quasiquote ((#%unquote x) xs ...)) (cons x (quasiquote (xs ...)))]
    [(quasiquote (#%unquote x)) x]

    [(quasiquote ((#%unquote-splicing x))) (append x '())]
    [(quasiquote #((#%unquote-splicing x))) (list->vector (append x '()))]
    [(quasiquote ((#%unquote-splicing x) xs ...)) (append x (quasiquote (xs ...)))]
    [(quasiquote #((#%unquote-splicing x) xs ...)) (list->vector (append x (quasiquote (xs ...))))]

    ;; TODO: Do unquote-splicing as well, follow the same rules as unquote
    [(quasiquote ((unquote-splicing x))) (append (list (list 'unquote-splicing (quasiquote x))) '())]
    [(quasiquote ((unquote-splicing x) xs ...))
     (append (list (list 'unquote-splicing (quasiquote x))) (quasiquote (xs ...)))]
    [(quasiquote (x xs ...)) (cons (quasiquote x) (quasiquote (xs ...)))]
    [(quasiquote #(x xs ...)) (list->vector (cons (quasiquote x) (quasiquote (xs ...))))]
    [(quasiquote x) 'x]))

(define-syntax #%proto-syntax-object
  (syntax-rules ()
    [(#%proto-syntax-object x) (#%syntax/raw 'x 'x (#%syntax-span x))]))

;; TODO: @Matt
;; Bootstrap this by expanding first, then expanding the resulting
;; TODO: Add syntax->span in the macro expander as a special case function.
;; That just inlines the span object '(left right <source-id>)
;; And then also calls the constructor for the
(define-syntax quasisyntax
  (syntax-rules (syntax unsyntax unsyntax-splicing #%unsyntax #%unsyntax-splicing #%internal-crunch)

    [(quasisyntax #%internal-crunch ((syntax x) xs ...))
     (cons (list 'syntax (quasisyntax #%internal-crunch x)) (quasisyntax #%internal-crunch (xs ...)))]

    ;; What the heck is going on here?
    [(quasisyntax #%internal-crunch (syntax x))
     (#%syntax/raw (quote x) (quasisyntax #%internal-crunch x) (#%syntax-span x))]

    [(quasisyntax #%internal-crunch ((unsyntax x) xs ...))
     (cons (list 'unsyntax (quasisyntax #%internal-crunch x))
           (quasisyntax #%internal-crunch (xs ...)))]
    [(quasisyntax #%internal-crunch (unsyntax x)) (list 'unsyntax (quasisyntax #%internal-crunch x))]

    [(quasisyntax #%internal-crunch ((#%unsyntax x) xs ...))
     (#%syntax/raw (quote (xs ...))
                   (cons x (syntax-e (quasisyntax #%internal-crunch (xs ...))))
                   (#%syntax-span (xs ...)))]

    [(quasisyntax #%internal-crunch (#%unsyntax x)) x]

    ; [(quasisyntax #%internal-crunch (#%unsyntax-splicing x xs ...))
    ;  (#%syntax/raw (quote x) (append (syntax-e x) '()) (#%syntax-span x))]

    ; (let ([evald x])
    ;   (begin
    ;     (stdout-simple-displayln "first case" evald)
    ;     (#%syntax/raw (quote x) evald (#%syntax-span x))))
    [(quasisyntax #%internal-crunch (#%unsyntax-splicing x)) x]

    [(quasisyntax #%internal-crunch ((#%unsyntax-splicing x)))
     (let ([evald x]) (#%syntax/raw (quote x) evald (#%syntax-span x)))]

    [(quasisyntax #%internal-crunch ((#%unsyntax-splicing x) xs ...))
     (let ([evald x])
       (#%syntax/raw (quote (xs ...))
                     (append (if (list? evald)
                                 evald
                                 (syntax-e evald))
                             (syntax-e (quasisyntax #%internal-crunch (xs ...))))
                     (#%syntax-span (xs ...))))]

    ;; TODO: Do unquote-splicing as well, follow the same rules as unquote
    [(quasisyntax #%internal-crunch ((unsyntax-splicing x)))
     (#%syntax/raw (quote '())
                   (append (list (list 'unsyntax-splicing (quasisyntax #%internal-crunch x))) '())
                   (#%syntax-span x))]

    [(quasisyntax #%internal-crunch ((unsyntax-splicing x) xs ...))
     (#%syntax/raw (quote xs ...)
                   (append (list (list 'unsyntax-splicing (quasisyntax #%internal-crunch x)))
                           (syntax-e (quasisyntax #%internal-crunch (xs ...))))
                   (#%syntax-span (xs ...)))]

    [(quasisyntax #%internal-crunch ()) (#%syntax/raw '() '() '(0 0 0))]
    [(quasisyntax #%internal-crunch (x xs ...))
     ;; TODO: Wrap this up in a syntax/raw?
     (#%syntax/raw (quote (x xs ...))
                   (cons (quasisyntax #%internal-crunch x)
                         (syntax-e (quasisyntax #%internal-crunch (xs ...))))
                   (#%syntax-span (x xs ...)))]

    ;; Internal, we don't do anything special
    [(quasisyntax #%internal-crunch x)
     (if (empty? 'x)
         (#%syntax/raw '() '() (#%syntax-span x))
         (#%syntax/raw 'x 'x (#%syntax-span x)))]

    ;; This is the absolute worst!
    [(quasisyntax (x))
     (syntax (#%syntax/raw (quote (x)) (quasisyntax #%internal-crunch x) (#%syntax-span (x xs ...))))]

    [(quasisyntax (x xs ...))
     (syntax (#%syntax/raw (quote (x xs ...))
                           (cons (quasisyntax #%internal-crunch x)
                                 (syntax-e (quasisyntax #%internal-crunch (xs ...))))
                           (#%syntax-span (x xs ...))))]

    ;; If x is something like (lambda (x) ...)
    ;; it won't match properly, because the lambda is not getting
    ;; destructed into a list.
    [(quasisyntax x)
     (if (empty? 'x)
         (#%syntax/raw '() '() (#%syntax-span x))
         (syntax (#%syntax/raw 'x 'x (#%syntax-span x))))]))

;;@doc
;; Syntax:
;; If no exprs are provided, then the result is `#false`
;;
;; If a single expr is provided, then it is in tail position, so the results
;; of the `or` expressions are the results of the `expr`.
;;
;; Otherwise, the first `expr` is evaluated. If it produces a value other
;; than `#f`, that result is the result of the `or` expression. Otherwise,
;; the result is the same as an `or` expression witih
;; the remaining `expr`s in tail position with respect to the original
;; `or` form.
;;
;; # Examples
;; ```scheme
;; (or) ;; => #f
;; (or 1) ;; => `
;; (or 5 (error "should not get here")) ;; => 5
;; (or #f 5) ;; => 5
;; ```
(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or x) x]
    [(or x y) (let ([z x]) (if z z y))]
    [(or x y ...) (or x (or y ...))]))

;;@doc
;; Syntax:
;; If no `expr`s are provided, the the result is #t.
;;
;; If a single `expr` is provided, then it is in tail position, so the results of
;; the `and` expression are the results of the `expr`.
;;
;; Otherwise, the first `expr` is evaluated. If it produces `#f`, the result
;; of the `and` expression is `#f`. Otherwise, the result is the same as an
;; `and` expression with the remaining `expr`s in tail position with
;; respect to the original `and` form.
;;
;; # Examples
;; ```scheme
;; (and) ;; => #t
;; (and 1) ;; => 1
;; (and #f (error "should not get here")) ;; => #f
;; (and #t 5) ;; => 5
;; ```
(define-syntax and
  (syntax-rules ()
    [(and) #t]
    [(and x) x]
    [(and x y) (if x y #f)]
    [(and x y ...) (and x (and y ...))]))

;;@doc
;; Syntax:
;;
;; ```scheme
;; (when test-expr body ...)
;; ```
;;
;; Evaluates `test-expr`. If the result is `#f`, then the result of the `when`
;; expression is `#<void>`. Otherwise, the `body`s are evaluated, and the
;; last `body` is in tail position with respect to the `when` form.
;;
;; # Examples
;; ```scheme
;; (when (positive? -f)
;;     "found positive") ;; => #<void>
;;
;; (when (positive? 5)
;;      10
;;      20) ;; => 20
;; ```
(define-syntax when
  (syntax-rules ()
    [(when a
       b ...)
     (if a
         (begin
           b ...)
         void)]))

;;@doc
;; Syntax:
;;
;; Equivalent to:
;; ```scheme
;; (when (not test-expr) body ...)
;; ```
(define-syntax unless
  (syntax-rules ()
    [(unless a
       b ...)
     (if a
         void
         (begin
           b ...))]))

(define-syntax cond
  (syntax-rules (else =>)
    [(cond
       [else
        =>
        e1 ...])
     (begin
       e1 ...)]
    [(cond
       [else
        e1 ...])
     (begin
       e1 ...)]
    [(cond
       [e1
        e2 ...])
     (when e1
       e2 ...)]
    [(cond
       [e1
        =>
        e2 ...]
       c1 ...)
     (let ([res e1]
           [function (begin
                       e2 ...)])
       (if res
           (function res)
           (cond
             c1 ...)))]
    [(cond
       [e1
        e2 ...]
       c1 ...)
     (if e1
         (begin
           e2 ...)
         (cond
           c1 ...))]))

;;@doc
;; Syntax:
;;
;; ```scheme
;; (case val-expr case-clause ...)
;;
;;   case-clause = [(datum ...) then-body ...]
;;               | [else then-body ...]
;; ```
;;
;; Evaluates `val-expr` and uses the result to select a `case-clause`. The selected
;; clause is the first one with a `datum` whose `quote`d form is `equal?` to the
;; result of `val-expr`. If no such `datum` is present, the `else` case-clause is
;; selected. If no `else` case-clause is present, then the result of the `case`
;; form is `#<void>`.
;;
;; For the selected `case-clause`, the results of the last `then-body`, which is
;; in tail position with respect to the `case` form, are the results for the whole
;; `case` form.
;;
;; A `case-clause` that starts with `else` must be the last case-clause.
;;
;; # Examples
;; ```scheme
;; (case (+ 7 5)
;;    [(1 2 3) 'small]
;;    [(10 11 12) 'big]) ;; => 'big
;;
;; (case (- 7 5)
;;    [(1 2 3) 'small]
;;    [(10 11 12) 'big]) ;; => 'small
;; ```
(define-syntax case
  (syntax-rules (else)
    [(case (key ...)
       clauses ...)
     (let ([atom-key (key ...)])
       (case atom-key
         clauses ...))]
    [(case key
       [else
        result1
        result2 ...])
     (begin
       result1
       result2 ...)]

    [(case key
       [(atoms)
        result1
        result2 ...])
     (when (equal? key (quote atoms))
       (begin
         result1
         result2 ...))]

    [(case key
       [(atoms ...)
        result1
        result2 ...])
     (when (list-contains key '(atoms ...))
       (begin
         result1
         result2 ...))]

    ; [(case key
    ;        ((atoms ...) result1 ...)
    ;        clause clauses ...)
    ;      (if (member key '(atoms ...))
    ;          (begin result1 ...)
    ;          (case key clause clauses ...))]

    [(case key
       [(atoms)
        result1
        result2 ...]
       clause
       clauses ...)
     (if (equal? key (quote atoms))
         (begin
           result1
           result2 ...)
         (case key
           clause
           clauses ...))]

    [(case key
       [(atoms ...)
        result1
        result2 ...]
       clause
       clauses ...)
     (if (list-contains key '(atoms ...))
         (begin
           result1
           result2 ...)
         (case key
           clause
           clauses ...))]))
;;@doc
;; Syntax:
;;
;; ```scheme
;; (while test body ...)
;; ```
;;
;; A while loop. Each iteration of the loop evaluates the test
;; expression, and if it evaluates to a true value, the
;; body expressions are evaluates sequentially.
;;
;; ```scheme
;; (while #t (displayln "hello world"))
;; ```
(define-syntax while
  (syntax-rules (do)
    [(while cond do body ...)
     (begin
       (define (loop)
         (when cond
           body ...
           (loop)))
       (loop))]
    [(while cond body ...)
     (begin
       (define (loop)
         (when cond
           body ...
           (loop)))
       (loop))]))

(define-syntax f>
  (syntax-rules ()
    [(f> fun) fun]
    [(f> fun args* ...) (lambda (x) (fun x args* ...))]))

;;@doc
;; Syntax:
;; Alias for `~>`. Prefer to use `~>` over `->`.
(define-syntax ->
  (syntax-rules ()
    [(-> a) a]
    [(-> a (b c ...)) ((f> b c ...) a)]
    [(-> a (b)) ((f> b) a)]
    [(~> a b) ((f> b) a)]
    [(-> a b c ...) (-> (-> a b) c ...)]))

;;@doc
;; Syntax:
;;
;; This can be read as "thread-first". It is used to pipe expressions
;; through to the first argument of the next expression in order to avoid
;; nesting.
;;
;; # Examples
;; ```scheme
;; (~> 10) ;; => 10
;; (~> 10 list) ;; equivalent to (list 10)
;; (~> 10 list car) ;; equivalent to (car (list 10))
;; (~> 10 list ((lambda (m) (map add1 m)))) ;; => '(11)
;; ```
(define-syntax ~>
  (syntax-rules ()
    [(~> a) a]
    [(~> a (b c ...)) ((f> b c ...) a)]
    [(~> a (b)) ((f> b) a)]
    [(~> a b) ((f> b) a)]
    [(~> a b c ...) (~> (~> a b) c ...)]))

(define-syntax l>
  (syntax-rules ()
    [(l> fun) fun]
    [(l> fun args* ...) (lambda (x) (fun args* ... x))]))

;;@doc
;; Syntax:
;;
;; This can be read as "thread-last". It is used to pipe expressions
;; through to the last argument of the next expression in order to avoid
;; nesting.
;;
;; # Examples
;; ```scheme
;; (~>> 10) ;; => 10
;; (~>> 10 list) ;; equivalent to (list 10)
;; (~>> 10 list car) ;; equivalent to (car (list 10))
;; (~>> 10 list (map add1)) ;; => '(11)
;; ```
(define-syntax ~>>
  (syntax-rules ()
    [(~>> a) a]
    [(~>> a (b c ...)) ((l> b c ...) a)]
    [(~>> a (b)) ((l> b) a)]
    [(~>> a b) ((l> b) a)]
    [(~>> a b c ...) (~>> (~>> a b) c ...)]))

;;@doc
;; Syntax:
;; Alias for `~>>`. Prefer to use `~>>` over `->>`.
(define-syntax ->>
  (syntax-rules ()
    [(->> a) a]
    [(->> a (b c ...)) ((l> b c ...) a)]
    [(->> a (b)) ((l> b) a)]
    [(~>> a b) ((l> b) a)]
    [(->> a b c ...) (->> (->> a b) c ...)]))

;;@doc
;; Syntax:
;;
;; Swap the values for the given identifiers
;;
;; ```scheme
;; (define a 10)
;; (define b 20)
;; (swap a b)
;; a ;; => 20
;; b ;; => 10
;; ```
(define-syntax swap
  (syntax-rules ()
    [(swap a b)
     (let ([tmp b])
       (begin
         (set! b a)
         (set! a tmp)))]))

;;@doc
;; Syntax:
;;
;; ```scheme
;; (let* ([id val-expr] ...) body ...)
;; ```
;;
;; Like `let`, but evaluates the `val-expr`s one by one.
;; Each id is bound in the remaining `val-expr` as well
;; as the `body`s. The `id`s do not need to be distinct;
;; later bindings will shadow earlier bindings.
;;
;; # Examples
;; ```scheme
;; (let* ([x 1]
;;        [y (+ x 1)])
;;     (list y x)) ;; => '(2 1)
;; ```
(define-syntax let*
  (syntax-rules ()
    [(let* ()
       body ...) ; base case
     ((lambda ()
        body ...))]
    [(let* ([var val]
            rest ...)
       body ...) ; binding case
     ((lambda (var)
        (let* (rest ...)
          body ...))
      val)]))

(define-syntax #%letrec*-helper
  (syntax-rules ()
    [(#%letrec*-helper () body ...)
     (begin
       body ...)]
    [(#%letrec*-helper ((var val) rest ...) body ...)
     (begin
       (define var val)
       (#%letrec*-helper (rest ...) body ...))]))

;;@doc
;;
;; Syntax:
;;
;; Alias for `letrec`.
(define-syntax letrec*
  (syntax-rules ()
    [(letrec* bindings body ...) ((lambda () (#%letrec*-helper bindings body ...)))]))

;;@doc
;; Syntax:
;;
;; ```scheme
;; (letrec ([id val-expr] ...) body ...)
;; ```
;;
;; Let `let`, but the identifiers are created first, meaning
;; `id`s within `val-expr`s can reference later `id`s in the
;; letrec.
;;
;; # Examples
;; ```scheme
;; (letrec ([is-even? (lambda (n)
;;                       (or (zero? n)
;;                           (is-odd? (sub1 n))))]
;;           [is-odd? (lambda (n)
;;                      (and (not (zero? n))
;;                           (is-even? (sub1 n))))])
;;    (is-odd? 11)) ;; => #t
;; ```
(define-syntax letrec
  (syntax-rules ()
    [(letrec bindings
       body ...)
     ((lambda () (#%letrec*-helper bindings body ...)))]))

(define-syntax module
  (syntax-rules (provide gen-defines
                         contract/out)
    [(module name (provide ids ...)
       funcs ...)
     (begin
       (define (datum->syntax name)
         ((lambda ()
            funcs ...
            (module provide ids
              ...))))
       (module gen-defines name
         ids ...))]

    ;; in the contract case, ignore the contract in the hash
    [(module provide (contract/out name contract)
       )
     (%proto-hash% 'name name)]
    ;; Normal case
    [(module provide name
       )
     (%proto-hash% 'name name)]

    ;; in the contract case, ignore the contract in the hash
    [(module provide (contract/out name contract)
       rest ...)
     (%proto-hash-insert% (module provide rest
                            ...)
                          'name
                          name)]

    ;; Normal case
    [(module provide name
       rest ...)
     (%proto-hash-insert% (module provide rest
                            ...)
                          'name
                          name)]

    ;; Module contract provides
    [(module gen-defines mod
       (contract/out name contract))
     (define (datum->syntax name)
       (bind/c contract (%proto-hash-get% mod 'name)))]
    [(module gen-defines mod
       (contract/out name contract)
       rest ...)
     (begin
       (define (datum->syntax name)
         (bind/c contract (%proto-hash-get% mod 'name)))
       (module gen-defines mod
         rest ...))]

    ;; Normal provides
    [(module gen-defines mod
       name)
     (define (datum->syntax name)
       (%proto-hash-get% mod 'name))]
    [(module gen-defines mod
       name
       rest ...)
     (begin
       (define (datum->syntax name)
         (%proto-hash-get% mod 'name))
       (module gen-defines mod
         rest ...))]))

(define-syntax do
  (syntax-rules ()
    [(do ((var init step ...) ...) (test expr ...) command ...)
     (letrec* ([loop
                (lambda (var ...)
                  (if test
                      (begin
                        expr ...)
                      (begin
                        command ...
                        (loop (do "step" var step ...) ...))))])
              (loop init ...))]
    [(do "step" x) x]
    [(do "step" x y) y]))

;; TODO: Replace some of these with just list ref to abuse the underlying implementation
(define caar (lambda (pair) (car (car pair))))
(define cadr (lambda (pair) (car (cdr pair))))
(define cdar (lambda (pair) (cdr (car pair))))
(define cddr (lambda (pair) (cdr (cdr pair))))
(define caaar (lambda (pair) (car (car (car pair)))))
(define caadr (lambda (pair) (car (car (cdr pair)))))
(define cadar (lambda (pair) (car (cdr (car pair)))))
(define caddr (lambda (pair) (car (cdr (cdr pair)))))
(define cdaar (lambda (pair) (cdr (car (car pair)))))
(define cdadr (lambda (pair) (cdr (car (cdr pair)))))
(define cddar (lambda (pair) (cdr (cdr (car pair)))))
(define cdddr (lambda (pair) (cdr (cdr (cdr pair)))))
(define caaaar (lambda (pair) (car (car (car (car pair))))))
(define caaadr (lambda (pair) (car (car (car (cdr pair))))))
(define caadar (lambda (pair) (car (car (cdr (car pair))))))
(define caaddr (lambda (pair) (car (car (cdr (cdr pair))))))
(define cadaar (lambda (pair) (car (cdr (car (car pair))))))
(define cadadr (lambda (pair) (car (cdr (car (cdr pair))))))
(define caddar (lambda (pair) (car (cdr (cdr (car pair))))))
(define cadddr (lambda (pair) (car (cdr (cdr (cdr pair))))))
(define cdaaar (lambda (pair) (cdr (car (car (car pair))))))
(define cdaadr (lambda (pair) (cdr (car (car (cdr pair))))))
(define cdadar (lambda (pair) (cdr (car (cdr (car pair))))))
(define cdaddr (lambda (pair) (cdr (car (cdr (cdr pair))))))
(define cddaar (lambda (pair) (cdr (cdr (car (car pair))))))
(define cddadr (lambda (pair) (cdr (cdr (car (cdr pair))))))
(define cdddar (lambda (pair) (cdr (cdr (cdr (car pair))))))
(define cddddr (lambda (pair) (cdr (cdr (cdr (cdr pair))))))
(define id (lambda (obj) obj))
(define flip (lambda (func) (lambda (arg1 arg2) (func arg2 arg1))))
(define curry (lambda (func arg1) (lambda (arg) (func arg1 arg))))
(define curry2 (lambda (func arg1) (lambda (arg2 arg3) (func arg1 arg2 arg3))))

;;@doc
;; Applies `func` to the elements of the `lsts` from the first
;; elements to the last. The `func` argument must accept the same
;; number of arguments as the number of supplied `lsts`, and all
;; `lsts` must have the same number of elements. The result is a list
;; containing each result of `func` in order.
;;
;; (map func lst . lsts) -> list?
;;
;; # Examples
;; ```scheme
;; (map add1 (range 0 5)) ;; '(1 2 3 4 5)
;; ```
(define (map function list1 . more-lists)
  (define (some? function list)
    (and (pair? list) (or (function (car list)) (some? function (cdr list)))))

  (define (map1 func accum lst)
    (if (null? lst)
        (reverse accum)
        (map1 func (cons (func (car lst)) accum) (cdr lst))))

  (define (map-many func accum lsts)
    (if (some? null? lsts)
        (reverse accum)
        (map-many func (cons (apply func (map1 car '() lsts)) accum) (map1 cdr '() lsts))))

  (if (null? more-lists)
      (map1 function '() list1)
      (let ([lists (cons list1 more-lists)])
        (if (some? null? lists)
            '()
            (map-many function '() lists)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func (car lst) accum) (cdr lst))))

(define (foldr func accum lst)
  (if (null? lst)
      accum
      (func (car lst) (foldr func accum (cdr lst)))))

(define (unfold func init stop?)
  (if (stop? init)
      (cons init '())
      (cons init (unfold func (func init) stop?))))

(define fold foldl)
(define reduce fold)

(define (max x . rest)
  (fold (lambda (y z) (if (> y z) y z)) x rest))

(define (min x . rest)
  (fold (lambda (y z) (if (< y z) y z)) x rest))

(define mem-helper
  (lambda (pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc))))

;;@doc
;; Return the first tail of the list, where the car is `eqv?` to the given obj.
;; Returns `#f`, if no element is found.
;;
;; This procedure is equivalent to `member`, but using `eqv?` instead of `equal?`.
;;
;; (memv obj lst) -> (or/c list? #f)
;;
;; * obj : any/c
;; * lst : list?
;;
;; ```scheme
;; (memv #\c '(#\a #\b #\c #\d #\e)) ;; => '(#\c #\d #\e)
;; (memv 5 '(0 1 2 3 4)) ;; => #f
;; ```
(define (memv obj lst)
  (cond
    [(null? lst) #f]
    [(eqv? obj (car lst)) lst]
    [else (memv obj (cdr lst))]))

;;@doc
;; Return the first tail of the list, where the given proc returns a true value,
;; when applied to the car. Returns `#f`, if no element is found.
;;
;; This procedure is equivalent to `member`, but using the given procedure
;; instead of `equal?`.
;;
;; (memf proc lst) -> (or/c list? #f)
;;
;; * proc : procedure?
;; * lst : list?
;;
;; # Examples
;;
;; ```scheme
;; (memf odd? '(0 2 1 3 4)) ;; => '(1 3 4)
;; (memf (λ (x) (char-ci=? #\D x)) '(#\a #\b #\c #\d #\e)) ;; => '(#\d #\e)
;; (memf (λ (x) (> x 5)) '(0 2 1 3 4)) ;; => #f
;; ```
(define (memf proc lst)
  (cond
    [(null? lst) #f]
    [(proc (car lst)) lst]
    [else (memf proc (cdr lst))]))

(define (contains? pred? lst)
  (cond
    [(empty? lst) #f]
    [(pred? (car lst)) #t]
    [else (contains? pred? (cdr lst))]))

;;@doc
;; Returns the first pair in the given list, where the car element is `equal?`
;; to the given obj, returning `#f` if nothing was found.
;;
;; It is an error if the given list is not a list of pairs.
;;
;; (assoc obj lst) -> (or/c pair? #f)
;;
;; * obj : any/c
;; * lst : (listof pair?)
;;
;; # Examples
;;
;; ```scheme
;; (assoc 2 '((1 1) (2 4) (3 9))) ;; => '(2 4)
;; (assoc 'b '((a 1) (b 2) (c 3))) ;; => '(b 2)
;; (assoc #\B '((#\a 1) (#\b 2) (#\c 3))) ;; => #f
;; ```
(define (assoc obj lst)
  (cond
    [(null? lst) #f]
    [(equal? (car (car lst)) obj) (car lst)]
    [else (assoc obj (cdr lst))]))

;;@doc
;; Returns the first pair in the given list, where the car element is `eq?`
;; to the given obj, returning `#f` if nothing was found.
;;
;; This procedure is equivalent to `assoc`, but using `eq?` instead of `equal?`.
;;
;; It is an error if the given list is not a list of pairs.
;;
;; (assq obj lst) -> (or/c pair? #f)
;;
;; * obj : any/c
;; * lst : (listof pair?)
;;
;; # Examples
;;
;; ```scheme
;; (assq 2 '((1 1) (2 4) (3 9))) ;; => '(2 4)
;; (assq 'b '((a 1) (b 2) (c 3))) ;; => '(b 2)
;; (assq #\B '((#\a 1) (#\b 2) (#\c 3))) ;; => #f
;; ```
(define (assq obj lst)
  (cond
    [(null? lst) #f]
    [(eq? (car (car lst)) obj) (car lst)]
    [else (assq obj (cdr lst))]))

;;@doc
;; Returns the first pair in the given list, where the car element is `eqv?`
;; to the given obj, returning `#f` if nothing was found.
;;
;; This procedure is equivalent to `assoc`, but using `eqv?` instead of `equal?`.
;;
;; It is an error if the given list is not a list of pairs.
;;
;; (assv obj lst) -> (or/c pair? #f)
;;
;; * obj : any/c
;; * lst : (listof pair?)
;;
;; # Examples
;;
;; ```scheme
;; (assv 2 '((1 1) (2 4) (3 9))) ;; => '(2 4)
;; (assv 'b '((a 1) (b 2) (c 3))) ;; => '(b 2)
;; (assv #\B '((#\a 1) (#\b 2) (#\c 3))) ;; => #f
;; ```
(define (assv obj lst)
  (cond
    [(null? lst) #f]
    [(eqv? (car (car lst)) obj) (car lst)]
    [else (assv obj (cdr lst))]))

;;@doc
;; Returns the first pair in the given list, where the given proc returns a true
;; value, when applied to the car element. returning `#f`, if no element is found.
;;
;; This procedure is equivalent to `assoc`, but using using the given procedure
;; instead of `equal?`.
;;
;; It is an error if the given list is not a list of pairs.
;;
;; (assf proc lst) -> (or/c pair? #f)
;;
;; * proc : procedure?
;; * lst : (listof pair?)
;;
;; # Examples
;;
;; ```scheme
;; (assf odd? '((0 a) (2 b) (1 c))) ;; => '(1 c)
;; (assf (λ (x) (char-ci=? #\B x)) '((#\a 1) (#\b 2) (#\c 3))) ;; => '(#\b 2)
;; (assf (λ (x) (> x 5)) '((1 1) (2 4) (3 9))) ;; => #f
;; ```
(define (assf proc lst)
  (cond
    [(null? lst) #f]
    [(proc (car (car lst))) (car lst)]
    [else (assf proc (cdr lst))]))

;;@doc
;; Returns the first element of the list, where the given proc returns a true
;; value, when applied to it. Returns `#f`, if no element is found.
;;
;; If `#f` is an element of *lst*, a return value of `#f` is ambiguous: it
;; might indicate that no element satisfies *proc* or it may indicate, that
;; `#f` satisfies *proc*.
;;
;; (findf proc lst) -> (or/c any/c #f)
;;
;; - proc : procedure?
;; - lst: list?
;;
;; # Examples
;;
;; ```scheme
;; (findf odd? '(0 2 1 3 4)) ;; => 1
;; (findf (λ (x) (char-ci=? #\D x)) '(#\a #\b #\c #\d #\e)) ;; => #\d
;; (findf (λ (x) (> x 5)) '(0 2 1 3 4)) ;; => #f
;; ```
(define (findf proc lst)
  (cond
    [(null? lst) #f]
    [(proc (car lst)) (car lst)]
    [else (findf proc (cdr lst))]))

;;@doc
;; Returns new list, keeping elements from `lst` which applying `pred` to the element
;; returns #t.
;;
;;
;; # Examples
;;
;; ```scheme
;; (filter even? (range 0 5)) ;; '(0 2 4)
;; ```
(define (filter function lst)
  (define (filter-inner function accum lst)
    (if (null? lst)
        (reverse accum)
        (let ([next (car lst)])
          (if (function next)
              (filter-inner function (cons next accum) (cdr lst))
              (filter-inner function accum (cdr lst))))))

  (filter-inner function '() lst))

(define even-rec?
  (lambda (x)
    (if (= x 0)
        #t
        (odd-rec? (- x 1)))))
(define odd-rec?
  (lambda (x)
    (if (= x 0)
        #f
        (even-rec? (- x 1)))))

(define sum (lambda (x) (reduce + 0 x)))

(define (add1 n)
  (+ 1 n))
(define (sub1 n)
  (- n 1))
(define (zero? n)
  (= n 0))

;;@doc
;; Returns the list l after the first n elements.
;;
;; (drop l n) -> list?
;;
;; * l : list?
;; * n : (and/c positive? int?)
;;
;; # Examples
;;
;; ```scheme
;; > (drop '(1 2 3 4) 2) ;; => '(3 4)
;; > (drop (range 0 10) 6) ;; => '(6 7 8 9)
;; ```
(define (drop lst n)
  (define (loop lst n)
    (if (zero? n)
        lst
        (loop (cdr lst) (sub1 n))))
  (if (< n 0)
      (error 'drop "expects a positive number")
      (loop lst n)))

(define (slice l offset n)
  (take (drop l offset) n))

;;@doc
;; Recursively flatten an arbitray structure of pairs into a single list.
;;
;; (flatten any/c) -> list?
;;
;; # Examples
;;
;; ```scheme
;; (flatten '(a (b (c . d)) e ())) ;; => '(a b c d e)
;; (flatten 'a) => '(a)
;; ```
(define (flatten lst)
  (cond
    [(null? lst) '()]
    [(pair? lst) (append (flatten (car lst)) (flatten (cdr lst)))]
    [else (list lst)]))

(define (gcd a b)
  (cond
    [(= b 0) (abs a)]
    [else (gcd b (modulo a b))]))

(define (lcm a b)
  (if (or (zero? a) (zero? b))
      0
      (abs (* b (floor (/ a (gcd a b)))))))

;;@doc
;; Applies a procedure to all elements of a list
;;
;; (for-each procedure? list?) ;; => void?
;;
;; # Examples
;;
;; ```scheme
;; > (for-each (λ (x) (println x)) '(a b c))
;; 'a
;; 'b
;; 'c
;; ```
(define (for-each func lst)
  (define (#%for-each func2 lst2)
    (unless (null? lst2)
      (func (car lst2))
      (#%for-each func2 (cdr lst2))))
  (if (function? func)
      (if (list? lst)
          (#%for-each func lst)
          (error-with-span (current-function-span) "for-each expected a list, found: " lst))
      (error-with-span (current-function-span) "for-each expected a function, found: " func)))

;; TODO: Just make this a built in!
(define (vector->list v . remaining)
  (cond
    [(immutable-vector? v) (apply immutable-vector->list (cons v remaining))]
    [(mutable-vector? v) (apply mutable-vector->list (cons v remaining))]
    [else (error "vector->list expects a vector, found: " v)]))

;;; Macros go here:

(define-syntax reset
  (syntax-rules ()
    [(reset ?e) (*reset (lambda () ?e))]))

(define-syntax shift
  (syntax-rules ()
    [(shift ?k ?e) (*shift (lambda (?k) ?e))]))

;; TODO: This should be boxed at some point, we don't want it
;; to be globally accessible directly (I think)
; (define (*meta-continuation* v)
;   (error "You forgot the top-level reset..."))

;; TODO: This doesn't just need to be TLS - it also
;; needs to be captured by the children below. A global
;; reference unforuntately isn't enough - it needs to be
;; a copy of the underlying value.
(define *meta-continuation* (make-tls (lambda (_) (error "You forgot the top-level reset..."))))

(define (*abort thunk)
  (let ([v (thunk)]) ((get-tls *meta-continuation*) v)))

(define (*reset thunk)
  ; (let ([mc *meta-continuation*])
  (let ([mc (get-tls *meta-continuation*)])
    (call/cc (lambda (k)
               (begin
                 ; (set! *meta-continuation*
                 ;       (lambda (v)
                 ;         (set! *meta-continuation* mc)
                 ;         (k v)))

                 (set-tls! *meta-continuation*
                           (lambda (v)
                             (set-tls! *meta-continuation* mc)
                             (k v)))

                 (*abort thunk))))))

(define (*shift f)
  (call/cc (lambda (k) (*abort (lambda () (f (lambda (v) (reset (k v)))))))))

(define-syntax with-handler
  (syntax-rules ()
    [(with-handler handler expr)
     (reset (call-with-exception-handler (lambda (err)
                                           (define res (handler err))
                                           ; (shift k (k void))

                                           ; (shift *meta-continuation* (*meta-continuation* void)))
                                           (shift *meta-continuation*
                                                  ; ((get-tls *meta-continuation*) void)))
                                                  (*meta-continuation* res)))
                                         (lambda () expr)))]
    [(with-handler handler expr ...)
     (reset (call-with-exception-handler (lambda (err)
                                           (define res (handler err))
                                           ; (shift k (k void))
                                           ; (shift *meta-continuation* (*meta-continuation* void)))
                                           (shift *meta-continuation*
                                                  ; ((get-tls *meta-continuation*) void)))
                                                  (*meta-continuation* res)))
                                         (lambda ()
                                           expr ...)))]))

(define-syntax case-lambda
  (syntax-rules ()
    [(case-lambda) (lambda args (error "CASE-LAMBDA without any clauses."))]
    [(case-lambda
       [?a1
        ?e1 ...]
       ?clause1 ...)
     (lambda args
       (let ([l (length args)])
         (case-lambda
           "CLAUSE"
           args
           l
           [?a1
            ?e1 ...]
           ?clause1 ...)))]
    [(case-lambda
       "CLAUSE"
       ?args
       ?l
       [(?a1 ...)
        ?e1 ...])
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...)
                  ?e1 ...)
                ?args)
         (error! "Arity mismatch in case lambda: attempted to call function with arg length: "
                 ?l
                 "expecting length: "
                 (length '(?a1 ...))))]
    [(case-lambda
       "CLAUSE"
       ?args
       ?l
       [(?a1 ...)
        ?e1 ...]
       ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...)
                  ?e1 ...)
                ?args)
         (case-lambda
           "CLAUSE"
           ?args
           ?l
           ?clause1 ...))]
    [(case-lambda
       "CLAUSE"
       ?args
       ?l
       [(?a1 . ?ar)
        ?e1 ...]
       ?clause1 ...)
     (case-lambda
       "IMPROPER"
       ?args
       ?l
       1
       [?a1
        . ?ar]
       [?ar
        ?e1 ...]
       ?clause1 ...)]
    [(case-lambda
       "CLAUSE"
       ?args
       ?l
       [?a1
        ?e1 ...])
     (let ([?a1 ?args])
       ?e1 ...)]
    [(case-lambda
       "CLAUSE"
       ?args
       ?l)
     (error "Wrong number of arguments to CASE-LAMBDA.")]
    [(case-lambda
       "IMPROPER"
       ?args
       ?l
       ?k
       ?al
       [(?a1 . ?ar)
        ?e1 ...]
       ?clause1 ...)
     (case-lambda
       "IMPROPER"
       ?args
       ?l
       [+
        ?k
        1]
       ?al
       [?ar
        ?e1 ...]
       ?clause1 ...)]
    [(case-lambda
       "IMPROPER"
       ?args
       ?l
       ?k
       ?al
       [?ar
        ?e1 ...]
       ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al
                  ?e1 ...)
                ?args)
         (case-lambda
           "CLAUSE"
           ?args
           ?l
           ?clause1 ...))]))

(define-syntax help
  (syntax-rules ()
    [(help) (simple-displayln "help expects an identifier to lookup documentation for")]
    [(help ident) (#%private-help ident)]
    [(help module ident) (%doc? (datum->syntax %-builtin-module- module) (quote ident))]))

(define (#%private-help func)
  (unless (#%native-fn-ptr-doc func)
    (let ([doc (#%function-ptr-table-get #%function-ptr-table func)])
      (when doc
        (%string->render-markdown doc)))))

(define-syntax dbg!
  (syntax-rules ()
    [(dbg! expr)
     (let ([result expr])
       (simple-display (quote expr))
       (simple-display " = ")
       (simple-displayln result)
       result)]))

(define-syntax contract/out
  (syntax-rules ()
    [(contract/out name contract) (%require-ident-spec name (bind/c contract name 'name))]))

(define (force promise)
  (promise))

;; syntax
(define-syntax delay
  (syntax-rules ()
    [(delay
       expr)
     (lambda () expr)]))

(define values list)

(define (call-with-values producer consumer)
  (define result (producer))
  (cond
    [(not (list? result)) (consumer result)]
    ;; Does this work?
    [else
     (define res (apply consumer result))
     (if (and (list? res) (= (length res) 1))
         (car res)
         res)]))

(define-syntax @doc
  (syntax-rules (#%macro struct define/contract)

    [(_ documentation #%macro name)
     (define (datum->syntax name __doc__)
       documentation)]

    [(_ documentation
        (define (name . args)
          body ...))

     (begin
       (define (datum->syntax name __doc__)
         documentation)
       (define (name . args)
         body ...)
       (#%function-ptr-table-add #%function-ptr-table name (datum->syntax name __doc__)))]

    [(_ documentation
        (define (name args ...)
          body ...))

     (begin
       ; (stdout-simple-displayln "Expanding doc top case: " (quote (define name body)))
       (define (datum->syntax name __doc__)
         documentation)
       (define (name args ...)
         body ...)
       (#%function-ptr-table-add #%function-ptr-table name (datum->syntax name __doc__)))]

    [(_ documentation
        (define (name args ... . rest)
          body ...))

     (begin
       (define (datum->syntax name __doc__)
         documentation)
       (define (name args ... . rest)
         body ...)
       (#%function-ptr-table-add #%function-ptr-table name (datum->syntax name __doc__)))]

    [(_ documentation (define name body))

     (begin
       (define (datum->syntax name __doc__)
         documentation)
       (define name body)
       (#%function-ptr-table-add #%function-ptr-table name (datum->syntax name __doc__)))]

    [(_ documentation (struct name (args ...) options ...))

     (begin

       (define (datum->syntax name __doc__)
         documentation)

       (struct name (args ...) options ...)

       (#%function-ptr-table-add #%function-ptr-table name (datum->syntax name __doc__)))]

    [(_ documentation
        (define/contract (name args ...)
          body ...))

     (begin
       (define (datum->syntax name __doc__)
         documentation)

       (define/contract (name args ...)
         body ...)

       (#%function-ptr-table-add #%function-ptr-table name (datum->syntax name __doc__)))]

    ; [(_ documentation
    ;     (begin
    ;       ; (#%black-box "STRUCT" (quote struct-name))
    ;       exprs ...))

    ;  (begin
    ;    (stdout-simple-displayln "Matching on begin")
    ;    exprs ...)]

    ;  (begin
    ;    (begin
    ;      (define (datum->syntax struct-name __doc__)
    ;        documentation)
    ;      exprs ...)
    ;    (#%function-ptr-table-add #%function-ptr-table struct-name (datum->syntax name __doc__)))]

    [(_ documentation expr)

     (begin
       expr)]))

(define-syntax #%require-dylib
  (syntax-rules (prefix-in only-in)
    [(_ dylib-name (prefix-in prefix (only-in name ...)))
     (begin
       (define (datum->syntax prefix name)
         (%module-get% (#%get-dylib dylib-name) (quote name))) ...)]

    [(_ dylib-name (only-in name ...))
     (begin
       (define name (%module-get% (#%get-dylib dylib-name) (quote name))) ...)]))
