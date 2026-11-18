;; #%private/steel/passes: the registry of compiler passes written in Steel
;; (STEEL_PASSES_IN_STEEL_PLAN.md). It lives in an engine of its own, so
;; passes don't see or change the program they compile.
;;
;; A pass is `(lambda (exprs context) ...)`: the top level forms as syntax
;; objects in, the forms to compile out. context is a hash; 'point says where
;; in compilation it runs.
;;
;; The program is core forms only: (define name value), (lambda (arg ...)
;; body), (let ([name value] ...) body), (if test then else), (begin form ...),
;; (set! name value), (quote datum), (return! value), applications, literals,
;; identifiers and vectors (constants). Forms passes can't look into
;; (require, macros) are the symbol #%opaque-ast.

(require-builtin #%private/steel/pass-support)
;; A builtin module sees the prelude's macros without requiring them, but then
;; match-syntax's procedural half isn't found while its clauses are expanded,
;; and `and` / `or` patterns get expanded as code first
(require "#%private/steel/match")

(provide register-pass!
         passes-at
         pass-points
         pass-sites
         map-subforms
         ;; (lambda-rest? form): whether a lambda takes a rest argument, its last
         lambda-rest?
         ;; The analysis of the program the running pass was given, by syntax
         ;; (compiler/passes/pass_queries.rs). Made the first time a pass asks.
         ;; Symbols in what they return aren't eq? to ones written here: compare
         ;; them with equal? / member.
         binding-info
         function-info
         syntax-id
         binding-id
         binding-uses
         mutated?
         same-binding?
         struct-predicate?
         bound-name?
         boolean-primitive?
         reorderable-primitive?
         ;; (identifier-name stx): an identifier's name in the program (syntax-e
         ;; gives the name as written, without a module's prefix), or #f
         identifier-name
         identifier?
         keyword-syntax?
         called-primitive
         ;; (fresh-name 'prefix): a symbol no program has
         fresh-name
         compiler-setting
         ;; The built-in passes, one form at a time
         anonymous-lets
         empty-lets
         or-bindings
         hoist-car
         lower-rest
         unused-lets
         fold-primitives
         ;; (rebuild form parts): `form` with new parts, keeping its span, scopes
         ;; and identity. Use it rather than datum->syntax to rebuild a form a
         ;; pass was given.
         rebuild)

;; The binding an identifier refers to (or is), by id, or #f
(define (binding-id stx)
  (define info (binding-info stx))
  (and info (or (hash-ref info 'refers-to) (hash-ref info 'id))))

;; How often the binding an identifier refers to (or is) is used, or #f
(define (binding-uses stx)
  (define info (binding-info stx))
  (and info (hash-ref info 'uses)))

;; Whether the binding an identifier refers to (or is) is ever set!
(define (mutated? stx)
  (define info (binding-info stx))
  (and info (hash-ref info 'set!)))

;; Whether two identifiers refer to the same binding
(define (same-binding? a b)
  (define binding (binding-id a))
  (and binding (equal? binding (binding-id b))))

;; An identifier, not a literal or a keyword
(define (identifier? stx)
  (define name (identifier-name stx))
  (and name (not (starts-with? (symbol->string name) "#:"))))

;; A keyword, as in a call's keyword arguments (#:key)
(define (keyword-syntax? stx)
  (define name (identifier-name stx))
  (and name (starts-with? (symbol->string name) "#:")))

;; The primitive a call's head names, as a string without `#%prim.`: a
;; `#%prim.` name, or a name nothing in the program binds. #f otherwise. By
;; name (bound-name?), as inlined copies of a reference can share an id with
;; one that means something else.
(define (called-primitive head)
  (and (identifier? head)
       (let ([name (symbol->string (identifier-name head))])
         (cond
           [(starts-with? name "#%prim.") (substring name 7 (string-length name))]
           [(bound-name? head) #f]
           [else name]))))

;; Where passes can run, in the order compilation reaches them
(define pass-points '(after-constant-folding after-analysis-passes after-inlining before-code-generation))

;; Passes with a Rust version, each run where the compiler runs that version.
;; The site is the pass's name.
(define pass-sites '(anonymous-lets empty-lets or-bindings hoist-car lower-rest))

(define pass-modes '(off steel compare rust))

;; (name point default procedure), newest first
(define *passes* '())

;; Registers `procedure` as the pass `name` at `point`. `default` is its mode
;; when STEEL_PASSES doesn't name it: 'off, 'steel, 'compare (run with its Rust
;; version and compare) or 'rust. Registering a name again replaces the pass.
(define (register-pass! name point default procedure)
  (unless (symbol? name)
    (error "register-pass!: the name has to be a symbol" name))
  (unless (or (member point pass-points) (and (member point pass-sites) (equal? point name)))
    (error "register-pass!: no such point" point pass-points))
  (unless (member default pass-modes)
    (error "register-pass!: no such mode" default pass-modes))
  (unless (procedure? procedure)
    (error "register-pass!: a pass is a procedure" procedure))
  (set! *passes*
        (cons (list name point default procedure)
              (filter (lambda (pass) (not (equal? (car pass) name))) *passes*)))
  void)

;; The passes at `point`, oldest first, as (name default procedure)
(define (passes-at point)
  (map cdr-but-point (reverse (filter (lambda (pass) (equal? (cadr pass) point)) *passes*))))

(define (cdr-but-point pass)
  (list (car pass) (list-ref pass 2) (list-ref pass 3)))

;; Gives the program back as it came. For trying the machinery; its Rust
;; version does the same, so it also shows `compare` mode at work.
(register-pass! 'identity 'before-code-generation 'off (lambda (exprs context) exprs))
;; `stx` with `f` applied to each expression directly in it. Quoted data, and
;; the names a form binds or assigns, are left alone.
(define (map-subforms f stx)
  (match-syntax stx
    [(list 'quote _) stx]
    [(list (and head 'let) (and bindings (list pairs ...)) body)
     (rebuild stx
              (list head (rebuild bindings (map (lambda (pair) (map-binding f pair)) pairs)) (f body)))]
    [(list (and head (or 'lambda 'define 'set!)) target body) (rebuild stx (list head target (f body)))]
    [(list parts ...) (rebuild stx (map f parts))]
    [_ stx]))

(define (map-binding f binding)
  (match-syntax binding
    [(list name value) (rebuild binding (list name (f value)))]))

;; Pairs of `as` and `bs`, as many as the shorter has
(define (zip as bs)
  (if (or (null? as) (null? bs))
      '()
      (cons (list (car as) (car bs)) (zip (cdr as) (cdr bs)))))

;; The first `n` of `items`, or #f when there are fewer
(define (first-n items n)
  (cond
    [(= n 0) '()]
    [(null? items) #f]
    [else
     (let ([rest (first-n (cdr items) (- n 1))])
       (and rest (cons (car items) rest)))]))

;; ((lambda (a b) body) x y) to (let ([a x] [b y]) body), innermost calls
;; first. A rest argument gets the values past the others as a list.
;; Rust version: anonymous_calls_to_lets (passes/analysis.rs).
(define (anonymous-lets stx)
  (define inner (map-subforms anonymous-lets stx))
  (match-syntax inner
    [(list (and function (list (and keyword 'lambda) (list params ...) body)) values ...)
     (call->let inner function keyword params body values)]
    [_ inner]))

(define (call->let call function keyword params body values)
  (define bound
    (if (lambda-rest? function)
        (let* ([arity (max 0 (- (length params) 1))]
               [before (first-n values arity)])
          (and before
               (append before
                       (list (datum->syntax call
                                            (cons (datum->syntax call '#%prim.#%const-list)
                                                  (drop values arity)))))))
        values))
  (if bound
      (rebuild call
               (list (datum->syntax keyword 'let) (zip params bound) body))
      call))

;; Whether a let's body defines something, so the let is its scope
(define (defines? stx)
  (match-syntax stx
    [(list 'define _ _) #t]
    [(list 'begin parts ...) (any-defines? parts)]
    [_ #f]))

(define (any-defines? forms)
  (and (not (null? forms)) (or (defines? (car forms)) (any-defines? (cdr forms)))))

;; (let () body) and ((lambda () body)) to body when body defines nothing,
;; outermost first. Rust version: flatten_definition_free_empty_lets
;; (passes/analysis.rs).
(define (empty-lets stx)
  (match-syntax stx
    [(list (and function (list 'lambda (list) body)))
     #:when (and (not (lambda-rest? function)) (not (defines? body)))
     (empty-lets body)]
    [(list 'let (list) body) #:when (not (defines? body)) (empty-lets body)]
    [_ (map-subforms empty-lets stx)]))

;; `or` expands to (let ([z x]) (if z z y)). The binding goes when x can be
;; evaluated again for free (an atom), giving (if x x y), or when x is a call
;; that only answers #t or #f, giving (if x #t y). z has to be used only as the
;; test and the true arm. Innermost first, since that can expose the outer one.
;; Rust version: SimplifyOrBindings (passes/analysis.rs), which goes by names.
(define (or-bindings stx)
  (define inner (map-subforms or-bindings stx))
  (match-syntax inner
    [(list 'let (list (list name value)) (and body (list (and keyword 'if) test then otherwise)))
     #:when (and (same-binding? test name)
                 (same-binding? then name)
                 (eqv? (binding-uses name) 2)
                 (or (atom? value) (boolean-valued? value)))
     (rebuild body (list keyword value (if (atom? value) value (datum->syntax value #t)) otherwise))]
    [_ inner]))

;; An identifier or a literal
(define (atom? stx)
  (define e (syntax-e stx))
  (not (or (list? e) (vector? e))))

;; A call that answers #t or #f: to a struct predicate, or to a boolean valued
;; primitive the program doesn't shadow
(define (boolean-valued? stx)
  (match-syntax stx
    [(list head _ ...)
     (or (struct-predicate? head)
         (let ([primitive (called-primitive head)])
           (and primitive (boolean-primitive? (string->symbol primitive)))))]
    [_ #f]))

;; Drops the bindings of a let that nothing uses and whose values can't do
;; anything when evaluated: literals, local variables, quoted data and
;; lambdas. A let left with no bindings is its body, unless the body defines
;; something. New in Steel, no Rust version.
(define (unused-lets stx)
  (define inner (map-subforms unused-lets stx))
  (match-syntax inner
    [(list (and keyword 'let) (and bindings (list pairs ...)) body)
     (let ([kept (filter (lambda (pair) (not (unused-binding? pair))) pairs)])
       (cond
         [(= (length kept) (length pairs)) inner]
         [(and (null? kept) (not (defines? body))) body]
         [else (rebuild inner (list keyword (rebuild bindings kept) body))]))]
    [_ inner]))

(define (unused-binding? pair)
  (match-syntax pair
    [(list name value) (and (eqv? (binding-uses name) 0) (effect-free? value))]))

;; Evaluating it can't fail or do anything. A global might not be defined yet,
;; so only local variables count.
(define (effect-free? stx)
  (match-syntax stx
    [(list 'quote _) #t]
    [(list 'lambda _ _) #t]
    [_
     (and (atom? stx)
          (or (not (symbol? (syntax-e stx)))
              (let ([info (binding-info stx)])
                (and info (not (member (hash-ref info 'kind) '(global free)))))))]))

;; ---------------------------------------------------------------------------
;; hoist-car: binds (car x) before a call that also passes (cdr x), so the
;; cdr is x's last use and can advance the list in place:
;; (loop (cdr l) (+ acc (car l))) to
;; (let ([t (car l)]) (loop (cdr l) (+ acc t))). Also a let's bindings, which
;; is what a call becomes once inlined. Only where every argument is effect
;; free, and (unless hoist-car-all) only for a function's own calls in tail
;; position. Rust version: HoistCarBeforeCdr (passes/analysis.rs).
(define (hoist-car exprs)
  (define tail-only (not (compiler-setting 'hoist-car-all)))
  (define (worth? tail calls-self)
    (or (not tail-only) (and tail calls-self)))

  (define (walk stx tail enclosing)
    (define (sub e t)
      (walk e t enclosing))
    (define inner
      (match-syntax stx
        [(list 'quote _) stx]
        [(list (and head 'define) name body)
         (rebuild stx
                  (list head
                        name
                        (walk body
                              #f
                              (if (identifier? name)
                                  (cons (identifier-name name) enclosing)
                                  enclosing))))]
        [(list (and head 'lambda) args body) (rebuild stx (list head args (sub body #t)))]
        [(list (and head 'if) test then otherwise)
         (rebuild stx (list head (sub test #f) (sub then tail) (sub otherwise tail)))]
        [(list (and head 'begin) forms ...) (rebuild stx (cons head (walk-begin forms tail enclosing)))]
        [(list (and head 'let) (and bindings (list pairs ...)) body)
         (rebuild stx
                  (list head
                        (rebuild bindings
                                 (map (lambda (pair) (map-binding (lambda (v) (sub v #f)) pair))
                                      pairs))
                        (sub body tail)))]
        [(list (and head 'set!) name value) (rebuild stx (list head name (sub value tail)))]
        [(list (and head 'return!) value) (rebuild stx (list head (sub value tail)))]
        [(list parts ...) (rebuild stx (map (lambda (part) (sub part #f)) parts))]
        [_ stx]))
    (or (hoist inner tail enclosing) inner))

  ;; Only a begin's last form is in its position
  (define (walk-begin forms tail enclosing)
    (cond
      [(null? forms) '()]
      [(null? (cdr forms)) (list (walk (car forms) tail enclosing))]
      [else (cons (walk (car forms) #f enclosing) (walk-begin (cdr forms) tail enclosing))]))

  ;; The rewritten form, or #f
  (define (hoist stx tail enclosing)
    (match-syntax stx
      [(list (or 'quote 'define 'lambda 'if 'begin 'set! 'return!) _ ...) #f]
      [(list (and head 'let) (and bindings (list pairs ...)) body)
       (let ([var (let-candidate pairs body tail enclosing)])
         (and var
              (hoisted stx
                       var
                       (lambda (name)
                         (rebuild stx
                                  (list head
                                        (rebuild bindings
                                                 (map (lambda (pair)
                                                        (map-binding (lambda (v) (replace-car v var name))
                                                                     pair))
                                                      pairs))
                                        body))))))]
      [(list callee args ...)
       (let ([var (call-candidate callee args tail enclosing)])
         (and var
              (hoisted stx
                       var
                       (lambda (name)
                         (rebuild stx (cons callee (map (lambda (a) (replace-car a var name)) args)))))))]
      [_ #f]))

  (define (hoisted stx var replaced)
    (define name (fresh-name (string->symbol "##%car-hoist")))
    (datum->syntax stx (list 'let (list (list name (list '#%prim.car var))) (replaced name))))

  (define (call-candidate callee args tail enclosing)
    (and (identifier? callee)
         (>= (length args) 2)
         (all? reorderable? args)
         (let ([calls-self (and (pair? enclosing) (equal? (identifier-name callee) (car enclosing)))])
           (let loop ([args args])
             (cond
               [(null? args) #f]
               [(accessor-of (car args) "cdr")
                =>
                (lambda (var)
                  (if (and (only-cars-of? var (cdr args)) (worth? tail calls-self))
                      var
                      (loop (cdr args))))]
               [else (loop (cdr args))])))))

  (define (let-candidate pairs body tail enclosing)
    (define values (map binding-value pairs))
    (and (all? reorderable? values)
         (let ([recurses (and (pair? enclosing) (> (reference-count body (car enclosing)) 0))])
           (let loop ([values values])
             (cond
               [(null? values) #f]
               [(accessor-of (car values) "cdr")
                =>
                (lambda (var)
                  (if (and (= (reference-count body var) 0)
                           (only-cars-of? var (cdr values))
                           (worth? tail recurses))
                      var
                      (loop (cdr values))))]
               [else (loop (cdr values))])))))

  ;; Every mention of var in later is a (car var), and there is one
  (define (only-cars-of? var later)
    (define cars (apply + (map (lambda (e) (count-car e var)) later)))
    (and (> cars 0) (= cars (apply + (map (lambda (e) (reference-count e var)) later)))))

  (map (lambda (e) (walk e #t '())) exprs))

(define (binding-value pair)
  (match-syntax pair
    [(list _ value) value]))

(define (all? ok? items)
  (or (null? items) (and (ok? (car items)) (all? ok? (cdr items)))))

;; The variable of (op x), op a primitive such as "car", or #f
(define (accessor-of stx op)
  (match-syntax stx
    [(list head arg)
     #:when (and (equal? (called-primitive head) op) (identifier? arg))
     (identifier-name arg)]
    [_ #f]))

;; Evaluating it has no effects, so calls' arguments can go in another order
(define (reorderable? stx)
  (match-syntax stx
    [(list 'quote _) #t]
    [(list (or 'define 'lambda 'let 'if 'begin 'set! 'return!) _ ...) #f]
    [(list head args ...)
     (let ([primitive (called-primitive head)])
       (and primitive (reorderable-primitive? (string->symbol primitive)) (all? reorderable? args)))]
    [_ (atom? stx)]))

;; How often name appears, as passes/analysis.rs's reference_count counts:
;; everywhere but the names a let or lambda binds, quoted data included
(define (reference-count stx name)
  (define (count-all items)
    (apply + (map (lambda (e) (reference-count e name)) items)))
  (match-syntax stx
    [(list 'let (list pairs ...) body) (+ (count-all (map binding-value pairs)) (reference-count body name))]
    [(list 'lambda _ body) (reference-count body name)]
    [(list parts ...) (count-all parts)]
    [_
     (let ([e (syntax-e stx)])
       (cond
         [(vector? e) (count-all (vector->list e))]
         [(equal? (identifier-name stx) name) 1]
         [else 0]))]))

;; How many (car var) there are, outside quoted data
(define (count-car stx var)
  (define (count-all items)
    (apply + (map (lambda (e) (count-car e var)) items)))
  (cond
    [(equal? (accessor-of stx "car") var) 1]
    [else
     (match-syntax stx
       [(list 'quote _) 0]
       [(list 'let (list pairs ...) body) (+ (count-all (map binding-value pairs)) (count-car body var))]
       [(list 'lambda _ body) (count-car body var)]
       [(list parts ...) (count-all parts)]
       [_
        (let ([e (syntax-e stx)])
          (if (vector? e) (count-all (vector->list e)) 0))])]))

;; stx with each (car var) outside quoted data replaced by name
(define (replace-car stx var name)
  (cond
    [(equal? (accessor-of stx "car") var) (datum->syntax stx name)]
    [else
     (match-syntax stx
       [(list 'quote _) stx]
       [(list parts ...) (rebuild stx (map (lambda (e) (replace-car e var name)) parts))]
       [_ stx])]))

;; ---------------------------------------------------------------------------
;; lower-rest: a rest argument list a call builds, (#%prim.#%const-list a b ...),
;; known where the function was inlined, so the keyword and optional argument
;; getters (#%prim.plist-...) and (apply (lambda ...) args) that read it are
;; resolved at compile time and the list goes. Arguments that can't be moved
;; (effects, set! variables, globals) are bound to names first.
;; Rust version: LowerRestArguments (passes/analysis.rs).
(define (lower-rest exprs)
  ;; Scoped tables, innermost layer first: name -> value
  (define (make-scopes)
    (box (list (hash))))
  (define (push! scopes)
    (set-box! scopes (cons (hash) (unbox scopes))))
  (define (pop! scopes)
    (when (pair? (cdr (unbox scopes)))
      (set-box! scopes (cdr (unbox scopes)))))
  (define (define! scopes key value)
    (set-box! scopes (cons (hash-insert (car (unbox scopes)) key value) (cdr (unbox scopes)))))
  (define (lookup scopes key)
    (let loop ([layers (unbox scopes)])
      (cond
        [(null? layers) missing]
        [(hash-contains? (car layers) key) (hash-ref (car layers) key)]
        [else (loop (cdr layers))])))
  (define (defined? scopes key)
    (not (eq? (lookup scopes key) missing)))

  ;; const list name -> its arguments; names lowering used up; whether a
  ;; const list's arguments can all be moved
  (define bindings (make-scopes))
  (define used (make-scopes))
  (define movable (make-scopes))
  ;; Const lists with an argument that can't move, and the names made for
  ;; such arguments
  (define needs-names (box (hash)))
  (define argument-names (box (hash)))

  (define (can-move? arg)
    (match-syntax arg
      [(list 'quote _) #t]
      [_
       (cond
         [(not (atom? arg)) #f]
         [(not (identifier? arg)) #t]
         [(hash-contains? (unbox argument-names) (identifier-name arg)) #t]
         [else
          (let ([info (binding-info arg)])
            (and info
                 (not (hash-ref info 'set!))
                 (not (member (hash-ref info 'binding-kind) '(global free)))))])]))

  ;; The arguments of the const list bound to name if they can all move;
  ;; else marks it as needing names
  (define (movable-const-list name)
    (define can (lookup movable name))
    (cond
      [(eq? can missing) #f]
      [(not can)
       (set-box! needs-names (hash-insert (unbox needs-names) name #t))
       #f]
      [else
       (let ([args (lookup bindings name)])
         (if (eq? args missing) #f args))]))

  (define (use! name)
    (define! used name #t))

  (define (const-list? stx)
    (match-syntax stx
      [(list head _ ...) (equal? (identifier-name head) '#%prim.#%const-list)]
      [_ #f]))

  (define (call-error stx message)
    (datum->syntax stx (list '#%prim.error-with-span (list '#%prim.current-function-span) message)))

  (define (int-literal stx)
    (define e (syntax-e stx))
    (and (int? e) e))

  ;; The element after the first that names key (a keyword, or an
  ;; identifier spelled like one), or #f
  (define (value-of-key items key keyword-only)
    (let loop ([items items])
      (cond
        [(null? items) #f]
        [(and (equal? (identifier-name (car items)) key)
              (or (not keyword-only) (keyword-syntax? (car items))))
         (if (pair? (cdr items)) (cadr items) #f)]
        [else (loop (cdr items))])))

  ;; The index-th argument that isn't a keyword or a keyword's value, or #f
  (define (positional items index)
    (let loop ([items items]
               [offset 0])
      (cond
        [(null? items) #f]
        [(keyword-syntax? (car items)) (loop (if (pair? (cdr items)) (cddr items) '()) offset)]
        [(= offset index) (car items)]
        [else (loop (cdr items) (+ offset 1))])))

  ;; The arguments from the index-th positional one on, keywords left out,
  ;; or #f
  (define (positionals-from items index)
    (let loop ([items items]
               [offset 0])
      (cond
        [(null? items) #f]
        [(keyword-syntax? (car items)) (loop (if (pair? (cdr items)) (cddr items) '()) offset)]
        [(= offset index) (cons (car items) (positional-rest (cdr items)))]
        [else (loop (cdr items) (+ offset 1))])))
  (define (positional-rest items)
    (cond
      [(null? items) '()]
      [(keyword-syntax? (car items)) (positional-rest (if (pair? (cdr items)) (cddr items) '()))]
      [else (cons (car items) (positional-rest (cdr items)))]))

  ;; What a binding's value becomes once the const list it reads is known,
  ;; or #f
  (define (resolve-getter value)
    (match-syntax value
      [(list 'if test _ otherwise) (resolve-lazy-default test otherwise)]
      [(list head args ...)
       (let ([getter (identifier-name head)])
         (cond
           [(equal? getter '#%prim.plist-try-get)
            (match-syntax value
              [(list _ list-name key default _ ...)
               #:when (and (identifier? list-name) (keyword-syntax? key))
               (let ([items (movable-const-list (identifier-name list-name))])
                 (and items
                      (begin
                        (use! (identifier-name list-name))
                        (or (value-of-key items (identifier-name key) #f) default))))]
              [_ #f])]
           [(equal? getter '#%prim.plist-try-get-positional-arg)
            (match-syntax value
              [(list _ list-name index default _ ...)
               #:when (and (identifier? list-name) (int-literal index))
               (let ([items (movable-const-list (identifier-name list-name))])
                 (and items
                      (begin
                        (use! (identifier-name list-name))
                        (or (positional items (int-literal index)) default))))]
              [_ #f])]
           [(equal? getter '#%prim.plist-get-positional-arg-list)
            (match-syntax value
              [(list _ list-name index _ ...)
               #:when (and (identifier? list-name) (int-literal index))
               (let ([items (movable-const-list (identifier-name list-name))])
                 (and items
                      (begin
                        (use! (identifier-name list-name))
                        (datum->syntax value
                                       (cons '#%prim.list
                                             (or (positionals-from items (int-literal index)) '()))))))]
              [_ #f])]
           [(equal? getter '#%prim.plist-get-positional-arg)
            (match-syntax value
              [(list _ list-name index _ ...)
               #:when (and (identifier? list-name) (int-literal index))
               (let ([items (movable-const-list (identifier-name list-name))])
                 (and items
                      (begin
                        (use! (identifier-name list-name))
                        (or (positional items (int-literal index))
                            (call-error value "Missing positional arg")))))]
              [_ #f])]
           [(equal? getter '#%prim.plist-get-kwarg)
            (match-syntax value
              [(list _ list-name key function-name _ ...)
               #:when (and (identifier? list-name) (keyword-syntax? key))
               (let ([items (movable-const-list (identifier-name list-name))])
                 (and items
                      (begin
                        (use! (identifier-name list-name))
                        (kwarg-value value items key function-name))))]
              [_ #f])]
           [else #f]))]
      [_ #f]))

  (define (kwarg-value value items key function-name)
    (define name
      (match-syntax function-name
        [(list 'quote quoted) (written quoted)]
        [_ (written function-name)]))
    (define key-name (symbol->string (identifier-name key)))
    (let loop ([items items])
      (cond
        [(null? items) (call-error value (string-append name " : Key not found: " key-name))]
        [(and (keyword-syntax? (car items)) (equal? (identifier-name (car items)) (identifier-name key)))
         (if (pair? (cdr items))
             (cadr items)
             (call-error value (string-append name " : Missing value for key: " key-name)))]
        [else (loop (cdr items))])))

  ;; A name or literal as the Rust pass prints it
  (define (written stx)
    (define name (identifier-name stx))
    (if name (symbol->string name) (to-string (syntax->datum stx))))

  ;; (if (has? args which) (get args which) default): the argument, or the
  ;; default when the call doesn't pass it
  (define (resolve-lazy-default test otherwise)
    (match-syntax test
      [(list has list-name which _ ...)
       #:when (identifier? list-name)
       (let ([has (identifier-name has)])
         (cond
           [(and (equal? has '#%prim.plist-has-positional-arg?) (int-literal which))
            (let ([items (movable-const-list (identifier-name list-name))])
              (and items
                   (begin
                     (use! (identifier-name list-name))
                     (or (positional items (int-literal which)) otherwise))))]
           [(and (equal? has '#%prim.plist-has-kwarg?) (keyword-syntax? which))
            (let ([items (movable-const-list (identifier-name list-name))])
              (and items
                   (begin
                     (use! (identifier-name list-name))
                     (or (value-of-key items (identifier-name which) #f) otherwise))))]
           [else #f]))]
      [_ #f]))

  ;; (#%prim.plist-check-keywords args '(#:a #:b) ...) of a known argument
  ;; list: #t, or the check of just the keyword that makes it fail
  (define (check-keywords stx)
    (match-syntax stx
      [(list head list-name (list 'quote (list declared ...)) _ ...)
       #:when (and (equal? (identifier-name head) '#%prim.plist-check-keywords) (identifier? list-name))
       (let ([items (lookup bindings (identifier-name list-name))])
         (and (not (eq? items missing))
              (let loop ([items items]
                         [seen '()])
                (cond
                  [(null? items) (datum->syntax stx #t)]
                  [(keyword-syntax? (car items))
                   (let* ([keyword (identifier-name (car items))]
                          [undeclared (not (ormap-name keyword declared))])
                     (if (or undeclared (member keyword seen))
                         (let ([given (append (list '#%prim.list (car items) #f)
                                              (if undeclared '() (list (car items) #f)))])
                           (rebuild stx
                                    (cons head
                                          (cons (datum->syntax stx given) (cddr (syntax-e stx))))))
                         (loop (if (pair? (cdr items)) (cddr items) '()) (cons keyword seen))))]
                  [else (loop (cdr items) seen)]))))]
      [_ #f]))

  (define (ormap-name name items)
    (and (pair? items) (or (equal? (identifier-name (car items)) name) (ormap-name name (cdr items)))))

  (define (core-form? stx)
    (match-syntax stx
      [(list (or 'quote 'define 'lambda 'let 'if 'begin 'set! 'return!) _ ...) #t]
      [_ #f]))

  (define (visit stx)
    (match-syntax stx
      [(list 'let (and bindings-stx (list pairs ...)) body) (visit-let stx bindings-stx pairs body)]
      [(list _ _ ...)
       #:when (not (core-form? stx))
       (or (check-keywords stx) (map-subforms visit stx))]
      [_ (map-subforms visit stx)]))

  (define (pair-name pair)
    (match-syntax pair
      [(list name _) name]))

  (define (visit-let stx bindings-stx pairs body)
    ;; Binding values are their own expressions, in the enclosing scope
    (define visited (map (lambda (pair) (map-binding visit pair)) pairs))
    (define names (map pair-name visited))
    (define values (map binding-value visited))
    ;; Getters read lists bound further out, so before this let's are in view
    (define replacements (map resolve-getter values))

    (push! bindings)
    (push! movable)
    (let loop ([names names]
               [values values])
      (when (pair? names)
        (let ([name (car names)]
              [value (car values)])
          (when (and (identifier? name) (const-list? value))
            (let ([args (cdr (syntax-e value))])
              (define! movable (identifier-name name) (all? can-move? args))
              (define! bindings (identifier-name name) args))))
        (loop (cdr names) (cdr values))))
    (push! used)

    (define new-body (visit body))
    ;; The outer const list an apply here was lowered from, if any
    (define lowered-from-outer #f)
    (define lowered-body
      (match-syntax new-body
        [(list head (and function (list 'lambda (list params ...) function-body)) list-name)
         #:when (and (not (core-form? new-body))
                     (equal? (identifier-name head) '#%prim.apply)
                     (identifier? list-name)
                     (defined? bindings (identifier-name list-name))
                     (let ([info (binding-info list-name)])
                       (and info (hash-ref info 'refers-to) (eqv? (hash-ref info 'uses) 1)))
                     (not (lambda-rest? function))
                     (= (length params) (length (lookup bindings (identifier-name list-name))))
                     (movable-const-list (identifier-name list-name)))
         (let* ([key (identifier-name list-name)]
                [arguments (lookup bindings key)])
           (use! key)
           (unless (ormap-name key names)
             (set! lowered-from-outer key))
           (datum->syntax stx (list 'let (zip params arguments) function-body)))]
        [_ new-body]))

    (define removed (map (lambda (name) (and (identifier? name) (defined? used (identifier-name name)))) names))
    (pop! used)
    (pop! bindings)
    (pop! movable)
    (when lowered-from-outer
      (use! lowered-from-outer))

    (define kept
      (let loop ([pairs visited]
                 [replacements replacements]
                 [removed removed])
        (cond
          [(null? pairs) '()]
          [(car removed) (loop (cdr pairs) (cdr replacements) (cdr removed))]
          [else
           (cons (if (car replacements)
                     (rebuild-binding (car pairs) (car replacements))
                     (car pairs))
                 (loop (cdr pairs) (cdr replacements) (cdr removed)))])))

    (define result (rebuild stx (list (car (syntax-e stx)) (rebuild bindings-stx kept) lowered-body)))
    (if (any-needs-names? kept)
        (name-arguments stx result)
        result))

  (define (rebuild-binding pair value)
    (match-syntax pair
      [(list name _) (rebuild pair (list name value))]))

  (define (any-needs-names? pairs)
    (and (pair? pairs)
         (or (let ([name (pair-name (car pairs))])
               (and (const-list? (binding-value (car pairs)))
                    (identifier? name)
                    (hash-contains? (unbox needs-names) (identifier-name name))))
             (any-needs-names? (cdr pairs)))))

  ;; (let ([a A] [args (#%prim.#%const-list X Y)]) body) to
  ;; (let ([a A] [x X] [y Y]) (let ([args (#%prim.#%const-list x y)]) body))
  ;; for the lists that need names, so each argument is evaluated once and in
  ;; order; then the inner let is visited again
  (define (name-arguments stx result)
    (match-syntax result
      [(list head (list pairs ...) body)
       (let loop ([pairs pairs]
                  [outer '()]
                  [inner '()])
         (if (null? pairs)
             (rebuild result
                      (list head
                            (reverse outer)
                            (visit (datum->syntax stx (list 'let (reverse inner) body)))))
             (let* ([pair (car pairs)]
                    [name (pair-name pair)]
                    [value (binding-value pair)]
                    [wanted (and (const-list? value)
                                 (identifier? name)
                                 (hash-contains? (unbox needs-names) (identifier-name name)))])
               (if (not wanted)
                   (loop (cdr pairs) (cons pair outer) inner)
                   (begin
                     (set-box! needs-names (hash-remove (unbox needs-names) (identifier-name name)))
                     (let name-args ([args (cdr (syntax-e value))]
                                     [named '()]
                                     [outer outer])
                       (cond
                         [(null? args)
                          (loop (cdr pairs)
                                outer
                                (cons (rebuild-binding pair
                                                       (rebuild value
                                                                (cons (car (syntax-e value))
                                                                      (reverse named))))
                                      inner))]
                         [(can-move? (car args)) (name-args (cdr args) (cons (car args) named) outer)]
                         [else
                          (let ([fresh (fresh-name (string->symbol "##rest-argument"))])
                            (set-box! argument-names
                                      (hash-insert (unbox argument-names) fresh #t))
                            (name-args (cdr args)
                                       (cons (datum->syntax (car args) fresh) named)
                                       (cons (list (datum->syntax (car args) fresh) (car args))
                                             outer)))])))))))]))

  (map visit exprs))

(define missing (list 'missing))

;; A primitive called with literals, when calling it at compile time is fine
;; (constant-primitive) and it gives a number, boolean or character back:
;; (#%prim.+ 1 2) to 3. Strings stay calls, since each call makes a new one.
(define (fold-primitives stx)
  (define inner (map-subforms fold-primitives stx))
  (match-syntax inner
    [(list head args ...)
     #:when (symbol? (syntax-e head))
     (let ([primitive (constant-primitive (syntax-e head))])
       (if (and primitive (all-literals? args))
           (fold-call inner primitive args)
           inner))]
    [_ inner]))

(define (all-literals? args)
  (or (null? args)
      (and (program-literal? (syntax-e (car args))) (all-literals? (cdr args)))))

(define fold-failed (list 'fold-failed))

(define (fold-call call primitive args)
  (define result
    (with-handler (lambda (err) fold-failed) (apply primitive (map syntax-e args))))
  (if (and (not (eq? result fold-failed)) (program-literal? result) (not (string? result)))
      (datum->syntax call result)
      call))

(register-pass! 'anonymous-lets 'anonymous-lets 'steel
                (lambda (exprs context) (map anonymous-lets exprs)))
(register-pass! 'empty-lets 'empty-lets 'steel (lambda (exprs context) (map empty-lets exprs)))
(register-pass! 'or-bindings 'or-bindings 'steel (lambda (exprs context) (map or-bindings exprs)))
(register-pass! 'hoist-car 'hoist-car 'steel (lambda (exprs context) (hoist-car exprs)))
(register-pass! 'lower-rest 'lower-rest 'steel (lambda (exprs context) (lower-rest exprs)))
(register-pass! 'unused-lets 'after-inlining 'off (lambda (exprs context) (map unused-lets exprs)))
(register-pass! 'fold-primitives 'after-analysis-passes 'off
                (lambda (exprs context) (map fold-primitives exprs)))
