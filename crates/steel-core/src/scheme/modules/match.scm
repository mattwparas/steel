;; These _don't_ need to be provided for syntax.
;; However in the mean time, this will work.
(provide match
         match*
         match-define
         match-lambda
         match-lambda*
         match-let
         match-let*
         match-syntax)

; (require-builtin steel/base)

;; A pattern's `list` or `cons`, as written or under the module prefix a template in another
;; module wrote it with
(begin-for-syntax
  (define (pattern-keyword? x name)
    (or (equal? x name)
        (and (symbol? x)
             (ends-with? (symbol->string x) (string-append "__%#__" (symbol->string name))))))

  (define *gensym-counter* 0)
  (define (gensym)
    (set! *gensym-counter* (+ 1 *gensym-counter*))
    (string->symbol (string-append "##gensym" (to-string *gensym-counter*))))

  (define (quoted? x)
    (and (list? x) (not (null? x)) (equal? (car x) 'quote)))

  (define unquote-names '(unquote #%unquote))
  (define unquote-splicing-names '(unquote-splicing #%unquote-splicing))

  (define (pattern-identifiers pattern found)
    (define inner (if (syntax? pattern) (#%syntax-e pattern) pattern))
    (cond
      [(symbol? inner) (cons pattern found)]
      [(list? inner)
       (let loop ([items inner]
                  [found found])
         (if (null? items)
             found
             (loop (cdr items)
                   (if (syntax? (car items))
                       (pattern-identifiers (car items) found)
                       found))))]
      [else found]))

  ;; A pattern is compiled with the syntax it was written with turned into data, except for the
  ;; expressions in it (predicates, functions, values and struct names), which stay syntax.
  (define pattern-heads '(list list-rest list* cons vector box and or not))

  (define (head-keyword x)
    (cond
      [(not (symbol? x)) #f]
      [(pattern-keyword? x '?) '?]
      [(pattern-keyword? x 'app) 'app]
      [(pattern-keyword? x '==) '==]
      [(pattern-keyword? x 'struct) 'struct]
      [(pattern-keyword? x 'hash-table) 'hash-table]
      [else
       (let loop ([heads pattern-heads])
         (cond
           [(null? heads) #f]
           [(pattern-keyword? x (car heads)) (car heads)]
           [else (loop (cdr heads))]))]))

  ;; Quoted parts of syntax can be plain data
  (define (stx-e x)
    (if (syntax? x) (syntax-e x) x))

  (define (datum-of x)
    (if (syntax? x) (#%syntax->datum x) x))

  (define (pattern-of stx)
    (define e (stx-e stx))
    (cond
      [(symbol? e) e]
      [(null? e) (list 'quote '())]
      [(list? e)
       (define head (datum-of (car e)))
       (define keyword (head-keyword head))
       (cond
         [(equal? head 'quote) (datum-of stx)]
         [(equal? head 'quasiquote) (pattern-of-quasi (cadr e))]
         [(or (equal? keyword '?) (equal? keyword 'app))
          (when (null? (cdr e))
            (error! "match: bad pattern" (datum-of stx)))
          (cons keyword (cons (cadr e) (map pattern-of (cddr e))))]
         [(equal? keyword '==) (cons '== (cdr e))]
         [(equal? keyword 'hash-table) (cons 'hash-table (map hash-entry-of (cdr e)))]
         [(equal? keyword 'struct)
          (unless (= (length e) 3)
            (error! "match: bad struct pattern" (datum-of stx)))
          (list 'struct (cadr e) (map pattern-of (stx-list (caddr e))))]
         [keyword (cons keyword (map pattern-of (cdr e)))]
         [(symbol? head)
          (cons (if (syntax? (car e)) (car e) (#%datum->syntax stx head)) (map pattern-of (cdr e)))]
         [else (error! "match: bad pattern" (datum-of stx))])]
      [else (datum-of stx)]))

  (define (hash-entry-of stx)
    (define e (stx-e stx))
    (cond
      [(symbol? e) e]
      [(and (list? e) (= (length e) 2)) (list 'entry (pattern-of (car e)) (pattern-of (cadr e)))]
      [else (error! "match: bad hash-table pattern" (datum-of stx))]))

  (define (pattern-of-quasi stx)
    (define e (stx-e stx))
    (cond
      [(and (list? e) (= (length e) 2) (member (datum-of (car e)) unquote-names))
       (pattern-of (cadr e))]
      [(null? e) (list 'quote '())]
      [(list? e) (cons 'list (quasi-items e))]
      [else (list 'quote (datum-of stx))]))

  (define (quasi-items items)
    (cond
      [(null? items) '()]
      [(let ([e (stx-e (car items))])
         (and (list? e) (= (length e) 2) (member (datum-of (car e)) unquote-splicing-names)))
       (cons (pattern-of (cadr (stx-e (car items)))) (cons '... (quasi-items (cdr items))))]
      [else (cons (pattern-of-quasi (car items)) (quasi-items (cdr items)))]))

  ;; The fewest elements an ellipsis matches, or #f if it isn't one
  (define (ellipsis-min x)
    (cond
      [(not (symbol? x)) #f]
      [(or (equal? x '...) (equal? x '___)) 0]
      [else
       (let ([s (symbol->string x)])
         (and (> (string-length s) 2)
              (or (starts-with? s "..") (starts-with? s "__"))
              (string->number (substring s 2 (string-length s)))))]))

  (define (wildcard? x)
    (equal? x '_))

  ;; The variables a pattern binds, in the order they're written
  (define (pattern-vars pat found)
    (cond
      [(symbol? pat)
       (if (or (wildcard? pat) (ellipsis-min pat) (member pat found)) found (cons pat found))]
      [(or (not (pair? pat)) (quoted? pat)) found]
      [(syntax? (car pat)) (pattern-vars-of (cdr pat) found)]
      [(or (equal? (car pat) '?) (equal? (car pat) 'app)) (pattern-vars-of (cddr pat) found)]
      [(or (equal? (car pat) '==) (equal? (car pat) 'not)) found]
      [(equal? (car pat) 'struct) (pattern-vars-of (caddr pat) found)]
      [else (pattern-vars-of (cdr pat) found)]))

  (define (pattern-vars-of pats found)
    (if (null? pats)
        found
        (pattern-vars-of (cdr pats) (pattern-vars (car pats) found))))

  (define (vars-of pat)
    (reverse (pattern-vars pat '())))

  (define (mk-let bindings body)
    (list 'let bindings body))

  (define (mk-if test then else)
    (list 'if test then else))

  (define (mk-lambda args body)
    (list 'lambda args body))

  (define (calls-of expr name)
    (cond
      [(and (pair? expr) (equal? (car expr) name) (null? (cdr expr))) 1]
      [(list? expr) (apply + (map (lambda (x) (calls-of x name)) expr))]
      [else 0]))

  (define (replace-call expr name body)
    (cond
      [(and (pair? expr) (equal? (car expr) name) (null? (cdr expr))) body]
      [(list? expr) (map (lambda (x) (replace-call x name body)) expr)]
      [else expr]))

  ;; The expression `make` builds given a call that runs `body`. The call only ever appears where
  ;; nothing but the matcher's own temporaries are bound, so a single call is replaced by `body`.
  (define (with-thunk make body)
    (define name (gensym))
    (define expr (make (list name)))
    (define uses (calls-of expr name))
    (cond
      [(= uses 0) expr]
      [(= uses 1) (replace-call expr name body)]
      [else (mk-let (list (list name (mk-lambda '() body))) expr)]))

  (define (lookup var env)
    (let ([found (assoc var env)])
      (if found (cdr found) #f)))

  ;; The code that matches `pat` against the variable `v`. `k` makes the code for a match from the
  ;; variables bound so far, which are bound to temporaries until the whole pattern has matched.
  ;; `fail` is the code for a failed match.
  (define (compile-pattern pat v env k fail)
    (cond
      [(symbol? pat)
       (cond
         [(wildcard? pat) (k env)]
         [(ellipsis-min pat) (error! "match: an ellipsis must follow a pattern in a list or vector")]
         [(assoc pat env) (mk-if (list '#%prim.equal? v (lookup pat env)) (k env) fail)]
         [else (k (cons (cons pat v) env))])]
      [(quoted? pat)
       (inspect v
                (lambda (v)
                  (if (null? (cadr pat))
                      (mk-if (list '#%prim.null? v) (k env) fail)
                      (mk-if (list '#%prim.equal? v pat) (k env) fail))))]
      [(and (pair? pat) (structural? pat))
       (inspect v (lambda (v) (compile-compound pat v env k fail)))]
      [(pair? pat) (compile-compound pat v env k fail)]
      [else (inspect v (lambda (v) (mk-if (list '#%prim.equal? v (list 'quote pat)) (k env) fail)))]))

  ;; In match-syntax, a value is looked inside after unwrapping it, and variables bind the
  ;; syntax itself
  (define *syntax-mode* #f)

  (define (inspect v make)
    (if *syntax-mode*
        (bind-temp (mk-if (list 'syntax? v) (list 'syntax-e v) v) make)
        (make v)))

  (define (structural? pat)
    (or (syntax? (car pat))
        (member (car pat) '(struct list list-rest list* cons vector box hash-table))))

  (define (compile-all pats v env k fail)
    (if (null? pats)
        (k env)
        (compile-pattern (car pats)
                         v
                         env
                         (lambda (env) (compile-all (cdr pats) v env k fail))
                         fail)))

  (define (bind-temp expr make)
    (define t (gensym))
    (mk-let (list (list t expr)) (make t)))

  (define (all-but-last xs)
    (reverse (cdr (reverse xs))))

  (define (compile-compound pat v env k fail)
    (define head (car pat))
    (define args (cdr pat))
    (cond
      [(syntax? head)
       (compile-struct head args v env k fail)]
      [(equal? head 'struct) (compile-struct (car args) (cadr args) v env k fail)]
      [(equal? head 'list) (compile-list args (list 'quote '()) v env k fail)]
      [(or (equal? head 'list-rest) (equal? head 'list*))
       (when (null? args)
         (error! "match: list-rest needs a pattern for the rest of the list"))
       (compile-list (all-but-last args) (last args) v env k fail)]
      [(equal? head 'cons)
       (unless (= (length args) 2)
         (error! "match: cons takes two patterns"))
       (compile-list (list (car args)) (cadr args) v env k fail)]
      [(equal? head 'vector)
       (mk-if (list '#%prim.vector? v)
              (bind-temp (list 'vector->list v)
                         (lambda (t) (compile-list args (list 'quote '()) t env k fail)))
              fail)]
      [(equal? head 'box)
       (unless (= (length args) 1)
         (error! "match: box takes one pattern"))
       (mk-if (list 'box? v)
              (bind-temp (list 'unbox v)
                         (lambda (t) (compile-pattern (car args) t env k fail)))
              fail)]
      [(equal? head '?)
       (mk-if (list (car args) v) (compile-all (cdr args) v env k fail) fail)]
      [(equal? head 'app)
       (bind-temp (list (car args) v) (lambda (t) (compile-all (cdr args) t env k fail)))]
      [(equal? head '==)
       (mk-if (if (null? (cdr args))
                  (list '#%prim.equal? v (car args))
                  (list (cadr args) (car args) v))
              (k env)
              fail)]
      [(equal? head 'and) (compile-all args v env k fail)]
      [(equal? head 'not)
       (with-thunk (lambda (matched)
                     (let loop ([pats args])
                       (if (null? pats)
                           matched
                           (with-thunk (lambda (next)
                                         (compile-pattern (car pats) v env (lambda (_) fail) next))
                                       (loop (cdr pats))))))
                   (k env))]
      [(equal? head 'or) (compile-or args v env k fail)]
      [(equal? head 'hash-table) (mk-if (list '#%prim.hash? v) (compile-hash args v env k fail) fail)]
      [else (error! "match: bad pattern" pat)]))

  ;; Each alternative jumps to one continuation with the variables any of them bind, and the ones
  ;; it doesn't bind are #f
  (define (compile-or alts v env k fail)
    (define vars (vars-of (cons 'and alts)))
    (define params (map (lambda (_) (gensym)) vars))
    (define (alternatives jump)
      (let loop ([alts alts])
        (if (null? alts)
            fail
            (with-thunk (lambda (next)
                          (compile-pattern (car alts)
                                           v
                                           env
                                           (lambda (env)
                                             (if (null? vars)
                                                 jump
                                                 (cons (car jump)
                                                       (map (lambda (var) (lookup var env)) vars))))
                                           next))
                        (loop (cdr alts))))))
    (define rest-env (append (map cons vars params) env))
    (if (null? vars)
        (with-thunk alternatives (k env))
        (let ([name (gensym)])
          (mk-let (list (list name (mk-lambda params (k rest-env)))) (alternatives (list name))))))

  ;; Each entry with a literal key is looked up. An entry followed by an ellipsis matches the
  ;; entries left over, each as a list of its key and value.
  (define (compile-hash entries v env k fail)
    (define repeated
      (and (>= (length entries) 2) (ellipsis-min (last entries)) (last (all-but-last entries))))
    (define lookups (if repeated (all-but-last (all-but-last entries)) entries))
    (define (key-of entry)
      (define key (cadr entry))
      (unless (or (quoted? key) (not (or (symbol? key) (pair? key))))
        (error! "match: a hash-table key must be a literal" key))
      (if (quoted? key) key (list 'quote key)))
    (for-each (lambda (entry)
                (unless (and (pair? entry) (equal? (car entry) 'entry))
                  (error! "match: bad hash-table pattern" entry)))
              lookups)
    (let loop ([remaining lookups]
               [env env])
      (if (null? remaining)
          (if repeated
              (bind-temp (hash-entries-without v (map key-of lookups))
                         (lambda (t)
                           (compile-list (list (if (pair? repeated)
                                                   (cons 'list (cdr repeated))
                                                   repeated)
                                               (last entries))
                                         (list 'quote '())
                                         t
                                         env
                                         k
                                         fail)))
              (k env))
          (let ([key (key-of (car remaining))])
            (mk-if (list '#%prim.hash-contains? v key)
                   (bind-temp (list '#%prim.hash-ref v key)
                              (lambda (t)
                                (compile-pattern (caddr (car remaining))
                                                 t
                                                 env
                                                 (lambda (env) (loop (cdr remaining) env))
                                                 fail)))
                   fail)))))

  ;; The entries of a hash without the given keys, as lists of key and value. The code is written
  ;; out in place so that phase 1 code can use it.
  (define (hash-entries-without v keys)
    (define entry (gensym))
    (list 'map
          (mk-lambda (list entry) (list 'list (list 'car entry) (list 'cdr entry)))
          (list 'hash->list
                (let loop ([h v]
                           [keys keys])
                  (if (null? keys)
                      h
                      (loop (list 'hash-remove h (car keys)) (cdr keys)))))))

  (define (compile-struct name pats v env k fail)
    (define predicate
      (#%datum->syntax name
                       (string->symbol (string-append (symbol->string (#%syntax->datum name)) "?"))))
    (mk-if (list predicate v)
           (bind-temp (list '#%struct-fields v)
                      (lambda (t) (compile-list pats (list 'quote '()) t env k fail)))
           fail))

  (define (null-pattern? pat)
    (and (quoted? pat) (null? (cadr pat))))

  (define (has-ellipsis? pats)
    (cond
      [(null? pats) #f]
      [(ellipsis-min (car pats)) #t]
      [else (has-ellipsis? (cdr pats))]))

  ;; The elements of a list against `pats`, and what follows them against `tail`
  (define (compile-list pats tail v env k fail)
    (cond
      [(null? pats) (compile-pattern tail v env k fail)]
      [(and (pair? (cdr pats)) (ellipsis-min (cadr pats)))
       (compile-repeat (car pats) (ellipsis-min (cadr pats)) (cddr pats) tail v env k fail)]
      [(ellipsis-min (car pats)) (error! "match: an ellipsis must follow a pattern")]
      [else
       (define a (gensym))
       (define d (gensym))
       (mk-if (list '#%prim.pair? v)
              (mk-let (list (list a (list '#%prim.car v)) (list d (list '#%prim.cdr v)))
                      (compile-pattern (car pats)
                                       a
                                       env
                                       (lambda (env) (compile-list (cdr pats) tail d env k fail))
                                       fail))
              fail)]))

  ;; `pat` repeated at least `least` times, then `rest`
  (define (compile-repeat pat least rest tail v env k fail)
    (if (and (null-pattern? tail) (not (has-ellipsis? rest)))
        (compile-repeat-fixed pat least rest v env k fail)
        (compile-repeat-backtracking pat least rest tail v env k fail)))

  ;; Only the last `(length rest)` elements are left for `rest`
  (define (compile-repeat-fixed pat least rest v env k fail)
    (define n (gensym))
    (define after (length rest))
    (mk-if
     (list '#%prim.list? v)
     (mk-let
      (list (list n (list '#%prim.- (list '#%prim.length v) after)))
      (mk-if (list '#%prim.>= n least)
             (if (null? rest)
                 (compile-each pat v env k fail)
                 (bind-temp (list '#%prim.take v n)
                            (lambda (front)
                              (bind-temp (list '#%prim.list-tail v n)
                                         (lambda (back)
                                           (compile-each pat
                                                         front
                                                         env
                                                         (lambda (env)
                                                           (compile-list rest
                                                                         (list 'quote '())
                                                                         back
                                                                         env
                                                                         k
                                                                         fail))
                                                         fail))))))
             fail))
     fail))

  ;; `pat` against every element of the list `v`, each of its variables bound to the list of what
  ;; it matched
  (define (compile-each pat v env k fail)
    (cond
      [(wildcard? pat) (k env)]
      [(and (symbol? pat) (not (assoc pat env))) (k (cons (cons pat v) env))]
      [else
       (define vars (vars-of pat))
       (define accs (map (lambda (_) (gensym)) vars))
       (define loop (gensym))
       (define xs (gensym))
       (define e (gensym))
       (define results (map (lambda (_) (gensym)) vars))
       (list 'let
             loop
             (cons (list xs v) (map (lambda (acc) (list acc (list 'quote '()))) accs))
             (mk-if (list '#%prim.pair? xs)
                    (bind-temp (list '#%prim.car xs)
                               (lambda (e)
                                 (compile-pattern pat
                                                  e
                                                  '()
                                                  (lambda (inner)
                                                    (cons loop
                                                          (cons (list '#%prim.cdr xs)
                                                                (map (lambda (var acc)
                                                                       (list '#%prim.cons
                                                                             (lookup var inner)
                                                                             acc))
                                                                     vars
                                                                     accs))))
                                                  fail)))
                    (mk-let (map (lambda (r acc) (list r (list '#%prim.reverse acc))) results accs)
                            (k (append (map cons vars results) env)))))]))

  ;; The longest run of elements that match `pat` such that `rest` and `tail` match what follows
  (define (compile-repeat-backtracking pat least rest tail v env k fail)
    (define vars (vars-of pat))
    (define accs (map (lambda (_) (gensym)) vars))
    (define results (map (lambda (_) (gensym)) vars))
    (define try (gensym))
    (define collect (gensym))
    (define xs (gensym))
    (define seen (gensym))
    (define n (gensym))
    (define try-body
      (mk-if (list '#%prim.>= n least)
             (with-thunk (lambda (shorter)
                           (mk-let (map (lambda (r acc) (list r (list '#%prim.reverse acc)))
                                        results
                                        accs)
                                   (compile-list rest
                                                 tail
                                                 xs
                                                 (append (map cons vars results) env)
                                                 k
                                                 shorter)))
                         (mk-if (list '#%prim.pair? seen)
                                (cons try
                                      (cons (list '#%prim.cons (list '#%prim.car seen) xs)
                                            (cons (list '#%prim.cdr seen)
                                                  (cons (list '#%prim.- n 1)
                                                        (map (lambda (acc) (list '#%prim.cdr acc))
                                                             accs)))))
                                fail))
             fail))
    (define (call-try)
      (cons try (cons xs (cons seen (cons n accs)))))
    (list 'letrec
          (list (list try (mk-lambda (cons xs (cons seen (cons n accs))) try-body)))
          (list 'let
                collect
                (cons (list xs v)
                      (cons (list seen (list 'quote '()))
                            (cons (list n 0) (map (lambda (acc) (list acc (list 'quote '()))) accs))))
                (mk-if (list '#%prim.pair? xs)
                       (bind-temp (list '#%prim.car xs)
                                  (lambda (e)
                                    (compile-pattern
                                     pat
                                     e
                                     '()
                                     (lambda (inner)
                                       (cons collect
                                             (cons (list '#%prim.cdr xs)
                                                   (cons (list '#%prim.cons e seen)
                                                         (cons (list '#%prim.+ n 1)
                                                               (map (lambda (var acc)
                                                                      (list '#%prim.cons
                                                                            (lookup var inner)
                                                                            acc))
                                                                    vars
                                                                    accs))))))
                                     (call-try))))
                       (call-try)))))

  (define (stx-list x)
    (let ([e (stx-e x)])
      (if (list? e) e #f)))

  ;; A clause's body, given the variables its pattern bound
  (define (clause-parts clause)
    (define items (stx-list clause))
    (when (or (not items) (null? items) (null? (cdr items)))
      (error! "match: a clause needs a pattern and a body" (datum-of clause)))
    (define pattern (car items))
    (define rest (cdr items))
    (define guard
      (if (and (pair? rest) (equal? (datum-of (car rest)) '#:when) (pair? (cdr rest)))
          (cadr rest)
          #f))
    (define after-guard (if guard (cddr rest) rest))
    (define escape
      (let ([e (and (pair? after-guard) (stx-list (car after-guard)))])
        (if (and e (= (length e) 2) (equal? (datum-of (car e)) '=>))
            (cadr e)
            #f)))
    (define body (if escape (cdr after-guard) after-guard))
    (when (null? body)
      (error! "match: a clause needs a body" (datum-of clause)))
    (list pattern guard escape body))

  ;; `else` as the match module means it, so a local `else` is a pattern variable
  (define (else-clause? clause else-id)
    (let ([items (stx-list clause)])
      (and items
           (pair? items)
           (equal? (datum-of (car items)) 'else)
           (or (not (syntax? (car items))) (free-identifier=? (car items) else-id)))))

  (define (bind-pattern-vars env written body)
    (mk-let (map (lambda (b) (list (written-identifier (car b) written) (cdr b))) (reverse env))
            body))

  ;; The patterns of a clause against each of `inputs`
  (define (compile-clause patterns guard escape body inputs fail)
    (define written
      (let loop ([patterns patterns]
                 [found '()])
        (if (null? patterns)
            found
            (loop (cdr patterns) (pattern-identifiers (car patterns) found)))))
    (define body-expr (cons 'let (cons '() body)))
    (let loop ([patterns patterns]
               [inputs inputs]
               [env '()])
      (if (null? patterns)
          (bind-pattern-vars env
                             written
                             (let* ([body-expr (if escape
                                                   (mk-let (list (list escape (mk-lambda '() fail)))
                                                           body-expr)
                                                   body-expr)])
                               (if guard
                                   (mk-if guard body-expr fail)
                                   body-expr)))
          (compile-pattern (pattern-of (car patterns))
                           (car inputs)
                           env
                           (lambda (env) (loop (cdr patterns) (cdr inputs) env))
                           fail))))

  ;; `split` gives a clause's patterns, one for each input
  (define (compile-clauses clauses inputs split no-match else-id)
    (if (null? clauses)
        no-match
        (let ([clause (car clauses)])
          (if (else-clause? clause else-id)
              (cons 'let (cons '() (cdr (stx-list clause))))
              (let* ([parts (clause-parts clause)]
                     [patterns (split (car parts))]
                     [guard (cadr parts)]
                     [escape (caddr parts)]
                     [body (cadddr parts)]
                     [rest (compile-clauses (cdr clauses) inputs split no-match else-id)])
                (if (or guard escape)
                    ;; The failure is called where the pattern variables are bound
                    (let ([name (gensym)])
                      (mk-let (list (list name (mk-lambda '() rest)))
                              (compile-clause patterns guard escape body inputs (list name))))
                    (with-thunk (lambda (fail) (compile-clause patterns #f #f body inputs fail))
                                rest)))))))

  (define (match-error inputs)
    (list 'error!
          "Unable to match expression: "
          (if (= (length inputs) 1) (car inputs) (cons 'list inputs))
          " to any of the given patterns"))

  ;; `head` is the name of the macro as this module's template wrote it
  (define (compile-match head exprs clauses split syntax-mode)
    (define inputs (map (lambda (_) (gensym)) exprs))
    (set! *syntax-mode* syntax-mode)
    (mk-let (map list inputs exprs)
            (compile-clauses clauses
                             inputs
                             split
                             (match-error inputs)
                             (#%datum->syntax head 'else))))

  (define (written-identifier name identifiers)
    (cond
      [(null? identifiers) name]
      [(equal? (#%syntax->datum (car identifiers)) name) (car identifiers)]
      [else (written-identifier name (cdr identifiers))])))


;; ----------------- match --------------------

;; The procedural macros are behind syntax-rules ones, which phase 1 code can use too
(defmacro (match-clauses expression)
          (define items (stx-list expression))
          (when (< (length items) 2)
            (error! "match: expects an expression and clauses"))
          (syntax/loc (compile-match (car items) (list (cadr items)) (cddr items) list #f)
            (syntax-span expression)))

(defmacro (match*-clauses expression)
          (define items (stx-list expression))
          (when (< (length items) 2)
            (error! "match*: expects a list of expressions and clauses"))
          (define exprs (stx-list (cadr items)))
          (syntax/loc
              (compile-match (car items)
                             exprs
                             (cddr items)
                             (lambda (patterns)
                               (let ([split (stx-list patterns)])
                                 (unless (and split (= (length split) (length exprs)))
                                   (error! "match*: expected a pattern for each expression"
                                           (datum-of patterns)))
                                 split))
                             #f)
            (syntax-span expression)))

;; Defines the pattern variables where the form is written
(defmacro (match-define-pattern expression)
          (define items (stx-list expression))
          (unless (= (length items) 3)
            (error! "match-define: expects a pattern and an expression"))
          (define pattern (cadr items))
          (define input (gensym))
          (define vars (vars-of (pattern-of pattern)))
          (define written (pattern-identifiers pattern '()))
          (define fail
            (list 'error-with-span
                  (list 'quote (syntax-span pattern))
                  "Unable to match the given expression: "
                  input
                  "to any of the patterns"))
          (set! *syntax-mode* #f)
          (syntax/loc
              (list 'define-values
                    (map (lambda (var) (written-identifier var written)) vars)
                    (mk-let (list (list input (caddr items)))
                            (with-thunk (lambda (fail)
                                          (compile-pattern (pattern-of pattern)
                                                           input
                                                           '()
                                                           (lambda (env)
                                                             (cons 'list
                                                                   (map (lambda (var)
                                                                          (lookup var env))
                                                                        vars)))
                                                           fail))
                                        fail)))
            (syntax-span expression)))

;; The syntax's contents are matched, and pattern variables are bound to syntax
(defmacro (match-syntax-clauses expression)
          (define items (stx-list expression))
          (when (< (length items) 2)
            (error! "match-syntax: expects an expression and clauses"))
          (syntax/loc (compile-match (car items) (list (cadr items)) (cddr items) list #t)
            (syntax-span expression)))

(define-syntax match
  (syntax-rules ()
    [(_ expr clause ...) (match-clauses expr clause ...)]))

(define-syntax match*
  (syntax-rules ()
    [(_ exprs clause ...) (match*-clauses exprs clause ...)]))

(define-syntax match-define
  (syntax-rules ()
    [(_ pattern expr) (match-define-pattern pattern expr)]))

(define-syntax match-lambda
  (syntax-rules ()
    [(_ clause ...) (lambda (x) (match x clause ...))]))

(define-syntax match-lambda*
  (syntax-rules ()
    [(_ clause ...) (lambda args (match args clause ...))]))

(define-syntax match-let
  (syntax-rules ()
    [(_ ([pat expr] ...) body ...) (match* (expr ...) [(pat ...) (let () body ...)])]))

(define-syntax match-let*
  (syntax-rules ()
    [(_ () body ...) (let () body ...)]
    [(_ ([pat expr] rest ...) body ...) (match expr [pat (match-let* (rest ...) body ...)])]))

(define-syntax match-syntax
  (syntax-rules ()
    [(_ expr clause ...) (match-syntax-clauses expr clause ...)]))
