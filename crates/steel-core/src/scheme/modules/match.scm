(provide match
         match*
         match-define
         match-lambda
         match-lambda*
         match-let
         match-let*
         match-syntax)

(begin-for-syntax
  ;; A pattern keyword, as written or under the prefix of the module whose template wrote it
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

  ;; These take syntax or plain data, since the quoted parts of syntax can be data
  (define (stx-e x)
    (if (syntax? x) (syntax-e x) x))

  (define (datum-of x)
    (if (syntax? x) (#%syntax->datum x) x))

  (define (stx-list x)
    (let ([e (stx-e x)])
      (if (list? e) e #f)))

  (define (as-identifier x stx)
    (if (syntax? x) x (#%datum->syntax stx (datum-of x))))

  (define (pattern-error stx message)
    (if (syntax? stx)
        (error-with-span (syntax-span stx) message (datum-of stx))
        (error! message (datum-of stx))))

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

  (define (keyword-symbol? x)
    (and (symbol? x) (starts-with? (symbol->string x) "#:")))

  ;; A variable's key. Identifiers that `bound-identifier=?` tells apart get different keys, and
  ;; `table` maps each key back to its identifier.
  (define (variable-key stx name table)
    (let loop ([entries (mutable-vector->list table)])
      (cond
        [(null? entries)
         (let ([key (string->symbol (string-append (symbol->string (gensym)) "-" (symbol->string name)))])
           (vector-push! table (cons key stx))
           key)]
        [(let ([other (cdr (car entries))])
           (if (and (syntax? other) (syntax? stx))
               (bound-identifier=? other stx)
               (equal? (datum-of other) name)))
         (car (car entries))]
        [else (loop (cdr entries))])))

  (define (key-identifier key table)
    (let loop ([entries (mutable-vector->list table)])
      (cond
        [(null? entries) key]
        [(equal? (car (car entries)) key) (cdr (car entries))]
        [else (loop (cdr entries))])))

  ;; The elements of a list pattern. An ellipsis has to come after a pattern.
  (define (pattern-elements items table stx)
    (let loop ([items items]
               [previous #f])
      (cond
        [(null? items) '()]
        [(ellipsis-min (datum-of (car items)))
         (when (or (not previous) (ellipsis-min previous))
           (pattern-error stx "match: an ellipsis must follow a pattern"))
         (cons (datum-of (car items)) (loop (cdr items) (datum-of (car items))))]
        [else
         (let ([pattern (pattern-of (car items) table)])
           (cons pattern (loop (cdr items) pattern)))])))

  ;; A pattern as data, with its variables as keys. The expressions in it (predicates, functions,
  ;; values and struct names) stay syntax.
  (define (pattern-of stx table)
    (define e (stx-e stx))
    (cond
      [(symbol? e)
       (cond
         [(wildcard? e) e]
         [(ellipsis-min e) (pattern-error stx "match: an ellipsis must follow a pattern in a list")]
         [(keyword-symbol? e) (list 'quote e)]
         [else (variable-key stx e table)])]
      [(null? e) (list 'quote '())]
      [(list? e)
       (define head (datum-of (car e)))
       (define keyword (head-keyword head))
       (define args (cdr e))
       (cond
         [(equal? head 'quote) (datum-of stx)]
         [(equal? head 'quasiquote)
          (unless (= (length args) 1)
            (pattern-error stx "match: bad quasiquote pattern"))
          (pattern-of-quasi (car args) table)]
         [(equal? keyword '?)
          (when (null? args)
            (pattern-error stx "match: ? needs a predicate"))
          (cons '? (cons (car args) (map (lambda (p) (pattern-of p table)) (cdr args))))]
         [(equal? keyword 'app)
          (when (null? args)
            (pattern-error stx "match: app needs a function"))
          (cons 'app (cons (car args) (map (lambda (p) (pattern-of p table)) (cdr args))))]
         [(equal? keyword '==)
          (unless (or (= (length args) 1) (= (length args) 2))
            (pattern-error stx "match: == takes a value and an optional comparison"))
          (cons '== args)]
         [(equal? keyword 'hash-table) (cons 'hash-table (hash-entries-of args table stx))]
         [(equal? keyword 'struct)
          (unless (and (= (length args) 2) (symbol? (datum-of (car args))) (stx-list (cadr args)))
            (pattern-error stx "match: a struct pattern is (struct name (pattern ...))"))
          (struct-pattern (as-identifier (car args) stx) (stx-list (cadr args)) table stx)]
         [(equal? keyword 'cons)
          (unless (= (length args) 2)
            (pattern-error stx "match: cons takes two patterns"))
          (cons 'cons (map (lambda (p) (pattern-of p table)) args))]
         [(equal? keyword 'box)
          (unless (= (length args) 1)
            (pattern-error stx "match: box takes one pattern"))
          (cons 'box (map (lambda (p) (pattern-of p table)) args))]
         [(or (equal? keyword 'list-rest) (equal? keyword 'list*))
          (when (or (null? args) (ellipsis-min (datum-of (last args))))
            (pattern-error stx "match: list-rest needs a pattern for the rest of the list"))
          (cons 'list-rest (pattern-elements args table stx))]
         [(or (equal? keyword 'list) (equal? keyword 'vector))
          (cons keyword (pattern-elements args table stx))]
         [(equal? keyword 'or)
          (let ([alts (map (lambda (p) (pattern-of p table)) args)])
            (unless (or (null? alts)
                        (let ([vars (vars-of (car alts))])
                          (let loop ([alts (cdr alts)])
                            (or (null? alts) (and (same-vars? vars (vars-of (car alts))) (loop (cdr alts)))))))
              (pattern-error stx "match: variable not bound in all or patterns"))
            (cons 'or alts))]
         [(or (equal? keyword 'and) (equal? keyword 'not))
          (cons keyword (map (lambda (p) (pattern-of p table)) args))]
         [(symbol? head)
          (struct-pattern (as-identifier (car e) stx) args table stx)]
         [else (pattern-error stx "match: bad pattern")])]
      [(or (pair? e) (vector? e)) (pattern-error stx "match: bad pattern")]
      [else (datum-of stx)]))

  (define (struct-pattern name fields table stx)
    (when (has-ellipsis? (map datum-of fields))
      (pattern-error stx "match: an ellipsis can't be used in a struct pattern"))
    (let ([info (#%struct-info name)])
      (when (and info (not (= (length fields) (length (cadr info)))))
        (pattern-error stx
                       (string-append "match: "
                                      (symbol->string (car info))
                                      " has "
                                      (number->string (length (cadr info)))
                                      " fields, but the pattern has "
                                      (number->string (length fields))))))
    (cons name (map (lambda (p) (pattern-of p table)) fields)))

  (define (hash-entries-of entries table stx)
    (let loop ([entries entries]
               [previous #f])
      (cond
        [(null? entries) '()]
        [(ellipsis-min (datum-of (car entries)))
         (unless (and previous (null? (cdr entries)))
           (pattern-error stx "match: an ellipsis in a hash-table pattern must follow its last entry"))
         (list (datum-of (car entries)))]
        [else
         (let* ([e (stx-e (car entries))]
                [entry (cond
                         [(and (list? e) (= (length e) 2))
                          (list 'entry (pattern-of (car e) table) (pattern-of (cadr e) table))]
                         [(and (symbol? e) (pair? (cdr entries)) (ellipsis-min (datum-of (cadr entries))))
                          (pattern-of (car entries) table)]
                         [else (pattern-error (car entries) "match: bad hash-table entry")])])
           (cons entry (loop (cdr entries) entry)))])))

  (define (unquote-form? x names)
    (let ([e (stx-e x)])
      (and (list? e) (= (length e) 2) (member (datum-of (car e)) names))))

  (define (pattern-of-quasi stx table)
    (define e (stx-e stx))
    (cond
      [(unquote-form? stx unquote-names) (pattern-of (cadr e) table)]
      [(null? e) (list 'quote '())]
      [(list? e) (quasi-list e table stx)]
      [(vector? e) (cons 'vector (cdr (quasi-items (vector->list e) table stx #f)))]
      [(pair? e)
       (let loop ([items e]
                  [front '()])
         (if (pair? items)
             (loop (cdr items) (cons (car items) front))
             (cons 'list-rest
                   (append (cdr (quasi-items (reverse front) table stx #f))
                           (list (list 'quote (datum-of items)))))))]
      [else (list 'quote (datum-of stx))]))

  ;; The reader turns `(a . ,b)` into `(a unquote b)`, so an unquote second to last is the tail
  (define (quasi-list items table stx)
    (let loop ([rest items]
               [front '()])
      (cond
        [(null? rest) (quasi-items items table stx #t)]
        [(and (member (datum-of (car rest)) unquote-names) (pair? (cdr rest)) (null? (cddr rest)))
         (cons 'list-rest
               (append (cdr (quasi-items (reverse front) table stx #f))
                       (list (pattern-of (cadr rest) table))))]
        [else (loop (cdr rest) (cons (car rest) front))])))

  ;; As in Racket, an ellipsis that doesn't follow a pattern, or follows a `,@`, is a literal. A
  ;; `,@` of a pattern that isn't a list pattern is the rest of the list, when it comes last.
  (define (quasi-items items table stx last-tail?)
    (let loop ([items items]
               [previous #f]
               [done '()])
      (cond
        [(null? items) (cons 'list (reverse done))]
        [(ellipsis-min (datum-of (car items)))
         (if (equal? previous 'pattern)
             (loop (cdr items) 'ellipsis (cons (datum-of (car items)) done))
             (loop (cdr items) 'pattern (cons (list 'quote (datum-of (car items))) done)))]
        [(unquote-form? (car items) unquote-splicing-names)
         (let ([spliced (pattern-of (cadr (stx-e (car items))) table)])
           (cond
             [(symbol? spliced) (loop (cdr items) 'splice (cons '... (cons spliced done)))]
             [(and (pair? spliced) (equal? (car spliced) 'list))
              (loop (cdr items) 'splice (append (reverse (cdr spliced)) done))]
             [(and (quoted? spliced) (list? (cadr spliced)))
              (loop (cdr items)
                    'splice
                    (append (reverse (map (lambda (x) (list 'quote x)) (cadr spliced))) done))]
             [(and last-tail? (null? (cdr items)))
              (if (and (pair? spliced) (equal? (car spliced) 'list-rest))
                  (cons 'list-rest (append (reverse done) (cdr spliced)))
                  (cons 'list-rest (append (reverse done) (list spliced))))]
             [else (pattern-error (car items) "match: ,@ needs a list pattern here")]))]
        [else
         (loop (cdr items) 'pattern (cons (pattern-of-quasi (car items) table) done))])))

  (define (has-ellipsis? pats)
    (cond
      [(null? pats) #f]
      [(ellipsis-min (car pats)) #t]
      [else (has-ellipsis? (cdr pats))]))

  (define (pattern-vars pat found)
    (cond
      [(symbol? pat)
       (if (or (wildcard? pat) (ellipsis-min pat) (member pat found)) found (cons pat found))]
      [(or (not (pair? pat)) (quoted? pat)) found]
      [(syntax? (car pat)) (pattern-vars-of (cdr pat) found)]
      [(or (equal? (car pat) '?) (equal? (car pat) 'app)) (pattern-vars-of (cddr pat) found)]
      [(or (equal? (car pat) '==) (equal? (car pat) 'not)) found]
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

  (define (count-calls expr name)
    (cond
      [(and (pair? expr) (equal? (car expr) name) (null? (cdr expr))) 1]
      [(list? expr) (apply + (map (lambda (x) (count-calls x name)) expr))]
      [else 0]))

  (define (replace-call expr name body)
    (cond
      [(and (pair? expr) (equal? (car expr) name) (null? (cdr expr))) body]
      [(list? expr) (map (lambda (x) (replace-call x name body)) expr)]
      [else expr]))

  ;; Calls `make` with a call to a thunk that runs `body`. If the code calls it once, `body`
  ;; replaces the call. Failures only happen where nothing but the matcher's temporaries are
  ;; bound, so `body` sees the same bindings either way.
  (define (with-thunk make body)
    (define name (gensym))
    (define expr (make (list name)))
    (define uses (count-calls expr name))
    (cond
      [(= uses 0) expr]
      [(= uses 1) (replace-call expr name body)]
      [else (mk-let (list (list name (mk-lambda '() body))) expr)]))

  ;; Besides the variables bound so far, the environment can hold `#%syntax` for match-syntax,
  ;; `#%guarded` for a clause with a guard or failure name, and `#%fail` for what a failed guard
  ;; calls instead of the clause's failure
  (define flags '(#%syntax #%guarded #%fail))

  (define (lookup var env)
    (let ([found (assoc var env)])
      (if found (cdr found) #f)))

  ;; In match-syntax, compound patterns look inside syntax with syntax-e, literals are compared
  ;; with syntax->datum, and variables bind the syntax itself
  (define (inspect v env make)
    (if (lookup '#%syntax env)
        (bind-temp (mk-if (list 'syntax? v) (list 'syntax-e v) v) make)
        (make v)))

  ;; In match-syntax the rest of a list is a plain list of syntax objects
  (define (inspect-datum v env make)
    (if (lookup '#%syntax env)
        (let ([e (gensym)])
          (bind-temp (mk-if (list 'syntax? v)
                            (list 'syntax->datum v)
                            (mk-if (list 'list? v)
                                   (list 'map
                                         (mk-lambda (list e)
                                                    (mk-if (list 'syntax? e)
                                                           (list 'syntax->datum e)
                                                           e))
                                         v)
                                   v))
                     make))
        (make v)))

  (define (bind-temp expr make)
    (define t (gensym))
    (mk-let (list (list t expr)) (make t)))

  (define (without-vars vars env)
    (cond
      [(null? env) '()]
      [(member (car (car env)) vars) (without-vars vars (cdr env))]
      [else (cons (car env) (without-vars vars (cdr env)))]))

  ;; The code that matches `pat` against the variable `v`. `k` makes the code for a match from the
  ;; variables bound so far, which are bound to temporaries until the whole pattern has matched.
  ;; `fail` is the code for a failed match.
  (define (compile-pattern pat v env k fail)
    (cond
      [(symbol? pat)
       (cond
         [(wildcard? pat) (k env)]
         [(assoc pat env) (mk-if (list '#%prim.equal? v (lookup pat env)) (k env) fail)]
         [else (k (cons (cons pat v) env))])]
      [(quoted? pat)
       (inspect-datum v
                      env
                      (lambda (v)
                        (if (null-pattern? pat)
                            (mk-if (list '#%prim.null? v) (k env) fail)
                            (mk-if (list '#%prim.equal? v pat) (k env) fail))))]
      [(pair? pat) (compile-compound pat v env k fail)]
      [else
       (inspect-datum v
                      env
                      (lambda (v) (mk-if (list '#%prim.equal? v (list 'quote pat)) (k env) fail)))]))

  ;; Matches every pattern against `v`
  (define (compile-and pats v env k fail)
    (if (null? pats)
        (k env)
        (compile-pattern (car pats)
                         v
                         env
                         (lambda (env) (compile-and (cdr pats) v env k fail))
                         fail)))

  ;; Matches each pattern against the variable in the same position in `vs`
  (define (compile-pairwise pats vs env k fail)
    (if (null? pats)
        (k env)
        (compile-pattern (car pats)
                         (car vs)
                         env
                         (lambda (env) (compile-pairwise (cdr pats) (cdr vs) env k fail))
                         fail)))

  (define (compile-compound pat v env k fail)
    (define head (car pat))
    (define args (cdr pat))
    (cond
      [(syntax? head) (inspect v env (lambda (v) (compile-struct head args v env k fail)))]
      [(equal? head 'list)
       (inspect v env (lambda (v) (compile-list args (list 'quote '()) v env k fail)))]
      [(equal? head 'list-rest)
       (inspect v env (lambda (v) (compile-list (all-but-last args) (last args) v env k fail)))]
      [(equal? head 'cons)
       (inspect v env (lambda (v) (compile-list (list (car args)) (cadr args) v env k fail)))]
      [(equal? head 'vector)
       (inspect v
                env
                (lambda (v)
                  (mk-if (list '#%prim.vector? v)
                         (bind-temp (list 'vector->list v)
                                    (lambda (t)
                                      (compile-list args (list 'quote '()) t env k fail)))
                         fail)))]
      [(equal? head 'box)
       (inspect v
                env
                (lambda (v)
                  (mk-if (list 'box? v)
                         (bind-temp (list 'unbox v)
                                    (lambda (t) (compile-pattern (car args) t env k fail)))
                         fail)))]
      [(equal? head 'hash-table)
       (inspect v
                env
                (lambda (v)
                  (mk-if (list '#%prim.hash? v) (compile-hash args v env k fail) fail)))]
      [(equal? head '?)
       (mk-if (list (car args) v) (compile-and (cdr args) v env k fail) fail)]
      [(equal? head 'app)
       (if (> (length args) 2)
           (let ([results (map (lambda (_) (gensym)) (cdr args))])
             (list 'call-with-values
                   (mk-lambda '() (list (car args) v))
                   (mk-lambda results (compile-pairwise (cdr args) results env k fail))))
           (bind-temp (list (car args) v) (lambda (t) (compile-and (cdr args) t env k fail))))]
      [(equal? head '==)
       (inspect-datum v
                      env
                      (lambda (v)
                        (mk-if (if (null? (cdr args))
                                   (list '#%prim.equal? v (car args))
                                   (list (cadr args) (car args) v))
                               (k env)
                               fail)))]
      [(equal? head 'and) (compile-and args v env k fail)]
      [(equal? head 'not)
       (with-thunk (lambda (matched)
                     (let loop ([pats args])
                       (if (null? pats)
                           matched
                           (with-thunk (lambda (next)
                                         (compile-pattern (car pats) v env (lambda (_) fail) next))
                                       (loop (cdr pats))))))
                   (k env))]
      [else (compile-or args v env k fail)]))

  (define (same-vars? a b)
    (and (= (length a) (length b))
         (let loop ([a a])
           (or (null? a) (and (member (car a) b) (loop (cdr a)))))))

  (define (unbound-vars vars env)
    (cond
      [(null? vars) '()]
      [(assoc (car vars) env) (unbound-vars (cdr vars) env)]
      [else (cons (car vars) (unbound-vars (cdr vars) env))]))

  ;; Every alternative jumps to the same continuation, passing the variables it binds
  (define (compile-or alts v env k fail)
    (define vars (if (null? alts) '() (vars-of (car alts))))
    (define new-vars (unbound-vars vars env))
    (define params (map (lambda (_) (gensym)) new-vars))
    (define (alternatives jump)
      (let loop ([alts alts])
        (if (null? alts)
            fail
            (with-thunk (lambda (next)
                          (compile-pattern (car alts)
                                           v
                                           env
                                           (lambda (env)
                                             (cons (car jump)
                                                   (map (lambda (var) (lookup var env)) new-vars)))
                                           next))
                        (loop (cdr alts))))))
    (define rest-env (append (map cons new-vars params) env))
    (if (null? new-vars)
        (with-thunk alternatives (k env))
        (let ([name (gensym)])
          (mk-let (list (list name (mk-lambda params (k rest-env)))) (alternatives (list name))))))

  (define (literal-key? key)
    (or (quoted? key) (not (or (symbol? key) (pair? key)))))

  (define (entries-where entries literal)
    (cond
      [(null? entries) '()]
      [(equal? (literal-key? (cadr (car entries))) literal)
       (cons (car entries) (entries-where (cdr entries) literal))]
      [else (entries-where (cdr entries) literal)]))

  ;; Entries with a literal key are looked up, and the others are matched against the entries left
  ;; over, each against a different one. An entry followed by an ellipsis matches what's left
  ;; after that, each as a list of its key and value. As in Racket, other entries are only allowed
  ;; when every key is literal or there's an ellipsis.
  (define (compile-hash entries v env k fail)
    (define repeated
      (and (>= (length entries) 2) (ellipsis-min (last entries)) (last (all-but-last entries))))
    (define given (if repeated (all-but-last (all-but-last entries)) entries))
    (define lookups (entries-where given #t))
    (define searched (entries-where given #f))
    (define keys
      (map (lambda (entry)
             (let ([key (cadr entry)])
               (if (quoted? key) key (list 'quote key))))
           lookups))
    (define (match-rest rest env)
      (if repeated
          (compile-list (list (if (pair? repeated) (cons 'list (cdr repeated)) repeated)
                              (last entries))
                        (list 'quote '())
                        rest
                        env
                        k
                        fail)
          (mk-if (list '#%prim.null? rest) (k env) fail)))
    (if (null? entries)
        (mk-if (list '#%prim.= (list 'hash-length v) 0) (k env) fail)
        (let loop ([remaining lookups]
                   [keys-left keys]
                   [env env])
          (if (null? remaining)
              (if (or repeated (pair? searched))
                  (bind-temp (hash-entries-without v keys)
                             (lambda (t) (compile-search searched t env match-rest fail)))
                  (k env))
              (let ([key (car keys-left)])
                (mk-if (list '#%prim.hash-contains? v key)
                       (bind-temp (list '#%prim.hash-ref v key)
                                  (lambda (t)
                                    (compile-pattern (caddr (car remaining))
                                                     t
                                                     env
                                                     (lambda (env)
                                                       (loop (cdr remaining) (cdr keys-left) env))
                                                     fail)))
                       fail))))))

  ;; Matches each entry pattern against a different entry in `xs`, trying them in turn. `k` gets
  ;; the entries none of them matched.
  (define (compile-search entries xs env k fail)
    (if (null? entries)
        (k xs env)
        (let ([loop (gensym)]
              [ys (gensym)]
              [skipped (gensym)])
          (list 'let
                loop
                (list (list ys xs) (list skipped (list 'quote '())))
                (mk-if (list '#%prim.pair? ys)
                       (bind-temp (list '#%prim.car ys)
                                  (lambda (e)
                                    (with-thunk
                                     (lambda (next)
                                       (compile-pattern
                                        (cons 'list (cdr (car entries)))
                                        e
                                        env
                                        (lambda (env)
                                          (bind-temp (list 'append skipped (list '#%prim.cdr ys))
                                                     (lambda (rest)
                                                       (compile-search (cdr entries)
                                                                       rest
                                                                       env
                                                                       k
                                                                       next))))
                                        next))
                                     (list loop
                                           (list '#%prim.cdr ys)
                                           (list '#%prim.cons e skipped)))))
                       fail)))))

  ;; The entries of a hash without the given keys, as lists of key and value. The code is written
  ;; out in place, since phase 1 code can't call this module's functions.
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

  ;; With the struct's info, its predicate and accessors are named as where it's defined, so a
  ;; local binding can't hide them. Without it, `name?` is looked up where the match is, and
  ;; #%struct-fields raises an error when the pattern has the wrong number of fields.
  (define (compile-struct name pats v env k fail)
    (define info (#%struct-info name))
    (if info
        (let* ([prefix (string-append (list-ref info 3) (symbol->string (car info)))]
               [named (lambda (suffix) (string->symbol (string-append prefix suffix)))]
               [fields (map (lambda (_) (gensym)) pats)])
          (mk-if (list (named "?") v)
                 (mk-let (map (lambda (t field)
                                (list t (list (named (string-append "-" (symbol->string field))) v)))
                              fields
                              (cadr info))
                         (compile-pairwise pats fields env k fail))
                 fail))
        (let ([predicate (#%datum->syntax
                          name
                          (string->symbol (string-append (symbol->string (#%syntax->datum name)) "?")))])
          (mk-if (list predicate v)
                 (bind-temp (list '#%struct-fields v (length pats))
                            (lambda (t) (compile-list pats (list 'quote '()) t env k fail)))
                 fail))))

  (define (null-pattern? pat)
    (and (quoted? pat) (null? (cadr pat))))

  ;; Matches the elements of a list against `pats`, and the rest of it against `tail`
  (define (compile-list pats tail v env k fail)
    (cond
      [(null? pats) (compile-pattern tail v env k fail)]
      [(and (pair? (cdr pats)) (ellipsis-min (cadr pats)))
       (compile-repeat (car pats) (ellipsis-min (cadr pats)) (cddr pats) tail v env k fail)]
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

  ;; Matches `pat` at least `least` times, followed by `rest`
  (define (compile-repeat pat least rest tail v env k fail)
    (if (and (null-pattern? tail) (not (has-ellipsis? rest)))
        (compile-repeat-fixed pat least rest v env k fail)
        (compile-repeat-backtracking pat least rest tail v env k fail)))

  ;; When the list ends after `rest`, which has no ellipsis, its last `(length rest)` elements
  ;; are left for `rest`
  (define (compile-repeat-fixed pat least rest v env k fail)
    (define after (length rest))
    (define (each front env k)
      (compile-ellipsis pat front env k fail))
    (mk-if
     (list '#%prim.list? v)
     (cond
       [(and (= after 0) (= least 0)) (each v env k)]
       [(= after 0)
        (mk-if (list '#%prim.>= (list '#%prim.length v) least) (each v env k) fail)]
       [else
        (define n (gensym))
        (mk-let (list (list n (list '#%prim.- (list '#%prim.length v) after)))
                (mk-if (list '#%prim.>= n least)
                       (bind-temp
                        (list '#%prim.take v n)
                        (lambda (front)
                          (bind-temp (list '#%prim.list-tail v n)
                                     (lambda (back)
                                       (each front
                                             env
                                             (lambda (env)
                                               (compile-list rest
                                                             (list 'quote '())
                                                             back
                                                             env
                                                             k
                                                             fail)))))))
                       fail))])
     fail))

  ;; The variables an ellipsis bound, as lists. One already bound outside it must be equal.
  (define (merge-repeated vars results env k fail)
    (cond
      [(null? vars) (k env)]
      [(assoc (car vars) env)
       (mk-if (list '#%prim.equal? (lookup (car vars) env) (car results))
              (merge-repeated (cdr vars) (cdr results) env k fail)
              fail)]
      [else
       (merge-repeated (cdr vars) (cdr results) (cons (cons (car vars) (car results)) env) k fail)]))

  (define (reverse-bindings results accs)
    (map (lambda (r acc) (list r (list '#%prim.reverse acc))) results accs))

  ;; An element of an ellipsis sees the variables bound outside it, apart from its own
  (define (element-env pat env)
    (without-vars (vars-of pat) env))

  ;; Matches `pat` against every element of the list `v`. Each of its variables is bound to the
  ;; list of what it matched.
  (define (compile-ellipsis pat v env k fail)
    (cond
      [(wildcard? pat) (k env)]
      [(and (symbol? pat) (not (assoc pat env))) (k (cons (cons pat v) env))]
      [else
       (define vars (vars-of pat))
       (define accs (map (lambda (_) (gensym)) vars))
       (define loop (gensym))
       (define xs (gensym))
       (define results (map (lambda (_) (gensym)) vars))
       (list 'let
             loop
             (cons (list xs v) (map (lambda (acc) (list acc (list 'quote '()))) accs))
             (mk-if (list '#%prim.pair? xs)
                    (bind-temp (list '#%prim.car xs)
                               (lambda (e)
                                 (compile-pattern pat
                                                  e
                                                  (element-env pat env)
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
                    (mk-let (reverse-bindings results accs)
                            (merge-repeated vars results env k fail))))]))

  ;; The longest run of elements that match `pat` such that `rest` and `tail` match what follows.
  ;; A failed guard tries a shorter run too.
  (define (compile-repeat-backtracking pat least rest tail v env k fail)
    (define vars (vars-of pat))
    (define accs (map (lambda (_) (gensym)) vars))
    (define results (map (lambda (_) (gensym)) vars))
    (define try (gensym))
    (define collect (gensym))
    (define xs (gensym))
    (define seen (gensym))
    (define n (gensym))
    (define (matched env shorter)
      (if (lookup '#%guarded env)
          (let ([retry (gensym)])
            (mk-let (list (list retry (mk-lambda '() shorter)))
                    (k (cons (cons '#%fail (list retry)) env))))
          (k env)))
    (define try-body
      (mk-if (list '#%prim.>= n least)
             (with-thunk (lambda (shorter)
                           ;; The run is only reversed once what follows it has matched
                           (compile-list rest
                                         tail
                                         xs
                                         env
                                         (lambda (env)
                                           (mk-let (reverse-bindings results accs)
                                                   (merge-repeated vars
                                                                   results
                                                                   env
                                                                   (lambda (env) (matched env shorter))
                                                                   shorter)))
                                         shorter))
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
                                     (element-env pat env)
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

  (define (mk-body body)
    (cons 'let (cons '() body)))

  (define (clause-body clause body)
    (when (null? body)
      (pattern-error clause "match: a clause needs a body"))
    body)

  ;; Calls `k` with the clause's pattern, guard, failure name and body
  (define (with-clause-parts clause k)
    (define items (stx-list clause))
    (when (or (not items) (null? items))
      (pattern-error clause "match: a clause needs a pattern and a body"))
    (define rest (cdr items))
    (define has-guard (and (pair? rest) (equal? (datum-of (car rest)) '#:when)))
    (when (and has-guard (null? (cdr rest)))
      (pattern-error clause "match: #:when needs an expression"))
    (define after-guard (if has-guard (cddr rest) rest))
    (define escape
      (let ([e (and (pair? after-guard) (stx-list (car after-guard)))])
        (cond
          [(not (and e (pair? e) (equal? (datum-of (car e)) '=>))) #f]
          [(and (= (length e) 2) (symbol? (datum-of (cadr e)))) (cadr e)]
          [else (pattern-error (car after-guard) "match: (=> name) needs one identifier")])))
    (k (car items)
       (if has-guard (cadr rest) #f)
       escape
       (clause-body clause (if escape (cdr after-guard) after-guard))))

  ;; Whether the clause is an `else` clause. A locally bound `else` is a pattern variable instead.
  (define (else-clause? clause else-id)
    (let ([items (stx-list clause)])
      (and items
           (pair? items)
           (equal? (datum-of (car items)) 'else)
           (or (not (syntax? (car items))) (free-identifier=? (car items) else-id)))))

  (define (bind-pattern-vars env table body)
    (let loop ([env (reverse env)]
               [seen '()]
               [bindings '()])
      (cond
        [(null? env) (mk-let (reverse bindings) body)]
        [(or (member (car (car env)) seen) (member (car (car env)) flags))
         (loop (cdr env) seen bindings)]
        [else
         (loop (cdr env)
               (cons (car (car env)) seen)
               (cons (list (key-identifier (car (car env)) table) (cdr (car env))) bindings))])))

  ;; Matches the patterns of a clause against each of `inputs`
  (define (compile-clause patterns guard escape body inputs start fail)
    (define table (mutable-vector))
    (define converted (map (lambda (p) (pattern-of p table)) patterns))
    (compile-pairwise
     converted
     inputs
     (if (or guard escape) (cons (cons '#%guarded #t) start) start)
     (lambda (env)
       (let ([fail (or (lookup '#%fail env) fail)])
         (bind-pattern-vars env
                            table
                            (let ([body-expr (if escape
                                                 (mk-let (list (list escape (mk-lambda '() fail)))
                                                         (mk-body body))
                                                 (mk-body body))])
                              (if guard
                                  (mk-if guard body-expr fail)
                                  body-expr)))))
     fail))

  ;; Each clause's failure is a thunk running the clauses after it. The failure only runs where
  ;; the matcher's temporaries are bound, unless the clause has a guard or a failure name.
  (define (compile-clauses clauses inputs split start no-match else-id)
    (if (null? clauses)
        no-match
        (let ([clause (car clauses)])
          (if (else-clause? clause else-id)
              (mk-body (clause-body clause (cdr (stx-list clause))))
              (with-clause-parts
               clause
               (lambda (pattern guard escape body)
                 (let* ([rest (compile-clauses (cdr clauses) inputs split start no-match else-id)]
                        [name (gensym)]
                        [code (compile-clause (split pattern)
                                              guard
                                              escape
                                              body
                                              inputs
                                              start
                                              (list name))]
                        [uses (count-calls code name)])
                   (cond
                     ;; Clauses after one that can't fail are still compiled, for their errors
                     [(and (= uses 0) (null? (cdr clauses))) code]
                     [(and (= uses 1) (not guard) (not escape)) (replace-call code name rest)]
                     [else (mk-let (list (list name (mk-lambda '() rest))) code)]))))))))

  (define (match-error inputs)
    (list 'error!
          "Unable to match expression: "
          (if (= (length inputs) 1) (car inputs) (cons 'list inputs))
          " to any of the given patterns"))

  ;; `head` is the name of the macro as this module's template wrote it
  (define (compile-match head exprs clauses split syntax-mode)
    (let ([inputs (map (lambda (_) (gensym)) exprs)])
      (mk-let (map list inputs exprs)
              (compile-clauses clauses
                               inputs
                               split
                               (if syntax-mode (list (cons '#%syntax #t)) '())
                               (match-error inputs)
                               (#%datum->syntax head 'else))))))

;; ----------------- match --------------------

;; Phase 1 code only finds prelude macros written with syntax-rules, so each of these
;; procedural macros has a syntax-rules macro in front of it
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
          (unless exprs
            (error! "match*: expects a list of expressions" (datum-of (cadr items))))
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

(defmacro (match-define-pattern expression)
          (define items (stx-list expression))
          (unless (= (length items) 3)
            (error! "match-define: expects a pattern and an expression"))
          (let* ([pattern (cadr items)]
                 [input (gensym)]
                 [table (mutable-vector)]
                 [converted (pattern-of pattern table)]
                 [vars (vars-of converted)]
                 [fail (list 'error-with-span
                             (list 'quote (syntax-span pattern))
                             "Unable to match the given expression: "
                             input
                             "to any of the patterns")])
            (syntax/loc
                (list 'define-values
                      (map (lambda (var) (key-identifier var table)) vars)
                      (mk-let (list (list input (caddr items)))
                              (with-thunk (lambda (fail)
                                            (compile-pattern converted
                                                             input
                                                             '()
                                                             (lambda (env)
                                                               (cons 'list
                                                                     (map (lambda (var)
                                                                            (lookup var env))
                                                                          vars)))
                                                             fail))
                                          fail)))
              (syntax-span expression))))

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
