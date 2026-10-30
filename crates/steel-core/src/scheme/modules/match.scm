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

  ;; Quoted parts of syntax can be plain data
  (define (stx-e x)
    (if (syntax? x) (syntax-e x) x))

  (define (datum-of x)
    (if (syntax? x) (#%syntax->datum x) x))

  (define (stx-list x)
    (let ([e (stx-e x)])
      (if (list? e) e #f)))

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

  ;; Elements of a list, where an ellipsis must follow a pattern
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
          (struct-pattern (if (syntax? (car args)) (car args) (#%datum->syntax stx (datum-of (car args))))
                          (stx-list (cadr args))
                          table
                          stx)]
         [(or (equal? keyword 'cons) (equal? keyword 'box))
          (unless (= (length args) (if (equal? keyword 'cons) 2 1))
            (pattern-error stx
                           (if (equal? keyword 'cons)
                               "match: cons takes two patterns"
                               "match: box takes one pattern")))
          (cons keyword (map (lambda (p) (pattern-of p table)) args))]
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
          (struct-pattern (if (syntax? (car e)) (car e) (#%datum->syntax stx head)) args table stx)]
         [else (pattern-error stx "match: bad pattern")])]
      [(or (pair? e) (vector? e)) (pattern-error stx "match: bad pattern")]
      [else (datum-of stx)]))

  (define (struct-pattern name fields table stx)
    (when (has-ellipsis? (map datum-of fields))
      (pattern-error stx "match: an ellipsis can't be used in a struct pattern"))
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
      [(vector? e) (cons 'vector (quasi-items (vector->list e) table stx))]
      [(pair? e)
       (let loop ([items e]
                  [front '()])
         (if (pair? items)
             (loop (cdr items) (cons (car items) front))
             (cons 'list-rest
                   (append (quasi-items (reverse front) table stx)
                           (list (list 'quote (datum-of items)))))))]
      [else (list 'quote (datum-of stx))]))

  ;; `(a . ,b)` reads as `(a unquote b)`
  (define (quasi-list items table stx)
    (let loop ([rest items]
               [front '()])
      (cond
        [(null? rest) (cons 'list (quasi-items items table stx))]
        [(and (member (datum-of (car rest)) unquote-names) (pair? (cdr rest)) (null? (cddr rest)))
         (cons 'list-rest
               (append (quasi-items (reverse front) table stx)
                       (list (pattern-of (cadr rest) table))))]
        [else (loop (cdr rest) (cons (car rest) front))])))

  (define (quasi-items items table stx)
    (cond
      [(null? items) '()]
      [(ellipsis-min (datum-of (car items)))
       (cons (datum-of (car items)) (quasi-items (cdr items) table stx))]
      [(unquote-form? (car items) unquote-splicing-names)
       (let ([spliced (pattern-of (cadr (stx-e (car items))) table)]
             [rest (quasi-items (cdr items) table stx)])
         (cond
           [(symbol? spliced) (cons spliced (cons '... rest))]
           [(and (pair? spliced) (equal? (car spliced) 'list)) (append (cdr spliced) rest)]
           [(and (quoted? spliced) (list? (cadr spliced)))
            (append (map (lambda (x) (list 'quote x)) (cadr spliced)) rest)]
           [else (pattern-error (car items) "match: ,@ needs a list pattern")]))]
      [else (cons (pattern-of-quasi (car items) table) (quasi-items (cdr items) table stx))]))

  (define (has-ellipsis? pats)
    (cond
      [(null? pats) #f]
      [(ellipsis-min (car pats)) #t]
      [else (has-ellipsis? (cdr pats))]))

  ;; The variables a pattern binds, in the order they're written
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

  ;; Puts `body` in a thunk that the code `make` builds calls on failure. If it's called once, `body`
  ;; goes in place of the call. Failures only happen where nothing but the matcher's temporaries
  ;; are bound, so `body` sees the same bindings either way.
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

  ;; In match-syntax, a value is looked inside after unwrapping it, literals are compared with its
  ;; datum, and variables bind the syntax itself
  (define *syntax-mode* #f)

  (define (inspect v make)
    (if *syntax-mode*
        (bind-temp (mk-if (list 'syntax? v) (list 'syntax-e v) v) make)
        (make v)))

  (define (inspect-datum v make)
    (if *syntax-mode*
        (bind-temp (mk-if (list 'syntax? v) (list 'syntax->datum v) v) make)
        (make v)))

  (define (bind-temp expr make)
    (define t (gensym))
    (mk-let (list (list t expr)) (make t)))

  (define (all-but-last xs)
    (reverse (cdr (reverse xs))))

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
                      (lambda (v)
                        (if (null? (cadr pat))
                            (mk-if (list '#%prim.null? v) (k env) fail)
                            (mk-if (list '#%prim.equal? v pat) (k env) fail))))]
      [(pair? pat) (compile-compound pat v env k fail)]
      [else
       (inspect-datum v
                      (lambda (v) (mk-if (list '#%prim.equal? v (list 'quote pat)) (k env) fail)))]))

  (define (compile-all pats v env k fail)
    (if (null? pats)
        (k env)
        (compile-pattern (car pats)
                         v
                         env
                         (lambda (env) (compile-all (cdr pats) v env k fail))
                         fail)))

  ;; Each pattern against its own variable
  (define (compile-each-of pats vs env k fail)
    (if (null? pats)
        (k env)
        (compile-pattern (car pats)
                         (car vs)
                         env
                         (lambda (env) (compile-each-of (cdr pats) (cdr vs) env k fail))
                         fail)))

  (define (compile-compound pat v env k fail)
    (define head (car pat))
    (define args (cdr pat))
    (cond
      [(syntax? head) (inspect v (lambda (v) (compile-struct head args v env k fail)))]
      [(equal? head 'list)
       (inspect v (lambda (v) (compile-list args (list 'quote '()) v env k fail)))]
      [(equal? head 'list-rest)
       (inspect v (lambda (v) (compile-list (all-but-last args) (last args) v env k fail)))]
      [(equal? head 'cons)
       (inspect v (lambda (v) (compile-list (list (car args)) (cadr args) v env k fail)))]
      [(equal? head 'vector)
       (inspect v
                (lambda (v)
                  (mk-if (list '#%prim.vector? v)
                         (bind-temp (list 'vector->list v)
                                    (lambda (t)
                                      (compile-list args (list 'quote '()) t env k fail)))
                         fail)))]
      [(equal? head 'box)
       (inspect v
                (lambda (v)
                  (mk-if (list 'box? v)
                         (bind-temp (list 'unbox v)
                                    (lambda (t) (compile-pattern (car args) t env k fail)))
                         fail)))]
      [(equal? head 'hash-table)
       (inspect v
                (lambda (v)
                  (mk-if (list '#%prim.hash? v) (compile-hash args v env k fail) fail)))]
      [(equal? head '?)
       (mk-if (list (car args) v) (compile-all (cdr args) v env k fail) fail)]
      [(equal? head 'app)
       (if (> (length args) 2)
           ;; Several patterns match the values the function returns
           (let ([results (map (lambda (_) (gensym)) (cdr args))])
             (list 'call-with-values
                   (mk-lambda '() (list (car args) v))
                   (mk-lambda results (compile-each-of (cdr args) results env k fail))))
           (bind-temp (list (car args) v) (lambda (t) (compile-all (cdr args) t env k fail))))]
      [(equal? head '==)
       (inspect-datum v
                      (lambda (v)
                        (mk-if (if (null? (cdr args))
                                   (list '#%prim.equal? v (car args))
                                   (list (cadr args) (car args) v))
                               (k env)
                               fail)))]
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

  ;; Each alternative jumps to one continuation with the variables they bind
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

  ;; Each entry with a literal key is looked up. An entry followed by an ellipsis matches the
  ;; entries left over, each as a list of its key and value.
  (define (compile-hash entries v env k fail)
    (define repeated
      (and (>= (length entries) 2) (ellipsis-min (last entries)) (last (all-but-last entries))))
    (define lookups (if repeated (all-but-last (all-but-last entries)) entries))
    (define (key-of entry)
      (define key (cadr entry))
      (unless (or (quoted? key) (not (or (symbol? key) (pair? key))))
        (error! "match: a hash-table key must be a literal" (cons 'hash-table entries)))
      (if (quoted? key) key (list 'quote key)))
    (define keys (map key-of lookups))
    (if (null? entries)
        (mk-if (list '#%prim.= (list 'hash-length v) 0) (k env) fail)
        (let loop ([remaining lookups]
                   [keys-left keys]
                   [env env])
          (if (null? remaining)
              (if repeated
                  (bind-temp (hash-entries-without v keys)
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

  (define (compile-struct name pats v env k fail)
    (define predicate
      (#%datum->syntax name
                       (string->symbol (string-append (symbol->string (#%syntax->datum name)) "?"))))
    (mk-if (list predicate v)
           (bind-temp (list '#%struct-fields v)
                      (lambda (t)
                        (compile-fields pats
                                        t
                                        env
                                        k
                                        fail
                                        (list 'error!
                                              "match: a pattern for"
                                              (list 'quote (#%syntax->datum name))
                                              "has"
                                              (length pats)
                                              "fields, but the struct has"
                                              (list '#%prim.length t)))))
           fail))

  ;; A struct's fields, where a list of the wrong length is an error rather than a failed match
  (define (compile-fields pats v env k fail wrong-count)
    (if (null? pats)
        (mk-if (list '#%prim.null? v) (k env) wrong-count)
        (let ([a (gensym)]
              [d (gensym)])
          (mk-if (list '#%prim.pair? v)
                 (mk-let (list (list a (list '#%prim.car v)) (list d (list '#%prim.cdr v)))
                         (compile-pattern (car pats)
                                          a
                                          env
                                          (lambda (env)
                                            (compile-fields (cdr pats) d env k fail wrong-count))
                                          fail))
                 wrong-count))))

  (define (null-pattern? pat)
    (and (quoted? pat) (null? (cadr pat))))

  ;; The elements of a list against `pats`, and what follows them against `tail`
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

  ;; `pat` repeated at least `least` times, then `rest`
  (define (compile-repeat pat least rest tail v env k fail)
    (if (and (null-pattern? tail) (not (has-ellipsis? rest)))
        (compile-repeat-fixed pat least rest v env k fail)
        (compile-repeat-backtracking pat least rest tail v env k fail)))

  ;; Only the last `(length rest)` elements are left for `rest`
  (define (compile-repeat-fixed pat least rest v env k fail)
    (define after (length rest))
    (define (each front env k)
      (compile-each pat front env k fail))
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

  (define (reversed results accs)
    (map (lambda (r acc) (list r (list '#%prim.reverse acc))) results accs))

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
                    (mk-let (reversed results accs) (merge-repeated vars results env k fail))))]))

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
                           ;; The run is only reversed once what follows it has matched
                           (compile-list rest
                                         tail
                                         xs
                                         env
                                         (lambda (env)
                                           (mk-let (reversed results accs)
                                                   (merge-repeated vars results env k shorter)))
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

  ;; A clause's pattern, guard, failure name and body
  (define (clause-parts clause)
    (define items (stx-list clause))
    (when (or (not items) (null? items))
      (pattern-error clause "match: a clause needs a pattern and a body"))
    (define rest (cdr items))
    (define has-guard (and (pair? rest) (equal? (datum-of (car rest)) '#:when)))
    (when (and has-guard (null? (cdr rest)))
      (pattern-error clause "match: #:when needs an expression"))
    (define guard (if has-guard (cadr rest) #f))
    (define after-guard (if has-guard (cddr rest) rest))
    (define escape
      (let ([e (and (pair? after-guard) (stx-list (car after-guard)))])
        (cond
          [(not (and e (pair? e) (equal? (datum-of (car e)) '=>))) #f]
          [(and (= (length e) 2) (symbol? (datum-of (cadr e)))) (cadr e)]
          [else (pattern-error (car after-guard) "match: (=> name) needs one identifier")])))
    (define body (if escape (cdr after-guard) after-guard))
    (when (null? body)
      (pattern-error clause "match: a clause needs a body"))
    (list (car items) guard escape body))

  ;; `else` as the match module means it, so a local `else` is a pattern variable
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
        [(member (car (car env)) seen) (loop (cdr env) seen bindings)]
        [else
         (loop (cdr env)
               (cons (car (car env)) seen)
               (cons (list (key-identifier (car (car env)) table) (cdr (car env))) bindings))])))

  ;; The patterns of a clause against each of `inputs`
  (define (compile-clause patterns guard escape body inputs fail)
    (define table (mutable-vector))
    (define converted (map (lambda (p) (pattern-of p table)) patterns))
    (define body-expr (cons 'let (cons '() body)))
    (compile-each-of converted
                     inputs
                     '()
                     (lambda (env)
                       (bind-pattern-vars env
                                          table
                                          (let ([body-expr (if escape
                                                               (mk-let (list (list escape
                                                                                   (mk-lambda '()
                                                                                              fail)))
                                                                       body-expr)
                                                               body-expr)])
                                            (if guard
                                                (mk-if guard body-expr fail)
                                                body-expr))))
                     fail))

  ;; Each clause's failure is a thunk running the clauses after it. The failure only runs where
  ;; the matcher's temporaries are bound, unless the clause has a guard or a failure name.
  (define (compile-clauses clauses inputs split no-match else-id)
    (if (null? clauses)
        no-match
        (let ([clause (car clauses)])
          (if (else-clause? clause else-id)
              (begin
                (when (null? (cdr (stx-list clause)))
                  (pattern-error clause "match: a clause needs a body"))
                (cons 'let (cons '() (cdr (stx-list clause)))))
              (let* ([parts (clause-parts clause)]
                     [patterns (split (car parts))]
                     [guard (cadr parts)]
                     [escape (caddr parts)]
                     [rest (compile-clauses (cdr clauses) inputs split no-match else-id)]
                     [name (gensym)]
                     [code (compile-clause patterns guard escape (cadddr parts) inputs (list name))]
                     [uses (calls-of code name)])
                (cond
                  ;; Clauses after one that can't fail are still compiled, for their errors
                  [(and (= uses 0) (null? (cdr clauses))) code]
                  [(and (= uses 1) (not guard) (not escape)) (replace-call code name rest)]
                  [else (mk-let (list (list name (mk-lambda '() rest))) code)]))))))

  (define (match-error inputs)
    (list 'error!
          "Unable to match expression: "
          (if (= (length inputs) 1) (car inputs) (cons 'list inputs))
          " to any of the given patterns"))

  ;; `head` is the name of the macro as this module's template wrote it
  (define (compile-match head exprs clauses split syntax-mode)
    (set! *syntax-mode* syntax-mode)
    (let ([inputs (map (lambda (_) (gensym)) exprs)])
      (mk-let (map list inputs exprs)
              (compile-clauses clauses inputs split (match-error inputs) (#%datum->syntax head 'else))))))

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

;; Defines the pattern variables where the form is written
(defmacro (match-define-pattern expression)
          (define items (stx-list expression))
          (unless (= (length items) 3)
            (error! "match-define: expects a pattern and an expression"))
          (set! *syntax-mode* #f)
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
