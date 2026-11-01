;; The kernel for Steel.
;; This contains core forms that are expanded during the last phase of macro expansion
;; Macros that are exported from the kernel and applied on code externally are defined via
;; the form `(#%define-syntax <macro> <body>)`.
;;
;; This makes this function publicly available for the kernel to expand
;; forms with.

; (define *transformer-functions* (hashset))

;; TODO: Move the parameter stuff to the stdlib.
; (define #%syntax-bindings (make-parameter (hash)))
; (define #%syntax-binding-kind (make-parameter (hash)))

(set! #%syntax-bindings (make-parameter (hash)))
(set! #%syntax-binding-kind (make-parameter (hash)))

;; TODO: Figure out a way to have this bootstrap correctly
;; in the global environment within the stdlib, and not just
;; reserved for the kernel.
; `old` with the entries of `new`, which replace the ones of the same key
(define (#%shadowing old new)
  (let loop ([keys (hash-keys->list new)]
             [result old])
    (if (null? keys)
        result
        (loop (cdr keys) (hash-insert result (car keys) (hash-ref new (car keys)))))))

; A function made in a syntax-case clause, which sees that clause's pattern variables when it
; runs, as it would lexically
(define-syntax #%syntax-closure
  (syntax-rules ()
    [(_ f)
     (let ([bindings (#%syntax-bindings)]
           [kinds (#%syntax-binding-kind)]
           [function f])
       (lambda args
         (parameterize ([#%syntax-bindings bindings]
                        [#%syntax-binding-kind kinds])
           (apply function args))))]))

; A template outside of any syntax-case clause or with-syntax, which has no pattern variables
(define-syntax #%syntax-without-patterns
  (syntax-rules ()
    [(_ template)
     (parameterize ([#%syntax-bindings (hash)]
                    [#%syntax-binding-kind (hash)])
       template)]))

; The right hand sides are all evaluated before any pattern binds
(define-syntax with-syntax
  (syntax-rules ()
    [(_ (clause ...) body ...) (#%with-syntax-values (clause ...) () body ...)]))

(define-syntax #%with-syntax-values
  (syntax-rules ()
    [(_ () ((p value) ...) body ...) (#%with-syntax-each ([p value] ...) body ...)]
    [(_ ([p expr] rest ...) (done ...) body ...)
     (let ([value expr])
       (#%with-syntax-values (rest ...) (done ... (p value)) body ...))]))

(define-syntax #%with-syntax-each
  (syntax-rules ()
    ;; A pattern binds its pattern variables, as a syntax-case clause does
    [(_ ([(p . ps) expr] others ...) body ...)
     (let ([#%matched (#%with-syntax-match '(p . ps) expr)])
       ;; The pattern's variables shadow the ones bound already
       (parameterize ([#%syntax-bindings (#%shadowing (#%syntax-bindings) (car #%matched))]
                      [#%syntax-binding-kind (#%shadowing (#%syntax-binding-kind) (cadr #%matched))])
         (#%with-syntax-each (others ...)
           body ...)))]

    [(_ () body ...) (let () body ...)]

    [(_ ([var expr]) body ...)
     (parameterize ([#%syntax-bindings (hash-insert (#%syntax-bindings) 'var expr)])
       body ...)]

    [(_ ([var expr] others ...) body ...)
     (parameterize ([#%syntax-bindings (hash-insert (#%syntax-bindings) 'var expr)])
       (#%with-syntax-each (others ...)
         body ...))]))

; syntax-case as an expression, in a transformer's code. The one a define-syntax ends with is
; taken apart by the define-syntax.
(define-syntax #%syntax-case
  (syntax-rules ()
    [(_ e (literal ...) clause ...)
     (let ([input e])
       (#%syntax-case-clauses input (literal ...) clause ...))]))

(define-syntax #%syntax-case-clauses
  (syntax-rules ()
    [(_ input literals) (error "syntax-case: bad syntax" input)]
    [(_ input literals [pattern body] clause ...)
     (#%syntax-case-clauses input literals [pattern #t body] clause ...)]
    [(_ input literals [pattern fender body] clause ...)
     (let ([matched (#%syntax-case-match 'pattern 'literals input)])
       (define (next)
         (#%syntax-case-clauses input literals clause ...))
       (if matched
           (let ([result
                  (parameterize ([#%syntax-bindings (#%shadowing (#%syntax-bindings) (car matched))]
                                 [#%syntax-binding-kind
                                  (#%shadowing (#%syntax-binding-kind) (cadr matched))])
                    (if fender body #%syntax-case-no-match))])
             ;; The next clause doesn't see what this one bound
             (if (eq? result #%syntax-case-no-match) (next) result))
           (next)))]))

;; Compatibility layers for making defmacro not as painful
(define displayln stdout-simple-displayln)

;; TODO: This needs to be updated with the current module during execution.
;; that way, `syntax-case` can go ahead and check which module to expand
;; from. It also needs a way to dynamically add itself to that module hash.
(define #%loading-current-module "default")

(define (#%set-module mod)
  (set! #%loading-current-module mod))

(define top-level (%proto-hash%))

;; Snag the current environment
(define (current-env)
  #%loading-current-module)

(define-syntax #%syntax-transformer-module
  (syntax-rules (provide)

    ;; An environment with phase 1 bindings but no macros
    [(#%syntax-transformer-module provide) (%proto-hash%)]

    [(#%syntax-transformer-module name)
     (define name
       (%proto-hash%))]

    [(#%syntax-transformer-module name (provide ids ...) funcs ...)
     (define name
       (let ()
         (begin
           funcs ...)
         (#%syntax-transformer-module provide ids ...)))]

    ;; Normal case
    [(#%syntax-transformer-module provide name) (%proto-hash% 'name name)]

    ;; Normal case
    [(#%syntax-transformer-module provide name rest ...)
     (%proto-hash-insert% (#%syntax-transformer-module provide rest ...) 'name name)]))

(define-syntax #%syntax-transformer-module-update
  (syntax-rules (provide)

    [(#%syntax-transformer-module provide) (%proto-hash%)]

    [(#%syntax-transformer-module name) void]

    [(#%syntax-transformer-module name (provide ids ...) funcs ...)
     (set! name
           (let ()
             (begin
               funcs ...)
             ;; A macro defined again replaces the old one
             (hash-union (#%syntax-transformer-module provide ids ...)
                         (if (void? name)
                             (%proto-hash%)
                             name))))]

    ;; Normal case
    [(#%syntax-transformer-module provide name) (%proto-hash% 'name name)]

    ;; Normal case
    [(#%syntax-transformer-module provide name rest ...)
     (%proto-hash-insert% (#%syntax-transformer-module provide rest ...) 'name name)]))

;; Loading macros via defmacro - there will be a pass where we lower anything with defmacro down to the kernel,
;; which will then load and register macros accordingly.
(define-syntax defmacro
  (syntax-rules ()
    [(defmacro environment (name arg) expr)
     (begin
       (register-macro-transformer! (symbol->string 'name) environment)
       (define (name arg)
         expr))]

    [(defmacro environment (name arg) exprs ...)
     (begin
       (register-macro-transformer! (symbol->string 'name) environment)
       (define (name arg)
         exprs ...))]

    [(defmacro environment name expr)
     (begin
       (register-macro-transformer! (symbol->string 'name) environment)
       (define name expr))]))

(define-syntax #%define-syntax
  (syntax-rules ()

    [(#%define-syntax (name arg) expr)
     (begin
       (register-macro-transformer! (symbol->string 'name) (current-env))
       (define (name arg)
         expr))]

    [(#%define-syntax (name arg) exprs ...)
     (begin
       (register-macro-transformer! (symbol->string 'name) (current-env))
       (define (name arg)
         exprs ...))]

    [(#%define-syntax name expr)
     (begin
       (register-macro-transformer! (symbol->string 'name) (current-env))
       (define name expr))]))

;; Kernal-lambda -> Used in the meantime while `lambda` finds its way out of the reserved keywords.
(define-syntax klambda
  (syntax-rules ()
    [(klambda () expr exprs ...) (#%plain-lambda () expr exprs ...)]
    [(klambda (x xs ...) expr exprs ...) (#%plain-lambda (x xs ...) expr exprs ...)]
    [(klambda x expr exprs ...) (#%plain-lambda x expr exprs ...)]))

;; Enumerate the given list starting at index `start`
(define (enumerate start accum lst)
  (if (empty? lst)
      (reverse accum)
      (enumerate (+ start 1) (cons (list (car lst) start) accum) (cdr lst))))

;; The keys and values of a hash map as arguments to `hash`, for the given keys in their order
(define (option-arguments hm keys)
  (apply append
         (map (lambda (k)
                (list (if (symbol? k)
                          (list 'quote k)
                          k)
                      (hash-ref hm k)))
              keys)))

;; The indices of the fields named in `#:equality-skip`, which `equal?` and
;; hashing leave out. Only a transparent struct compares its fields at all.
(define (equality-skip-indices skipped fields transparent? struct-name)
  (unless transparent?
    (error! "struct #:equality-skip needs #:transparent, since an opaque struct is only equal? to itself:"
            struct-name))
  (unless (and (list? skipped)
               (let loop ([rest skipped])
                 (or (null? rest) (and (symbol? (car rest)) (loop (cdr rest))))))
    (error! "struct #:equality-skip expects a list of field names, found " skipped))
  (map (lambda (name)
         (let loop ([rest fields]
                    [i 0])
           (cond
             [(null? rest) (error! "struct #:equality-skip names a field the struct doesn't have:" name)]
             [(equal? (car rest) name) i]
             [else (loop (cdr rest) (+ i 1))])))
       skipped))

(define (mutable-keyword? x)
  (equal? x '#:mutable))
(define (transparent-keyword? x)
  (equal? x '#:transparent))

(define (identifier? x)
  (symbol? (syntax-e x)))

;; The elements of syntax for a list, or #f
(define (syntax->list x)
  (let ([e (if (syntax? x) (syntax-e x) x)])
    (if (list? e) e #f)))

;; The same name with the same scopes
(define (bound-identifier=? a b)
  (and (identifier? a)
       (identifier? b)
       (eq? (syntax-e a) (syntax-e b))
       (#%syntax-same-scopes? a b)))

;; Whether they refer to the same binding where they're used. An imported name can be spelled
;; differently.
(define (free-identifier=? a b)
  (and (identifier? a)
       (identifier? b)
       (#%syntax-same-binding? a b)))

;; Fresh identifiers, one for each element of a syntax list
(define (generate-temporaries stx)
  (map (lambda (x) (#%datum->syntax x (gensym-sym '#%temporary)))
       (if (list? stx) stx (syntax->list stx))))

;; The transformers that define-syntax expressions made, by their keys
(define #%transformer-values (box (hash)))

(define (#%transformer-once key make)
  (let ([made (hash-try-get (unbox #%transformer-values) key)])
    (if made
        made
        (let ([made (make)])
          (set-box! #%transformer-values (hash-insert (unbox #%transformer-values) key made))
          made))))

(define (all-but-last l)
  (reverse (cdr (reverse l))))

(#%define-syntax (struct expr)
                 (define unwrapped (syntax-e expr))
                 (define struct-name (#%syntax->datum/kernel (second unwrapped)))
                 (define fields (#%syntax->datum/kernel (third unwrapped)))
                 (define options
                   (let ([raw (cdddr unwrapped)])
                     ; (displayln raw)
                     (if (empty? raw)
                         raw
                         (map #%syntax->datum/kernel raw))))
                 (define result (struct-impl struct-name fields options))
                 (#%syntax/with-mark-of (syntax/loc result
                                          (syntax-span expr))
                                        (second unwrapped)))

;; Provide specs, like Racket's provide transformers. Each is a list of names and the names to
;; provide them as, and nests in the others. Ones that rename expand to `(#%provide-as name
;; exported)`.
(define (spec-error expr message)
  (error-with-span (syntax-span expr) message (#%syntax->datum/kernel expr)))

(define (provide-exports expr)
  (define e (syntax-e expr))
  (cond
    [(symbol? e) (list (cons e e))]
    [(and (list? e) (pair? e) (symbol? (syntax-e (car e))))
     (let ([head (syntax-e (car e))]
           [args (cdr e)])
       (cond
         [(equal? head 'struct-out)
          (let ([info (and (= (length args) 1) (#%struct-info (car args)))])
            (unless info
              (spec-error expr "struct-out expects the name of a struct, found:"))
            ;; For a struct another module defines, only the names imported here
            (let ([names (struct-names (first info) (second info) (third info))])
              (map (lambda (x) (cons x x))
                   (if (list-ref info 4) names (filter #%imported? names)))))]
         [(equal? head 'all-from-out)
          (apply append
                 (map (lambda (path)
                        (let ([names (and (string? (syntax-e path)) (#%imported-from (syntax-e path)))])
                          (unless names
                            (spec-error path "all-from-out expects a required module, found:"))
                          (map (lambda (x) (cons x x)) names)))
                      args))]
         [(equal? head 'all-defined-out)
          (unless (null? args)
            (spec-error expr "all-defined-out takes no arguments:"))
          (map (lambda (x) (cons x x))
               (filter (lambda (x) (not (starts-with? (symbol->string x) "#")))
                       (#%defined-names)))]
         [(equal? head 'prefix-out)
          (unless (and (pair? args) (symbol? (syntax-e (car args))))
            (spec-error expr "prefix-out expects a prefix and provide specs:"))
          (map (lambda (export) (cons (car export) (concat-symbols (syntax-e (car args)) (cdr export))))
               (apply append (map provide-exports (cdr args))))]
         [(equal? head 'rename-out)
          (map (lambda (pair)
                 (let ([names (syntax-e pair)])
                   (unless (and (list? names)
                                (= (length names) 2)
                                (symbol? (syntax-e (first names)))
                                (symbol? (syntax-e (second names))))
                     (spec-error pair "rename-out expects pairs of a name and the name to provide it as:"))
                   (cons (syntax-e (first names)) (syntax-e (second names)))))
               args)]
         [(equal? head 'except-out)
          (unless (pair? args)
            (spec-error expr "except-out expects a provide spec and names:"))
          (let ([excluded (map syntax-e (cdr args))])
            (filter (lambda (export) (not (member (cdr export) excluded)))
                    (provide-exports (car args))))]
         [(equal? head 'combine-out) (apply append (map provide-exports args))]
         [else (spec-error expr "this can't be used inside another provide spec:")]))]
    [else (spec-error expr "bad provide spec:")]))

;; The names a spec's first identifier is written with, or its head's when it has none
(define (spec-identifier expr)
  (let loop ([items (cdr (syntax-e expr))])
    (cond
      [(null? items) (car (syntax-e expr))]
      [(symbol? (syntax-e (car items))) (car items)]
      [(list? (syntax-e (car items)))
       (let ([inner (loop (syntax-e (car items)))])
         (if (equal? inner (car (syntax-e expr))) (loop (cdr items)) inner))]
      [else (loop (cdr items))])))

(define (provide-spec expr)
  (#%syntax/with-mark-of (syntax/loc (cons '#%provide-specs
                                           (map (lambda (export)
                                                  (if (equal? (car export) (cdr export))
                                                      (car export)
                                                      (list '#%provide-as (car export) (cdr export))))
                                                (provide-exports expr)))
                           (syntax-span expr))
                         (spec-identifier expr)))

(#%define-syntax (struct-out expr)
                 (provide-spec expr))
(#%define-syntax (all-from-out expr)
                 (provide-spec expr))
(#%define-syntax (all-defined-out expr)
                 (provide-spec expr))
(#%define-syntax (prefix-out expr)
                 (provide-spec expr))
(#%define-syntax (rename-out expr)
                 (provide-spec expr))
(#%define-syntax (except-out expr)
                 (provide-spec expr))
(#%define-syntax (combine-out expr)
                 (provide-spec expr))

;; Require specs, like Racket's require transformers. Each expands to `(#%require-spec module
;; what ...)`: the module, then what each spec around it does to the names, innermost first.
(define (require-parts expr)
  (define e (syntax-e expr))
  (define (identifier? x)
    (let ([x (syntax-e x)])
      (and (symbol? x) (not (starts-with? (symbol->string x) "#:")))))
  (define (pair-of-identifiers? x)
    (let ([names (syntax-e x)])
      (and (list? names) (= (length names) 2) (identifier? (first names)) (identifier? (second names)))))
  (cond
    [(string? e) (list expr)]
    [(and (list? e) (pair? e) (identifier? (car e)))
     (let ([head (syntax-e (car e))]
           [args (cdr e)])
       (cond
         ;; Already expanded
         [(equal? head '#%require-spec) args]
         [(equal? head 'for-syntax)
          (unless (= (length args) 1)
            (spec-error expr "for-syntax expects one module to require:"))
          (list (list (car e) (require-spec (car args))))]
         [(not (member head '(only-in prefix-in rename-in except-in)))
          (spec-error expr "this can't be used inside another require spec:")]
         [(null? args) (spec-error expr "a require spec needs a module:")]
         [(equal? head 'only-in)
          (for-each (lambda (x)
                      (unless (or (identifier? x) (pair-of-identifiers? x))
                        (spec-error x "only-in expects identifiers, or pairs of identifiers to rename:")))
                    (cdr args))
          (append (require-parts (car args)) (list (cons '#:only (cdr args))))]
         [(equal? head 'prefix-in)
          (unless (and (= (length args) 2) (identifier? (car args)))
            (spec-error expr "prefix-in expects a prefix and a require spec:"))
          (append (require-parts (cadr args)) (list (list '#:prefix (car args))))]
         [(equal? head 'rename-in)
          (for-each (lambda (x)
                      (unless (pair-of-identifiers? x)
                        (spec-error x "rename-in expects pairs of the name it provides and the name to bind:")))
                    (cdr args))
          (append (require-parts (car args)) (list (cons '#:rename (cdr args))))]
         [(equal? head 'except-in)
          (for-each (lambda (x)
                      (unless (identifier? x)
                        (spec-error x "except-in expects identifiers to leave out:")))
                    (cdr args))
          (append (require-parts (car args)) (list (cons '#:except (cdr args))))]))]
    [else (spec-error expr "bad require spec:")]))

(define (require-spec expr)
  (syntax/loc (cons '#%require-spec (require-parts expr))
    (syntax-span expr)))

(#%define-syntax (only-in expr)
                 (require-spec expr))
(#%define-syntax (prefix-in expr)
                 (require-spec expr))
(#%define-syntax (rename-in expr)
                 (require-spec expr))
(#%define-syntax (except-in expr)
                 (require-spec expr))

(define (struct-names struct-name fields mutable?)
  (append (list struct-name
                (concat-symbols struct-name '?)
                (concat-symbols 'struct: struct-name)
                (concat-symbols '___ struct-name '-options___))
          (map (lambda (field) (concat-symbols struct-name '- field)) fields)
          (if mutable?
              (map (lambda (field) (concat-symbols 'set- struct-name '- field '!)) fields)
              '())))

;; Macro for creating a new struct, in the form of:
;; `(struct <struct-name> (fields ...) options ...)`
;; The options can consist of the following:
;;
;; Single variable options (those which their presence indicates #true)
;; - #:mutable
;; - #:transparent
;;
;; Other options must be presented as key value pairs, and will get stored
;; in the struct instance. They will also be bound to the variable
;; ___<struct-name>-options___ in the same lexical environment where the
;; struct was defined. For example:
;;
;; (Applesauce (a b c) #:mutable #:transparent #:unrecognized-option 1234)
;;
;; Will result in the value `___Applesauce-options___` like so:
;; (hash #:mutable #true #:transparent #true #:unrecognized-option 1234)
;;
;; By default, structs are immutable, which means setter functions will not
;; be generated. Also by default, structs are not transparent, which means
;; printing them will result in an opaque struct that does not list the fields
(define (struct-impl struct-name fields options)

  ;; Add a field for storing the options, and for the index to the func
  (define field-count (length fields))
  ;; Mark whether this is actually a mutable and transparent struct, and
  ;; then drain the values from the
  (define mutable? (contains? mutable-keyword? options))
  (define transparent? (contains? transparent-keyword? options))

  (define options-without-single-keywords
    (transduce options
               (filtering (lambda (x) (not (mutable-keyword? x))))
               (filtering (lambda (x) (not (transparent-keyword? x))))
               (into-list)))

  (define default-printer-function
    (if transparent?
        `(lambda (obj printer-function)
           (simple-display "(")
           (simple-display (#%prim.symbol->string ,(list 'quote struct-name)))
           ,@(map (lambda (field)
                    `(begin
                       (simple-display " ")
                       (printer-function (,(concat-symbols struct-name '- field) obj))))
                  fields)

           (simple-display ")"))

        #f))

  ;; Set up default values to go in the table
  (define extra-options
    (hash '#:mutable
          mutable?
          '#:transparent
          transparent?
          '#:fields
          (list 'quote fields)
          '#:name
          (list 'quote struct-name)))

  (when (not (list? fields))
    (error! "struct expects a list of field names, found " fields))

  (when (not (symbol? struct-name))
    (error! "struct expects an identifier as the first argument, found " struct-name))

  (when (odd? (length options-without-single-keywords))
    (error! "struct options are malformed - each option requires a value"))

  ;; For struct-out and match, like Racket's static struct info
  (#%record-struct-info! struct-name fields mutable?)

  ;; Update the options-map to have the fields included
  (let* ([options-map (apply hash options-without-single-keywords)]
         [options-map (hash-union options-map extra-options)]
         [options-map (if (hash-try-get options-map '#:printer)
                          options-map
                          (hash-insert options-map '#:printer default-printer-function))]
         [options-map (if (hash-contains? options-map '#:equality-skip)
                          (hash-insert options-map
                                       '#:equality-skip
                                       (list 'quote
                                             (equality-skip-indices (hash-ref options-map
                                                                              '#:equality-skip)
                                                                    fields
                                                                    transparent?
                                                                    struct-name)))
                          options-map)]
         [maybe-procedure-field (hash-try-get options-map '#:prop:procedure)]
         [maybe-finalizer (hash-try-get options-map '#:finalizer)])

    (when (and maybe-procedure-field (> maybe-procedure-field (length fields)))
      (error! "struct #:prop:procedure cannot refer to an index that is out of bounds"))

    ;; The options in the order they were written, then the defaults. Hash map
    ;; order changes from run to run, which would change the compiled code.
    (define option-keys
      (let loop ([rest (append options-without-single-keywords
                               (list '#:mutable #f '#:transparent #f '#:fields #f
                                     '#:name #f '#:printer #f))]
                 [keys '()])
        (cond
          [(null? rest) (reverse keys)]
          [(or (contains? (lambda (k) (equal? k (car rest))) keys)
               (not (hash-contains? options-map (car rest))))
           (loop (cddr rest) keys)]
          [else (loop (cddr rest) (cons (car rest) keys))])))

    (define struct-options-name (concat-symbols '___ struct-name '-options___))
    (define struct-prop-name (concat-symbols 'struct: struct-name))
    (define struct-predicate (concat-symbols struct-name '?))

    (define struct-proto-gensym (gensym))
    (define constructor-proto-gensym (gensym))
    (define predicate-proto-gensym (gensym))
    (define getter-proto-list-gensym (gensym))
    (define struct-prop-name-gensym (gensym))
    ;; Named so that serializing a function can tell the key from a number.
    (define struct-key-gensym
      (string->symbol (string-append "##struct-key" (symbol->string (gensym)))))

    `(begin
       (define ,struct-proto-gensym
         ;; Include the relative module as a key for the struct. For serialization
         ;; purposes, we'll treat two structs that come from the same place with the
         ;; same name as more or less, identical
         (#%prim.make-struct-type (quote ,struct-name) ,field-count (#%prim.current-module-relative)))
       (define ,struct-prop-name-gensym (#%prim.list-ref ,struct-proto-gensym 0))
       (define ,constructor-proto-gensym (#%prim.list-ref ,struct-proto-gensym 1))
       (define ,predicate-proto-gensym (#%prim.list-ref ,struct-proto-gensym 2))
       (define ,getter-proto-list-gensym (#%prim.list-ref ,struct-proto-gensym 4))
       (define ,struct-options-name
         (#%prim.hash ,@(option-arguments options-map option-keys)))

       (define ,struct-prop-name ,struct-prop-name-gensym)
       (define ,struct-key-gensym (#%struct-type-key ,struct-prop-name-gensym))
       (#%vtable-update-entry! ,struct-prop-name-gensym ,maybe-procedure-field ,struct-options-name)
       (if ,(not (bool? maybe-finalizer))
           (#%start-will-executor)
           #%prim.void)
       ,(if mutable?
            ;; A mutable struct keeps its fields inline, and names its type
            ;; by key.
            (if maybe-finalizer
                `(define ,struct-name
                   (lambda ,fields
                     (#%register-struct-finalizer (#%mstruct ,struct-key-gensym ,@fields)
                                                  ,maybe-finalizer)))

                `(define ,struct-name
                   (lambda ,fields
                     (#%mstruct ,struct-key-gensym ,@fields))))

            (if maybe-finalizer
                `(define ,struct-name
                   (lambda ,fields
                     (#%register-struct-finalizer (,constructor-proto-gensym ,@fields)
                                                  ,maybe-finalizer)))

                `(define ,struct-name ,constructor-proto-gensym)))
       ,(new-make-predicate struct-predicate predicate-proto-gensym)
       ,@(if mutable?
             (packed-make-getters struct-name fields struct-key-gensym)
             (new-make-getters struct-name fields getter-proto-list-gensym))
       ;; If this is a mutable struct, generate the setters
       ,@(if mutable?
             (packed-make-setters struct-name fields struct-key-gensym)
             (list))
       #%prim.void)))

(define (new-make-predicate struct-predicate-name predicate-proto-gensym)
  `(define ,struct-predicate-name ,predicate-proto-gensym))

;; The accessors of a struct with its fields inline. They name the type, so
;; the access checks the struct against it.
(define (packed-make-getters struct-name fields key-gensym)
  (map (lambda (field)
         `(define ,(concat-symbols struct-name '- (car field))
            (lambda (this) (#%mstruct-ref ,key-gensym this ,(#%prim.list-ref field 1)))))
       (enumerate 0 '() fields)))

(define (packed-make-setters struct-name fields key-gensym)
  (map (lambda (field)
         `(define ,(concat-symbols 'set- struct-name '- (car field) '!)
            (lambda (this value)
              (#%mstruct-set! ,key-gensym this ,(#%prim.list-ref field 1) value))))
       (enumerate 0 '() fields)))

(define (new-make-getters struct-name fields getter-proto-list-gensym)
  (map (lambda (field)
         `(define ,(concat-symbols struct-name '- (car field))
            (#%prim.list-ref ,getter-proto-list-gensym ,(#%prim.list-ref field 1))))
       (enumerate 0 '() fields)))

(define (%make-memoize f)
  (lambda n
    (let ([previous-value (%memo-table-ref %memo-table f n)])
      (if (and (Ok? previous-value) (Ok->value previous-value))
          (begin
            (Ok->value previous-value))
          (let ([new-value (apply f n)])
            (%memo-table-set! %memo-table f n new-value)
            new-value)))))

(define *gensym-counter* 0)
(define (gensym)
  (set! *gensym-counter* (+ 1 *gensym-counter*))
  (string->symbol (string-append "##gensym" (to-string *gensym-counter*))))

(#%define-syntax (#%better-lambda expr) (quasisyntax (list 10 20 30)))

;; TODO: make this not so suspect, but it does work!
(#%define-syntax (#%current-kernel-transformers expr)
                 (cons 'list (map (lambda (x) (list 'quote x)) (current-macro-transformers!))))

(#%define-syntax (#%fake-lambda expr)
                 (define underlying (syntax-e expr))
                 (define rest (cdr underlying))
                 (cons '#%plain-lambda rest))

;; Note: Will only work if the length is even
(define (take-every-other lst)
  (define count (/ (length lst) 2))
  (define indices (map (lambda (x) (+ 1 (* x 2))) (range 0 count)))
  (map (lambda (x) (list-ref lst x)) indices))

(define gensym-offset 0)
(define (gensym-sym base)
  (string->symbol (string-append (symbol->string base)
                                 (int->string (set! gensym-offset (+ gensym-offset 1))))))

;; TODO: Snag define-syntax, convert it to the right format?
;; As of now, the define-syntax will get parsed and yoinked
;; into the wrong format. Add plumbing to ignore those.
;; A syntax-case macro, from its pattern matcher and the body of each case
(define #%syntax-case-no-match (gensym))

(define (#%syntax-case-transformer matcher body)
  (lambda (expr)
    (define input (#%syntax/hygienic expr))
    (let loop ([start 0])
      (define result (#%match-syntax-case matcher input start))
      (define index (#%prim.list-ref result 0))
      (define case-bindings (#%prim.list-ref result 1))
      (define binding-kind (#%prim.list-ref result 2))
      (define output
        (parameterize ([#%syntax-bindings case-bindings])
          (parameterize ([#%syntax-binding-kind binding-kind])
            ;; The body sees the input as the transformer's parameter
            (body index input))))
      ;; A clause whose fender is false doesn't match
      (if (eq? output #%syntax-case-no-match)
          (loop (+ index 1))
          output))))

; Whether each syntax-case clause has a list for its pattern
(define (#%list-patterns? clauses)
  (cond
    [(null? clauses) #t]
    [(and (list? (car clauses)) (pair? (car (car clauses)))) (#%list-patterns? (cdr clauses))]
    [else #f]))

(define (parse-def-syntax stx phase1-env)

  ;; Name of the function
  (define name-expr (list-ref stx 1))

  ; (when (and (list? stx) (not (empty? stx)) (equal? (car stx) 'begin))
  ;   (stdout-simple-displayln "Updating the stx to destructure the begin")
  ;   (set! stx (cdr stx)))

  (cond

    [(list? name-expr)

     ; (stdout-simple-displayln stx)
     ; (stdout-simple-displayln "==============================")

     (define param-name (list-ref name-expr 1))

     ;; A body that ends in a syntax-case of its parameter is taken apart into a matcher
     ;; and the clauses. Anything else is code that runs on every use, with a syntax-case
     ;; in it matching what it's given.
     (define written-last (last stx))
     ;; Its syntax-case is wrapped to bind only what its clauses bind
     (define last-expr
       (if (and (list? written-last)
                (= (length written-last) 2)
                (equal? (car written-last) '#%syntax-without-patterns)
                (list? (cadr written-last))
                (>= (length (cadr written-last)) 3)
                (member (car (cadr written-last)) '(syntax-case #%syntax-case))
                (equal? (list-ref (cadr written-last) 1) param-name))
           (cadr written-last)
           written-last))
     ;; The matcher is a syntax-rules macro, whose patterns are lists
     (define syntax-case-expr
       (if (and (list? last-expr)
                (>= (length last-expr) 3)
                (member (car last-expr) '(syntax-case #%syntax-case))
                (equal? (list-ref last-expr 1) param-name)
                (#%list-patterns? (drop last-expr 3)))
           last-expr
           `(syntax-case ,param-name () [(_ . #%whole) ,last-expr])))

     ; (stdout-simple-displayln syntax-case-expr)
     ; (stdout-simple-displayln "--------------------------")

     (define body-exprs (all-but-last (drop stx 2)))

     (define name (list-ref name-expr 0))
     (define syntax-case-param (list-ref syntax-case-expr 1))
     (define syntax-case-syntax (list-ref syntax-case-expr 2))

     (define cases (drop syntax-case-expr 3))

     (define gensym-name (gensym-sym (concat-symbols '__generated- name)))

     ;; Expand to syntax-rules:
     ;; A clause can have a fender between its pattern and its body
     (define fake-syntax-rules
       `(define-syntax ,gensym-name
          (syntax-rules ,syntax-case-syntax
            ,@(map (lambda (p) (list (car p) (last p))) cases))))

     ;; This needs to be eval'd right away so that we can actually
     ;; reference the values.
     (eval fake-syntax-rules)

     (define conditions
       (map (lambda (p)
              (if (= (length p) 3)
                  `(if ,(list-ref p 1) ,(list-ref p 2) #%syntax-case-no-match)
                  (list-ref p 1)))
            cases))

     ;; List of cases, in order, for the syntax rules
     (define matched-case-bindings (#%macro-case-bindings (symbol->string gensym-name)))
     (define generated-function-name (gensym-sym '#%func))
     ;; Picks the clause by index with `if`, which the transformer's own definitions can't shadow
     (define dispatch
       (let loop ([i 0]
                  [conditions conditions])
         (if (null? (cdr conditions))
             (car conditions)
             `(if (#%prim.eq? #%#%index ,i) ,(car conditions) ,(loop (+ i 1) (cdr conditions))))))

     (define expansion-func
       ;; TODO: gensym this!
       `(define ,generated-function-name
          (lambda (#%#%index ,param-name)
            ,@body-exprs
            ,dispatch)))

     (define generated-match-function
       `(#%syntax-case-transformer ,(symbol->string gensym-name) ,generated-function-name))

     (eval expansion-func)

     ;; Another module that uses the macro builds its own transformer from this
     (#%record-phase1-transformer! phase1-env
                                   name
                                   (symbol->string gensym-name)
                                   `(lambda (#%#%index ,param-name)
                                      ,@body-exprs
                                      ,dispatch))

     generated-match-function]

    [(symbol? name-expr)
     ;; (lambda (x) ...)
     (define lambda-expr (list-ref stx 2))
     (cond
       [(and (list? lambda-expr) (equal? (car lambda-expr) 'lambda))

        ; (stdout-simple-displayln "We're under this case")
        ; (stdout-simple-displayln lambda-expr)
        ; (stdout-simple-displayln (drop lambda-expr 2))
        ; (stdout-simple-displayln (cdar (drop lambda-expr 2)))

        ;; TODO: @Matt clean this bad boy up
        (define list-of-expressions
          (if (and (= (length lambda-expr) 3)
                   (list? (list-ref lambda-expr 2))
                   (not (empty? (list-ref lambda-expr 2)))
                   (equal? (car (list-ref lambda-expr 2)) 'begin))

              (cdar (drop lambda-expr 2))

              (drop lambda-expr 2)))

        (define lowered-expression
          (append (list 'define-syntax (cons name-expr (cadr lambda-expr))) list-of-expressions))

        ;; Body exprs
        (parse-def-syntax lowered-expression phase1-env)]

       ;; A set! transformer, which a `set!` of the name is given to too
       [(and (list? lambda-expr)
             (member (car lambda-expr) '(make-set!-transformer make-variable-transformer))
             (= (length lambda-expr) 2))
        (#%register-set!-transformer! (symbol->string name-expr))
        (parse-def-syntax (list 'define-syntax name-expr (cadr lambda-expr)) phase1-env)]

       [(and (list? lambda-expr) (equal? (car lambda-expr) 'syntax-rules))

        (define rules (list-ref lambda-expr 1))

        (define lowered-expression
          (append (list 'define-syntax (cons name-expr '(stx)))
                  (list (append (list 'syntax-case 'stx rules) (drop lambda-expr 2)))))

        (parse-def-syntax lowered-expression phase1-env)]

       ;; Any other expression that makes a transformer, like a let around a lambda or a call
       ;; of a phase 1 function. It's evaluated once, at the first use.
       [else
        (define key (symbol->string (gensym-sym 'transformer)))
        (parse-def-syntax (list 'define-syntax
                                (list name-expr '#%transformer-input)
                                (list (list '#%transformer-once key (list 'lambda '() lambda-expr))
                                      '#%transformer-input))
                          phase1-env)])]

    [else (error "internal compilation error")]))

(#%define-syntax
 (define-syntax expression)
 (define unwrapped (#%syntax->datum/kernel expression))
 ;; Just register a syntax transformer?
 ; (stdout-simple-displayln "DEFINE SYNTAX START")
 ; (stdout-simple-displayln unwrapped)
 (define originating-file (syntax-originating-file expression))
 (define env (or originating-file "default"))
 (define func (parse-def-syntax unwrapped (#%phase1-environment env)))
 ; (define func
 ;   (if (and (list? unwrapped) (not (empty? unwrapped)) (equal? (car unwrapped) 'begin))
 ;       (parse-def-syntax (cdr unwrapped))
 ;       (parse-def-syntax unwrapped)))
 (define name-expr (list-ref unwrapped 1))
 (define name
   (if (list? name-expr)
       (list-ref name-expr 0)
       name-expr))
 (if (equal? env "default")
     (begin
       (eval `(define ,name ,func))
       ;; Register into the top environment
       (register-macro-transformer! name env))

     ; (define top-level-exists? (with-handler (lambda (_) #f) (eval 'top-level)))
     ; (stdout-simple-displayln top-level-exists?)

     (with-handler (lambda (e1)

                     (with-handler (lambda (e2)
                                     ;; We failed to find an existing environment to install it in.
                                     (eval `(define top-level (hash (quote ,name) ,func)))
                                     (register-macro-transformer! name "top-level"))
                                   ;; We failed to find an existing environment to install it in.
                                   (eval `(set! top-level
                                                (%proto-hash-insert% top-level (quote ,name) ,func)))
                                   (register-macro-transformer! name "top-level"))

                     ;; Does this work?
                     ; (eval `(define ,name ,func))
                     'void)
                   ;;
                   (eval `(set! ,(string->symbol env)
                                (%proto-hash-insert% ,(string->symbol env) (quote ,name) ,func)))
                   (register-macro-transformer! name env)))
 'void)

(let ([transformer (parse-def-syntax '(define-syntax (define-values stx)
                                        (syntax-case stx ()
                                          [(_ (name ...) expr)
                                           (let ([names (syntax-e #'(name ...))]
                                                 [produced (gensym)]
                                                 [packed (gensym)])
                                             (if (= (length names) 1)
                                                 ;; One value for one name. A list of one is
                                                 ;; what `(values x)` gives, as `values` is `list`.
                                                 #`(begin
                                                     (define #,produced expr)
                                                     (define #,(car names)
                                                       (if (#%prim.list? #,produced)
                                                           (if (#%prim.null? #,produced)
                                                               #,produced
                                                               (if (#%prim.null? (#%prim.cdr #,produced))
                                                                   (#%prim.car #,produced)
                                                                   #,produced))
                                                           #,produced)))
                                                 ;; One value can be given as it is
                                                 #`(begin
                                                     (define #,produced expr)
                                                     (define #,packed
                                                       (if (#%prim.list? #,produced)
                                                           #,produced
                                                           (#%prim.list #,produced)))
                                                     #,@(map (lambda (id index)
                                                               #`(define #,id
                                                                   (#%prim.list-ref #,packed #,index)))
                                                             names
                                                             (range 0 (length names))))))]))
                                     "default")])
  (eval `(define define-values ,transformer))
  (register-macro-transformer! 'define-values "default"))

(require-builtin #%private/steel/reader as reader.)

(define *reader* (reader.new-reader))

(define (read . port)
  (if (null? port)
      (reader.#%intern (read-impl reader.reader-read-one))
      (parameterize ([current-input-port (car port)])
        (reader.#%intern (read-impl reader.reader-read-one)))))

(define (read-syntax-object . port)
  (if (null? port)
      (read-impl reader.reader-read-one-syntax-object)
      (parameterize ([current-input-port (car port)])
        (read-impl reader.reader-read-one-syntax-object))))

(define (read-impl finisher)
  (cond
    [(reader.reader-empty? *reader*)

     (if (or (#%string-input-port? (current-input-port)) (#%file-input-port? (current-input-port)))
         (begin

           (define entry (read-port-to-string (current-input-port)))
           (reader.reader-push-string *reader* entry)

           (let ([next (finisher *reader*)])
             ; (displayln next)
             (if (void? next)
                 (begin
                   (let ([maybe-next-line (read-line-from-port (current-input-port))])
                     (if (eof-object? maybe-next-line)
                         (if (or (#%string-input-port? (current-input-port))
                                 (#%file-input-port? (current-input-port)))
                             maybe-next-line
                             (begin
                               (set! *reader* (reader.new-reader))
                               (error "missing closing paren - unexpected eof")))
                         ;; If the next line is not empty,
                         (begin
                           (reader.reader-push-string *reader* maybe-next-line)
                           (read-impl finisher)))))

                 next)))

         (begin

           (define next-line (read-line-from-port (current-input-port)))
           (cond
             [(string? next-line)
              (reader.reader-push-string *reader* next-line)

              (let ([next (finisher *reader*)])
                (if (void? next)
                    (begin
                      (let ([maybe-next-line (read-line-from-port (current-input-port))])
                        (if (eof-object? maybe-next-line)
                            (if (or (#%string-input-port? (current-input-port))
                                    (#%file-input-port? (current-input-port)))
                                maybe-next-line
                                (begin
                                  (set! *reader* (reader.new-reader))
                                  (error "missing closing paren - unexpected eof")))
                            ;; If the next line is not empty,
                            (begin
                              (reader.reader-push-string *reader* maybe-next-line)
                              (read-impl finisher)))))
                    next))]

             [else next-line])))]

    ;; The reader is not empty!
    [else
     (let ([next (reader.reader-read-one *reader*)])

       (if (void? next)
           ;; TODO: Share this code with the above
           (begin
             (let ([maybe-next-line (read-line-from-port (current-input-port))])
               (if (eof-object? maybe-next-line)
                   ;; Stupid hack
                   (if (or (#%string-input-port? (current-input-port))
                           (#%file-input-port? (current-input-port)))
                       maybe-next-line
                       (begin
                         ;; TODO: drain the reader - consider a separate function for this
                         (set! *reader* (reader.new-reader))
                         (error "missing closing paren - unexpected eof")))
                   ;; If the next line is not empty,
                   (begin
                     (reader.reader-push-string *reader* maybe-next-line)
                     (read-impl finisher)))))

           next))]))
