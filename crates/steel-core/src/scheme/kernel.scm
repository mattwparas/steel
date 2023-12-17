;; The kernel for Steel.
;; This contains core forms that are expanded during the last phase of macro expansion
;; Macros that are exported from the kernel and applied on code externally are defined via
;; the form `(#%define-syntax <macro> <body>)`.
;;
;; This makes this function publicly available for the kernel to expand
;; forms with.

; (define *transformer-functions* (hashset))

;; Compatibility layers for making defmacro not as painful
(define displayln stdout-simple-displayln)

(define-syntax #%syntax-transformer-module
  (syntax-rules (provide)

    [(#%syntax-transformer-module name (provide ids ...) funcs ...)
     (define (datum->syntax name)
       (let ()
         funcs ...
         (#%syntax-transformer-module provide ids ...)))]

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
       (register-macro-transformer! (symbol->string 'name) "default")
       (define (name arg)
         expr))]

    [(#%define-syntax (name arg)
       exprs ...)
     (begin
       (register-macro-transformer! (symbol->string 'name) "default")
       (define (name arg)
         exprs ...))]

    [(#%define-syntax name expr)
     (begin
       (register-macro-transformer! (symbol->string 'name) "default")
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

(define (hash->list hm)
  (transduce (transduce hm (into-list))
             (mapping (lambda (pair)
                        ;; If we have a symbol as a key, that means we need to
                        ;; quote it before we put it back into the map
                        (if (symbol? (car pair))
                            ;; TODO: @Matt - this causes a parser error
                            ;; (cons `(quote ,(car x)) (cdr x))
                            (list (list 'quote (car pair)) (cadr pair))
                            pair)))
             (flattening)
             (into-list)))

(define (mutable-keyword? x)
  (equal? x '#:mutable))
(define (transparent-keyword? x)
  (equal? x '#:transparent))

(#%define-syntax (struct expr)
  (define unwrapped (syntax-e expr))
  (define struct-name (syntax->datum (second unwrapped)))
  (define fields (syntax->datum (third unwrapped)))
  (define options
    (let ([raw (cdddr unwrapped)])
      ; (displayln raw)
      (if (empty? raw) raw (map syntax->datum raw))))

  (struct-impl struct-name fields options))

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
           (display "(")
           (display (symbol->string ,(list 'quote struct-name)))
           ,@(map (lambda (field)
                    `(begin
                       (display " ")
                       (printer-function (,(concat-symbols struct-name '- field) obj))))
                  fields)

           (display ")"))

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

  ;; Update the options-map to have the fields included
  (let* ([options-map (apply hash options-without-single-keywords)]
         [options-map (hash-union options-map extra-options)]
         [options-map (if (hash-try-get options-map '#:printer)
                          options-map
                          (hash-insert options-map '#:printer default-printer-function))]
         [maybe-procedure-field (hash-try-get options-map '#:prop:procedure)])

    (when (and maybe-procedure-field (> maybe-procedure-field (length fields)))
      (error! "struct #:prop:procedure cannot refer to an index that is out of bounds"))

    `(begin
       ; (#%black-box "STRUCT" (quote ,struct-name))
       (define ,(concat-symbols '___ struct-name '-options___)
         (hash ,@(hash->list options-map)))
       (define ,struct-name 'unintialized)
       (define ,(concat-symbols 'struct: struct-name)
         'uninitialized)
       (define ,(concat-symbols struct-name '?)
         'uninitialized)
       ,@(map (lambda (field)
                `(define ,(concat-symbols struct-name '- field)
                   'uninitialized))
              fields)
       ;; If we're mutable, set up the identifiers to later be `set!`
       ;; below in the same scope
       ,@(if mutable?
             (map (lambda (field)
                    `(define ,(concat-symbols 'set- struct-name '- field '!)
                       'unintialized))
                  fields)
             (list))

       ;; TODO: Change this to plain let to see the error
       (%plain-let
        ([prototypes (make-struct-type (quote ,struct-name) ,field-count)])
        (%plain-let
         ([struct-type-descriptor (list-ref prototypes 0)] [constructor-proto (list-ref prototypes 1)]
                                                           [predicate-proto (list-ref prototypes 2)]
                                                           [getter-proto (list-ref prototypes 3)])
         (set! ,(concat-symbols 'struct: struct-name) struct-type-descriptor)
         (#%vtable-update-entry! struct-type-descriptor
                                 ,maybe-procedure-field
                                 ,(concat-symbols '___ struct-name '-options___))
         ,(if mutable?
              `(set! ,struct-name
                     (lambda ,fields (constructor-proto ,@(map (lambda (x) `(#%box ,x)) fields))))

              `(set! ,struct-name constructor-proto))
         ,(new-make-predicate struct-name fields)
         ,@
         (if mutable? (mutable-make-getters struct-name fields) (new-make-getters struct-name fields))
         ;; If this is a mutable struct, generate the setters
         ,@(if mutable? (mutable-make-setters struct-name fields) (list))
         void)))))

(define (new-make-predicate struct-name fields)
  `(set! ,(concat-symbols struct-name '?) predicate-proto))

; (define (new-make-constructor struct-name procedure-index fields)
;   `(set!
;     ,struct-name
;     (lambda ,fields
;       (constructor-proto ,(concat-symbols '___ struct-name '-options___) ,procedure-index ,@fields))))

(define (mutable-make-getters struct-name fields)
  (map (lambda (field)
         `(set! ,(concat-symbols struct-name '- (car field))
                (lambda (this) (#%unbox (getter-proto this ,(list-ref field 1))))))
       (enumerate 0 '() fields)))

(define (mutable-make-setters struct-name fields)
  (map (lambda (field)
         `(set! ,(concat-symbols 'set- struct-name '- (car field) '!)
                (lambda (this value) (#%set-box! (getter-proto this ,(list-ref field 1)) value))))
       (enumerate 0 '() fields)))

(define (new-make-getters struct-name fields)
  (map (lambda (field)
         `(set! ,(concat-symbols struct-name '- (car field))
                (lambda (this) (getter-proto this ,(list-ref field 1)))))
       (enumerate 0 '() fields)))

(define (new-make-setters struct-name fields)
  (map (lambda (field)
         `(set! ,(concat-symbols 'set- struct-name '- (car field) '!)
                (lambda (this value) (setter-proto this ,(list-ref field 1) value))))
       (enumerate 0 '() fields)))

(define (%make-memoize f)
  (lambda n
    (let ([previous-value (%memo-table-ref %memo-table f n)])
      (if (and (Ok? previous-value) (Ok->value previous-value))
          (begin
            ; (displayln "READ VALUE: " previous-value " with args " n)
            (Ok->value previous-value))
          (let ([new-value (apply f n)])
            ; (displayln "SETTING VALUE " new-value " with args " n)
            (%memo-table-set! %memo-table f n new-value)
            new-value)))))

(define *gensym-counter* 0)
(define (gensym)
  (set! *gensym-counter* (+ 1 *gensym-counter*))
  (string->symbol (string-append "##gensym" (to-string *gensym-counter*))))

;; TODO: @Matt -> for whatever reason, using ~> plus (lambda (x) ...) generates a stack overflow... look into that
(define (make-unreadable symbol)
  (~>> symbol (symbol->string) (string-append "##") (string->symbol)))

; (define (define-values bindings expr)
;   `(begin
;      (define ,(make-unreadable '#%proto-define-values-binding-gensym__)
;        ,expr)
;      ,@(map (lambda (binding-index-pair)
;               `(define ,(car binding-index-pair)
;                  ,(list-ref binding-index-pair 1)))
;             (enumerate 0 '() bindings))))

(#%define-syntax (define-values expr)
  ; (displayln expr)
  (define underlying (syntax-e expr))
  (define bindings (syntax->datum (second underlying)))
  (define expression (third underlying))

  (define unreadable-list-name (make-unreadable '#%proto-define-values-binding-gensym__))

  `(begin
     (define ,unreadable-list-name ,expression)
     ,@(map (lambda (binding-index-pair)
              `(define ,(car binding-index-pair)
                 (list-ref ,unreadable-list-name ,(list-ref binding-index-pair 1))))
            (enumerate 0 '() bindings))))

(#%define-syntax (#%better-lambda expr)
  ; (displayln "Expanding: " expr)
  ; (displayln "unwrapping one level..." (syntax-e expr))
  (quasisyntax (list 10 20 30)))

;; TODO: make this not so suspect, but it does work!
(#%define-syntax (#%current-kernel-transformers expr)
  (cons 'list (map (lambda (x) (list 'quote x)) (current-macro-transformers!))))

(#%define-syntax (#%fake-lambda expr)
  (define underlying (syntax-e expr))
  (define rest (cdr underlying))
  ; (displayln rest)
  ; (displayln (syntax-e (list-ref rest 1)))
  (cons '#%plain-lambda rest))

;; Implement defmacro!
;; Defmacro: Load functions that operate on syntax, directly into the kernel.
;; And then, call those as such in the kernel via transformers.

; (#%define-syntax )

;; TODO: Come back to this once theres something to attach it to
; (define (@doc expr comment)
;   (if (equal? (car expr) 'define)
;       `(begin
;           (define ,(concat-symbols '__doc- (second expr)) ,comment)
;           ,expr
;        )
;        expr))

;; TODO: This is going to fail simply because when re-reading in the body of
;; expanded functions. The parser is unable to parse already made un-parseable
;; items. In this case, we should not re-parse the items, but rather
;; convert the s-expression BACK into a typed ast instead.
; (define (default-loop args found-pair)
;   (cond [(empty? args) #f]
;         [(pair? (car args)) (default-loop (cdr args) #t)]
;         [else
;          (if found-pair
;              #t
;              (default-loop (cdr args) #f))]))

; (define (non-default-after-default? args)
;   (default-loop args #f))

; (define (%test-lambda% args body)
;                                         ;   (->/c non-default-after-default? any/c any/c)
;   (when (non-default-after-default? args)
;     (error! "Non default argument occurs after a default argument"))
;   (let (
;         (args-len (length args))
;         (non-default-bindings (filter (lambda (x) (not (pair? x))) args))
;         (bindings
;          (transduce
;           ;; Now we have attached the index of the list to the iteration
;           args
;           ;; extract out the arguments that have a default associated
;           ;; So from the argument list like so:
;           ;; (a b [c <expr>] [d <expr>])
;           ;; We will get out ([c <expr>] [d <expr>])
;           (filtering (lambda (x) (pair? x)))
;           (enumerating)
;           ;; Map to the let form of (binding expr)
;           (mapping (lambda (x)
;                      ;; ( (x, expr), index )
;                      ;; TODO: clean this up
;                      (let ((var-name (car (list-ref x 1)))
;                            (expr (car (cdr (list-ref x 1))))
;                            (index (car x)))
;                        `(,var-name (let ((,var-name (try-list-ref !!dummy-rest-arg!! ,index)))
;                                      (if ,var-name ,var-name ,expr))))))
;           (into-list))))

;                                         ; (displayln bindings)

;     ;; TODO: Yes I understand this violates the macro writers bill of rights
;     ;; that being said I'm only doing this as a proof of concept anyway so it can be rewritten
;     ;; to be simpler and put the weight on the compiler later
;     (if (equal? args-len (length non-default-bindings))
;         `(lambda ,args ,body)
;                                         ; (displayln "hello world")
;         `(lambda (,@non-default-bindings . !!dummy-rest-arg!!)
;                                         ;  (displayln !!dummy-rest-arg!!)
;            (if (> (+ ,(length non-default-bindings) (length !!dummy-rest-arg!!))
;                   ,args-len)
;                (error! "Arity mismatch - function expected " ,args-len)
;                void)
;            (let (,@bindings) ,body)))))

; (define (keyword? symbol)
;     (unless (symbol? symbol) (return! #false))
;     (let ((symbol-as-list (-> symbol (symbol->string) (string->list))))
;       (and (equal? (list-ref symbol-as-list 0) #\#)
;            (equal? (list-ref symbol-as-list 1) #\:))))

; (define (keyword? symbol)
;   (and (symbol? symbol) (-> symbol (symbol->string) (starts-with? "#:"))))

; (define (drop-while pred? lst)
;   (cond [(empty? lst) lst]
;         [(pred? (car lst)) (drop-while pred? (cdr lst))]
;         [else lst]))

; (define (take-while-accum pred? lst accum)
;   (cond [(empty? lst) accum]
;         [(pred? (car lst)) (take-while-accum pred? (cdr lst) (cons (car lst) accum))]
;         [else accum]))

; (define (take-while pred? lst)
;   (reverse (take-while-accum pred? lst '())))

; (define (all func lst)
;   (if (null? lst)
;       #t
;       (if (func (car lst))
;           (all func (cdr lst))
;           #f)))

; (define (contains? pred? lst)
;                                         ; (displayln lst)
;   (cond [(empty? lst) #f]
;         [(pred? (car lst)) #t]
;         [else (contains? pred? (cdr lst))]))

; (define (contains-keywords? args)
;   (contains? keyword? args))

; (define (contains-defaults? args)
;   (contains? pair? args))

; (define (%better-lambda% args body)
;   (cond [(contains-keywords? args) (%lambda-keyword% args body)]
;         [(contains-defaults? args) (%test-lambda% args body)]
;         [else => `(#%plain-lambda ,args ,body)]))

; (define (%lambda-keyword% args body)
;   ;; TODO: Using define here causes a bug with the internal define expansion
;                                         ; (define keyword-args (drop-while (lambda (x) (not (keyword? x))) args))

;   (define keyword-args (drop-while (lambda (x) (not (keyword? x))) args))
;   (when (odd? (length keyword-args))
;     (error! "keyword arguments malformed - each option requires a value"))

;   (define non-keyword-args (take-while (lambda (x) (not (keyword? x))) args))
;   (define keyword-map (apply hash keyword-args))
;   (when (not (all keyword? (hash-keys->list keyword-map)))
;     (error! "Non keyword arguments found after the first keyword argument"))

;   (define bindings
;     (transduce
;      keyword-map
;      (mapping (lambda (x)
;                 (let* ((keyword (list-ref x 0))
;                        (original-var-name (list-ref x 1))
;                        (expr (if (pair? original-var-name)
;                                  (list-ref original-var-name 1)
;                                  original-var-name))
;                        (var-name (if (pair? original-var-name)
;                                      (list-ref original-var-name 0)
;                                      original-var-name)))

;                   `(,var-name (let ((,var-name (hash-try-get !!dummy-rest-arg!! (quote ,keyword))))
;                                 (if (hash-contains? !!dummy-rest-arg!! (quote ,keyword))
;                                     ,var-name
;                                     (if
;                                      ,(pair? original-var-name)
;                                      ,expr
;                                      (error!
;                                       "Function application missing required keyword argument: "
;                                       (quote ,keyword)))))))))
;      (into-list)))

;   `(lambda (,@non-keyword-args . !!dummy-rest-arg!!)
;      (let ((!!dummy-rest-arg!! (apply hash !!dummy-rest-arg!!)))
;        (let (,@bindings) ,body))))

; (let ((keyword-args (drop-while (lambda (x) (not (keyword? x))) args))
;       (non-keyword-args (take-while (lambda (x) (not (keyword? x))) args)))
;     (when (odd? (length keyword-args))
;         (error! "keyword arguments malformed - each option requires a value"))

;     (let ((keyword-map (apply hash keyword-args)))
;         (when (not (all keyword? (hash-keys->list keyword-map)))
;             (error! "Non keyword arguments found after the first keyword argument"))

;         (let ((bindings
;                 (transduce
;                   keyword-map
;                    (mapping (lambda (x)
;                         (let* ((keyword (list-ref x 0))
;                                (original-var-name (list-ref x 1))
;                                (expr (if (pair? original-var-name) (list-ref original-var-name 1) original-var-name))
;                                (var-name (if (pair? original-var-name) (list-ref original-var-name 0) original-var-name)))

;                             `(,var-name (let ((,var-name (hash-try-get !!dummy-rest-arg!! (quote ,keyword))))
;                                           (if (hash-contains? !!dummy-rest-arg!! (quote ,keyword))
;                                               ,var-name
;                                               (if
;                                                 ,(pair? original-var-name)
;                                                 ,expr
;                                                 (error! "Function application missing required keyword argument: " (quote ,keyword)))))))))
;                    (into-list))))
;             `(lambda (,@non-keyword-args . !!dummy-rest-arg!!)
;                 (let ((!!dummy-rest-arg!! (apply hash !!dummy-rest-arg!!)))
;                     (let (,@bindings) ,body)))))))

; (define test (%lambda-keyword% (a b #:transparent [transparent #t]) (if transparent (begin (displayln "hello world") (+ a b)) (+ a b 10))))
