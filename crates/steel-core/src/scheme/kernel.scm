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

;; TODO: This needs to be updated with the current module during execution.
;; that way, `syntax-case` can go ahead and check which module to expand
;; from. It also needs a way to dynamically add itself to that module hash.
(define #%loading-current-module "default")

;; Snag the current environment
(define (current-env)
  #%loading-current-module)

(define-syntax #%syntax-transformer-module
  (syntax-rules (provide)

    [(#%syntax-transformer-module name (provide ids ...) funcs ...)
     (define (datum->syntax name)
       (let ()
         (begin
           funcs ...)
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

    [(#%define-syntax (name arg) exprs ...)
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

(define (identifier? x)
  (symbol? (syntax-e x)))

(define (all-but-last l)
  (reverse (cdr (reverse l))))

(#%define-syntax (struct expr)
                 (define unwrapped (syntax-e expr))
                 (define struct-name (syntax->datum (second unwrapped)))
                 (define fields (syntax->datum (third unwrapped)))
                 (define options
                   (let ([raw (cdddr unwrapped)])
                     ; (displayln raw)
                     (if (empty? raw) raw (map syntax->datum raw))))
                 (define result (struct-impl struct-name fields options))
                 (syntax/loc result
                   (syntax-span expr)))

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

    (define struct-options-name (concat-symbols '___ struct-name '-options___))
    (define struct-prop-name (concat-symbols 'struct: struct-name))
    (define struct-predicate (concat-symbols struct-name '?))

    `(begin
       ; (#%black-box "STRUCT" (quote ,struct-name))
       (define ,struct-options-name (hash ,@(hash->list options-map)))
       (define ,struct-name 'unintialized)
       (define ,struct-prop-name 'uninitialized)
       (define ,struct-predicate 'uninitialized)
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

       (%plain-let
        ([prototypes (make-struct-type (quote ,struct-name) ,field-count)])
        (%plain-let
         ([struct-type-descriptor (list-ref prototypes 0)] [constructor-proto (list-ref prototypes 1)]
                                                           [predicate-proto (list-ref prototypes 2)]
                                                           ;; TODO: Deprecate this
                                                           [getter-proto (list-ref prototypes 3)]
                                                           [getter-proto-list
                                                            (list-ref prototypes 4)])
         (set! ,struct-prop-name struct-type-descriptor)
         (#%vtable-update-entry! struct-type-descriptor ,maybe-procedure-field ,struct-options-name)
         ,(if mutable?
              `(set! ,struct-name
                     (lambda ,fields (constructor-proto ,@(map (lambda (x) `(#%box ,x)) fields))))

              `(set! ,struct-name constructor-proto))
         ,(new-make-predicate struct-predicate struct-name fields)
         ,@
         (if mutable? (mutable-make-getters struct-name fields) (new-make-getters struct-name fields))
         ;; If this is a mutable struct, generate the setters
         ,@(if mutable? (mutable-make-setters struct-name fields) (list))
         void)))))

(define (new-make-predicate struct-predicate-name struct-name fields)
  `(set! ,struct-predicate-name predicate-proto))

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
                (list-ref getter-proto-list ,(list-ref field 1))
                ; (lambda (this) (getter-proto this ,(list-ref field 1)))
                ))
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

(#%define-syntax (define-values expr)
                 ; (displayln expr)
                 (define underlying (syntax-e expr))
                 (define bindings (syntax->datum (second underlying)))
                 (define expression (third underlying))
                 (define unreadable-list-name
                   (make-unreadable '#%proto-define-values-binding-gensym__))
                 `(begin
                    (define ,unreadable-list-name ,expression)
                    ,@(map (lambda (binding-index-pair)
                             `(define ,(car binding-index-pair)
                                (list-ref ,unreadable-list-name ,(list-ref binding-index-pair 1))))
                           (enumerate 0 '() bindings))))

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
(define (parse-def-syntax stx)
  ;; Name of the function
  (define name-expr (list-ref stx 1))
  ;; Syntax case expr
  (define syntax-case-expr (last stx))

  (define body-exprs (all-but-last (drop stx 2)))

  (define name (list-ref name-expr 0))
  (define param-name (list-ref name-expr 1))
  (define syntax-case-param (list-ref syntax-case-expr 1))
  (define syntax-case-syntax (list-ref syntax-case-expr 2))
  (define cases (list-ref syntax-case-expr 3))

  (define gensym-name (gensym-sym (concat-symbols '__generated- name)))

  ;; Expand to syntax-rules:
  (define fake-syntax-rules
    `(define-syntax ,gensym-name
       (syntax-rules ,syntax-case-syntax
         ,cases)))

  ;; This needs to be eval'd right away so that we can actually
  ;; reference the values.
  (eval fake-syntax-rules)

  (define conditions (take-every-other cases))

  ;; List of cases, in order, for the syntax rules
  (define matched-case-bindings (#%macro-case-bindings (symbol->string gensym-name)))

  (define generated-function-name (gensym-sym '#%func))

  (define expressions (transduce matched-case-bindings (zipping conditions) (into-list)))

  (define expansion-func
    ;; TODO: gensym this!
    `(define ,generated-function-name
       (lambda #%#%args
         ,@body-exprs
         (apply (case-lambda
                  ,@expressions)
                #%#%args))))

  (define generated-match-function
    `(lambda (expr)
       ;; Get the bindings first
       (define result (#%match-syntax-case ,(symbol->string gensym-name) expr))
       (define index (list-ref result 0))
       (define case-bindings (list-ref result 1))
       (define binding-kind (list-ref result 2))
       ;; Then, calculate the one that we've matched:
       ; (define matched-case-expr (list-ref (quote ,conditions) index))

       (define matched-bindings (list-ref (quote ,matched-case-bindings) index))

       ; (define func-args (hash-keys->list case-bindings))
       (define application-args (map (lambda (x) (hash-ref case-bindings x)) matched-bindings))

       ; (define template-func (eval (list 'lambda func-args matched-case-expr)))
       (define template-result (apply ,generated-function-name application-args))
       ;; Time to run expansions:
       (#%expand-syntax-case template-result case-bindings binding-kind)))

  (displayln expansion-func)
  (eval expansion-func)
  (displayln generated-match-function)

  generated-match-function)

(#%define-syntax (define-syntax expression)
                 (define unwrapped (syntax->datum expression))
                 ;; Just register a syntax transformer?
                 (define func (parse-def-syntax unwrapped))
                 (define name (list-ref (list-ref unwrapped 1) 0))
                 (define originating-file (syntax-originating-file expression))
                 (define env (or originating-file "default"))
                 ; (displayln "Loading: " (current-env))
                 (register-macro-transformer! name env)
                 ;; Update the environment in place if it exists?
                 ; (eval `(define ,name ,func))
                 (if (equal? env "default")
                     (eval `(define ,name ,func))
                     (begin
                       (eval `(set! ,(string->symbol env)
                                    (%proto-hash-insert% ,(string->symbol env) ,name ,func)))))
                 'void)
