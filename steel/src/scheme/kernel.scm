;; Enumerate the given list starting at index `start`
(define (enumerate start accum lst)
  (if (empty? lst)
      (reverse accum)
      (enumerate (+ start 1)
                 (cons (list (car lst) start)
                       accum)
                 (cdr lst))))

(define (hash->list hm)
  (transduce (transduce hm (into-list))
             (mapping (lambda (pair)
                        ;; If we have a symbol as a key, that means we need to
                        ;; quote it before we put it back into the map
                        (if (symbol? (car pair))
                            ;; TODO: @Matt - this causes a parser error
                            ;; (cons `(quote ,(car x)) (cdr x))
                            (list (list 'quote (car pair))
                                  (list 'quote (cadr pair)))
                            pair)))
             (flattening)
             (into-list)))

(define (mutable-keyword? x) (equal? x '#:mutable))
(define (transparent-keyword? x) (equal? x '#:transparent))

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
(define (struct struct-name fields . options)

  ;; Add a field for storing the options.
  (define field-count (+ 1 (length fields)))
  ;; Mark whether this is actually a mutable and transparent struct, and
  ;; then drain the values from the
  (define mutable? (contains? mutable-keyword? fields))
  (define transparent? (contains? transparent-keyword? fields))
  (define options-without-single-keywords
    (transduce options
               (filtering (lambda (x) (not (mutable-keyword? x))))
               (filtering (lambda (x) (not (transparent-keyword? x))))
               (into-list)))

  (define extra-options (hash '#:mutable mutable?
                              '#:transparent transparent?
                              '#:fields fields))

  (when (not (list? fields))
    (error! "make-struct expects a list of field names, found " fields))

  (when (not (symbol? struct-name))
    (error! "make-struct expects an identifier as the first argument, found "
            struct-name))

  (when (odd? (length options-without-single-keywords))
    (error! "make-struct options are malformed - each option requires a value"))

  ;; Update the options-map to have the fields included
  (let* ((options-map (apply hash options-without-single-keywords))
         (options-map (hash-union options-map extra-options)))
    `(begin
       (define ,(concat-symbols '___ struct-name '-options___)
         (hash ,@(hash->list options-map)))
       (define ,struct-name 'unintialized)
       (define ,(concat-symbols struct-name '?) 'uninitialized)
       ,@(map (lambda (field) `(define ,(concat-symbols struct-name '- field)
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

       (let ((prototypes (make-struct-type (quote ,struct-name) ,field-count)))
         (let ((constructor-proto (list-ref prototypes 0))
               (predicate-proto (list-ref prototypes 1))
               (getter-proto (list-ref prototypes 2))
               (setter-proto (list-ref prototypes 3)))

           ,(new-make-constructor struct-name fields)
           ,(new-make-predicate struct-name fields)
           ,@(new-make-getters struct-name fields)
           ;; If this is a mutable struct, generate the setters
           ,@(if mutable? (new-make-setters struct-name fields) (list))
           void)))))


(define (new-make-predicate struct-name fields)
  `(set! ,(concat-symbols struct-name '?) predicate-proto))


(define (new-make-constructor struct-name fields)
  `(set! ,struct-name
         (lambda ,fields
           (constructor-proto
            ,(concat-symbols '___ struct-name '-options___) ,@fields))))

(define (new-make-getters struct-name fields)
  (map (lambda (field)
         `(set! ,(concat-symbols struct-name '- (car field))
                (lambda (this) (getter-proto this ,(list-ref field 1)))))
       (enumerate 0 '() fields)))

(define (new-make-setters struct-name fields)
  (map (lambda (field)
         `(set! ,(concat-symbols 'set- struct-name '- (car field) '!)
                (lambda (this value)
                  (setter-proto this ,(list-ref field 1) value))))
       (enumerate 0 '() fields)))


;; TODO: This is going to fail simply because when re-reading in the body of
;; expanded functions. The parser is unable to parse already made un-parseable
;; items. In this case, we should not re-parse the items, but rather
;; convert the s-expression BACK into a typed ast instead.
(define (default-loop args found-pair)
  (cond [(empty? args) #f]
        [(pair? (car args)) (default-loop (cdr args) #t)]
        [else
         (if found-pair
             #t
             (default-loop (cdr args) #f))]))


(define (non-default-after-default? args)
  (default-loop args #f))


(define (%test-lambda% args body)
                                        ;   (->/c non-default-after-default? any/c any/c)
  (when (non-default-after-default? args)
    (error! "Non default argument occurs after a default argument"))
  (let (
        (args-len (length args))
        (non-default-bindings (filter (lambda (x) (not (pair? x))) args))
        (bindings
         (transduce
          ;; Now we have attached the index of the list to the iteration
          args
          ;; extract out the arguments that have a default associated
          ;; So from the argument list like so:
          ;; (a b [c <expr>] [d <expr>])
          ;; We will get out ([c <expr>] [d <expr>])
          (filtering (lambda (x) (pair? x)))
          (enumerating)
          ;; Map to the let form of (binding expr)
          (mapping (lambda (x)
                     ;; ( (x, expr), index )
                     ;; TODO: clean this up
                     (let ((var-name (car (list-ref x 1)))
                           (expr (car (cdr (list-ref x 1))))
                           (index (car x)))
                       `(,var-name (let ((,var-name (try-list-ref !!dummy-rest-arg!! ,index)))
                                     (if ,var-name ,var-name ,expr))))))
          (into-list))))

                                        ; (displayln bindings)

    ;; TODO: Yes I understand this violates the macro writers bill of rights
    ;; that being said I'm only doing this as a proof of concept anyway so it can be rewritten
    ;; to be simpler and put the weight on the compiler later
    (if (equal? args-len (length non-default-bindings))
        `(lambda ,args ,body)
                                        ; (displayln "hello world")
        `(lambda (,@non-default-bindings . !!dummy-rest-arg!!)
                                        ;  (displayln !!dummy-rest-arg!!)
           (if (> (+ ,(length non-default-bindings) (length !!dummy-rest-arg!!))
                  ,args-len)
               (error! "Arity mismatch - function expected " ,args-len)
               void)
           (let (,@bindings) ,body)))))



                                        ; (define (keyword? symbol)
                                        ;     (unless (symbol? symbol) (return! #false))
                                        ;     (let ((symbol-as-list (-> symbol (symbol->string) (string->list))))
                                        ;       (and (equal? (list-ref symbol-as-list 0) #\#)
                                        ;            (equal? (list-ref symbol-as-list 1) #\:))))

(define (keyword? symbol)
  (and (symbol? symbol) (-> symbol (symbol->string) (starts-with? "#:"))))

(define (drop-while pred? lst)
  (cond [(empty? lst) lst]
        [(pred? (car lst)) (drop-while pred? (cdr lst))]
        [else lst]))

(define (take-while-accum pred? lst accum)
  (cond [(empty? lst) accum]
        [(pred? (car lst)) (take-while-accum pred? (cdr lst) (cons (car lst) accum))]
        [else accum]))

(define (take-while pred? lst)
  (reverse (take-while-accum pred? lst '())))


(define (all func lst)
  (if (null? lst)
      #t
      (if (func (car lst))
          (all func (cdr lst))
          #f)))

(define (contains? pred? lst)
                                        ; (displayln lst)
  (cond [(empty? lst) #f]
        [(pred? (car lst)) #t]
        [else (contains? pred? (cdr lst))]))

(define (contains-keywords? args)
  (contains? keyword? args))

(define (contains-defaults? args)
  (contains? pair? args))

(define (%better-lambda% args body)
  (cond [(contains-keywords? args) (%lambda-keyword% args body)]
        [(contains-defaults? args) (%test-lambda% args body)]
        [else => `(#%plain-lambda ,args ,body)]))

(define (%lambda-keyword% args body)
  ;; TODO: Using define here causes a bug with the internal define expansion
                                        ; (define keyword-args (drop-while (lambda (x) (not (keyword? x))) args))

  (define keyword-args (drop-while (lambda (x) (not (keyword? x))) args))
  (when (odd? (length keyword-args))
    (error! "keyword arguments malformed - each option requires a value"))

  (define non-keyword-args (take-while (lambda (x) (not (keyword? x))) args))
  (define keyword-map (apply hash keyword-args))
  (when (not (all keyword? (hash-keys->list keyword-map)))
    (error! "Non keyword arguments found after the first keyword argument"))

  (define bindings
    (transduce
     keyword-map
     (mapping (lambda (x)
                (let* ((keyword (list-ref x 0))
                       (original-var-name (list-ref x 1))
                       (expr (if (pair? original-var-name)
                                 (list-ref original-var-name 1)
                                 original-var-name))
                       (var-name (if (pair? original-var-name)
                                     (list-ref original-var-name 0)
                                     original-var-name)))

                  `(,var-name (let ((,var-name (hash-try-get !!dummy-rest-arg!! (quote ,keyword))))
                                (if (hash-contains? !!dummy-rest-arg!! (quote ,keyword))
                                    ,var-name
                                    (if
                                     ,(pair? original-var-name)
                                     ,expr
                                     (error!
                                      "Function application missing required keyword argument: "
                                      (quote ,keyword)))))))))
     (into-list)))

  `(lambda (,@non-keyword-args . !!dummy-rest-arg!!)
     (let ((!!dummy-rest-arg!! (apply hash !!dummy-rest-arg!!)))
       (let (,@bindings) ,body))))


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
