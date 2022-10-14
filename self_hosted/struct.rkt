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
                        ;; If we have a symbol as a key, that means we need to quote it before
                        ;; we put it back into the map
                        (if (symbol? (car pair))
                            ;; TODO: @Matt - this causes a parser error
                            ;; (cons `(quote ,(car x)) (cdr x))
                            (cons (list 'quote (car pair)) (cdr pair))
                            pair)))
             (flattening)
             (into-list)))


(define (test-make-struct struct-name fields . options)

  (define field-count (length fields))

  (when (not (list? fields))
    (error! "make-struct expects a list of field names, found " fields))

  (when (not (symbol? struct-name))
    (error! "make-struct expects an identifier as the first argument, found " struct-name))

  (when (odd? (length options))
    (error! "make-struct options are malformed - each option requires a value"))

  (let ((options-map (apply hash options)))
    `(begin
        (define ,struct-name 'unintialized)
        (define ,(concat-symbols struct-name '?) 'uninitialized)
        ,@(map (lambda (field) `(define ,(concat-symbols struct-name '- field) 'uninitialized)) fields)
        ,@(map (lambda (field) `(define ,(concat-symbols 'set- struct-name '- field '!) 'unintialized)) fields)


        (let ((prototypes (make-struct-type struct-name ,field-count)))
          (let ((constructor-proto (list-ref 0 prototypes))
                (predicate-proto (list-ref 1 prototypes))
                (getter-proto (list-ref 2 prototypes))
                (setter-proto (list-ref 3 prototypes)))

              ,(new-make-constructor struct-name)
              ,(new-make-predicate struct-name fields)
              ,@(new-make-getters struct-name fields)
              ,@(new-make-setters struct-name fields)
))
        
        )))


(define (new-make-predicate struct-name fields)
  `(set! ,(concat-symbols struct-name '?) predicate-proto))

(define (new-make-constructor struct-name)
  `(set! ,struct-name constructor-proto))


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

(test-make-struct 'Applesauce '(a b c))