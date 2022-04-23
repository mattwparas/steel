(define (enumerate start accum lst)
  (if (empty? lst)
      (reverse accum)
      (enumerate (+ start 1)
                 (cons (list (car lst) start)
                       accum)
                 (cdr lst))))


;; make-struct is a macro here
(define make-struct
  (lambda (struct-name fields)
    (if (not (list? fields))
        (error! "make-struct expects a list of field names, found " fields)
        void)
    (if (not (symbol? struct-name))
        (error! "make-struct expects an identifier as the first argument, found " struct-name)
        void)

    `(begin
       ;; Constructor
       (define ,struct-name (lambda ,fields (mutable-vector ___magic_struct_symbol___ (quote ,struct-name) ,@fields)))
       ;; Predicate
       (define ,(concat-symbols struct-name '?)
         (bind/c (make-function/c (make/c any/c 'any/c) (make/c boolean? 'boolean?))
                 (lambda (this) (if (mutable-vector? this)
                                    (if (eq? (mut-vector-ref this 0) ___magic_struct_symbol___)
                                        (equal? (mut-vector-ref this 1) (quote ,struct-name))
                                        #f)
                                    #f))
                 (quote ,(concat-symbols struct-name '?))))

       ;; Getters here
       ,@(map (lambda (field)
                (let ((function-name (concat-symbols struct-name '- (car field)))
                      (pred-name (concat-symbols struct-name '?)))
                  `(define ,function-name
                     (bind/c (make-function/c
                              (make/c ,pred-name (quote ,pred-name))
                              (make/c any/c 'any/c))
                             (lambda (this) (mut-vector-ref this ,(car (cdr field))))
                             (quote ,function-name)))))
              (enumerate 2 '() fields))

       ;; Setters
       ,@(map (lambda (field)
                (let ((function-name (concat-symbols 'set- struct-name '- (car field) '!))
                      (pred-name (concat-symbols struct-name '?)))
                  `(define ,function-name
                     (bind/c (make-function/c
                              (make/c ,pred-name (quote ,pred-name))
                              (make/c any/c 'any/c)
                              (make/c any/c 'any/c))
                             (lambda (this value) (vector-set! this ,(car (cdr field)) value))
                             (quote ,function-name)))))
              (enumerate 2 '() fields)))))
