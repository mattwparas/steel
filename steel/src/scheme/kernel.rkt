(define (enumerate start accum lst)
  (if (empty? lst)
      (reverse accum)
      (enumerate (+ start 1)
                 (cons (list (car lst) start)
                       accum)
                 (cdr lst))))

;; make-struct is a macro here
(define make-struct (lambda (struct-name fields)
                      (cons 'begin
                            (append
                             (cons
                              ;; Constructor
                              ;; Can also add the contract here, requires having the predicate as well
                              (list 'define struct-name (list 'lambda fields
                                                              (append (list
                                                                       'vector
                                                                       ;; This is the magic tag that we use to check if this is an actual vector or not
                                                                       '___magic_struct_symbol___
                                                                       (list 'quote struct-name)) fields)))
                              ;; TODO: predicate goes here
                              (cons
                               (list 'define (concat-symbols struct-name '?)
                                     (list 'bind/c
                                           '(make-function/c
                                             (make/c any/c 'any/c)
                                             (make/c boolean? 'boolean?))
                                           '(lambda (this)
                                              (if (vector? this)
                                                  (eq? (vector-ref this 0) ___magic_struct_symbol___)
                                                  #f))
                                           (list 'quote (concat-symbols struct-name '?))))

                               (list)

                               ; (map (lambda (field)
                               ;     (list 'define
                               ;             (concat-symbols 'set struct-name '- (car field))
                               ;             (list 'lambda
                               ;                 '(this)
                               ;                 (list 'vector-ref 'this (car (cdr field))))))
                               ; (enumerate 1 '() fields))

                               )


                              )


                             ;; Getters go here
                             (map (lambda (field)
                                    (list 'define
                                          (concat-symbols struct-name '- (car field))
                                          (list 'bind/c
                                                (list 'make-function/c
                                                  (list 'make/c (concat-symbols struct-name '?) (list 'quote (concat-symbols struct-name '?)))
                                                  '(make/c any/c 'any/c))
                                                (list 'lambda '(this) (list 'vector-ref 'this (car (cdr field))))
                                                (list 'quote (concat-symbols struct-name '- (car field))))))
                                    (enumerate 1 '() fields)))
                            )))
