(define (enumerate start accum lst)
  (if (empty? lst)
      (reverse accum)
      (enumerate (+ start 1)
                 (cons (list (car lst) start)
                       accum)
                 (cdr lst))))


;; make-struct is a macro here
(define make-struct (lambda (struct-name fields)
                      (if (not (list? fields))
                          (error! "make-struct expects a list of field names, found " fields)
                          void)
                      (if (not (symbol? struct-name))
                          (error! "make-struct expects an identifier as the first argument, found " struct-name)
                          void)

                      (cons 'begin
                            (append
                             (cons
                              `(define ,struct-name (lambda ,fields (vector ___magic_struct_symbol___ (quote ,struct-name) ,@fields)))
                              ;; Constructor
                              ; (list 'define struct-name (list 'lambda fields
                              ;                                 (append (list
                              ;                                          'vector
                              ;                                          ;; This is the magic tag that we use to check if this is an actual vector or not
                              ;                                          '___magic_struct_symbol___
                              ;                                          (list 'quote struct-name)) fields)))
                              ;; TODO: predicate goes here
                              (cons
                               `(define ,(concat-symbols struct-name '?) 
                                    (bind/c (make-function/c (make/c any/c 'any/c) (make/c boolean? 'boolean?))
                                      (lambda (this) (if (vector? this)
                                                         (if (eq? (vector-ref this 0) ___magic_struct_symbol___)
                                                             (equal? (vector-ref this 1) (quote ,struct-name))
                                                             #f)
                                                        #f))
                                      (quote ,(concat-symbols struct-name '?))))
                              ;  (list 'define (concat-symbols struct-name '?)
                              ;        (list 'bind/c
                              ;              '(make-function/c
                              ;                (make/c any/c 'any/c)
                              ;                (make/c boolean? 'boolean?))
                              ;              (list 'lambda '(this)
                              ;                 (list 'if '(vector? this)
                              ;                     (list 'if 
                              ;                         '(eq? (vector-ref this 0) ___magic_struct_symbol___)
                              ;                         (list 'equal? '(vector-ref this 1) (list 'quote struct-name))
                              ;                         #f)
                              ;                     #f))
                              ;              (list 'quote (concat-symbols struct-name '?))))

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

                             ;; Getters here
                             (map (lambda (field)
                                    (let ((function-name (concat-symbols struct-name '- (car field)))
                                          (pred-name (concat-symbols struct-name '?)))
                                      `(define ,function-name
                                         (bind/c (make-function/c
                                                  (make/c ,pred-name (quote ,pred-name))
                                                  (make/c any/c 'any/c))
                                                 (lambda (this) (vector-ref this ,(car (cdr field))))
                                                 (quote ,function-name)))))
                                  (enumerate 2 '() fields)))




                                   ;; `(define ,(concat-symbols struct-name '- (car field))
                                  ;;     (bind/c (make-function/c
                                  ;;              (make/c ,(concat-symbols struct-name '?) (quote ,(concat-symbols struct-name '?)))
                                  ;;              (make/c any/c 'any/c))
                                  ;;             (lambda (this) (vector-ref this ,(car (cdr field))))
                                  ;;             (quote ,(concat-symbols struct-name '- (car field))))))
                                  ;; (enumerate 2 '() fields)))


                             ;; Getters go here
                             ;; (map (lambda (field)
                             ;;        (list 'define
                             ;;              (concat-symbols struct-name '- (car field))
                             ;;              (list 'bind/c
                             ;;                    (list 'make-function/c
                             ;;                      (list 'make/c (concat-symbols struct-name '?) (list 'quote (concat-symbols struct-name '?)))
                             ;;                      '(make/c any/c 'any/c))
                             ;;                    (list 'lambda '(this) (list 'vector-ref 'this (car (cdr field))))
                             ;;                    (list 'quote (concat-symbols struct-name '- (car field))))))
                             ;;        (enumerate 2 '() fields)))
                            )))
