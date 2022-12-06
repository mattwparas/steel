; #lang racket

(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ; ((quasiquote (quote (unquote x)))                 (quote (quasiquote (unquote x))))
    ((quasiquote (unquote x))                         x)
    ; ((quasiquote ((unquote x) xs))              (cons x (quasiquote xs)))
    ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
    ((quasiquote ((unquote-splicing x)))        (append x '()))
    ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
    ; ((quasiquote (x))                          '(x))
    ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
    ; ((quasiquote (quote (unquote x)))          (quote x))
    ((quasiquote x)                           'x)))


(define struct-name 'Applesauce)
(define fields '(a b c))

;; TODO: This actually expands using the reader macro from the parser which expands fancy defines
;;
`(define ,struct-name (lambda ,fields (vector __magic_struct_symbol__ (quote ,struct-name) ,@fields)))

(quasiquote (define (unquote struct-name)
           (lambda (unquote fields)
             (vector __magic_struct_symbol__
                     (quote (unquote struct-name))
                     (unquote-splicing fields)))))

(define field '(getter 2))

;; TODO: this parses the define incorrectly - it should go to an untyped AST and not a typed one
`(define ,(concat-symbols struct-name '- (car field))
                                      (bind/c (make-function/c
                                               (make/c (concat-symbols struct-name '?) (quote ,(concat-symbols struct-name '?)))
                                               (make/c any/c 'any/c))
                                              (lambda (this) (vector-ref this ,(car (cdr field))))
                                              (quote ,(concat-symbols struct-name '- (car field)))))


`(define ,(concat-symbols struct-name '?) 
                                    (bind/c (make-function/c (make/c any/c 'any/c) (make/c boolean? 'boolean?))
                                      (lambda (this) (if (vector? this)
                                                         (if (eq? (vector-ref this 0) ___magic_struct_symbol___)
                                                             (equal? (vector-ref this 1) (quote ,struct-name))
                                                             #f)
                                                        #f))
                                      (quote ,(concat-symbols struct-name '?))))


(let ((function-name (concat-symbols struct-name '- (car fields)))
                                          (pred-name (concat-symbols struct-name '?)))
                                      `(define ,function-name
                                         (bind/c (make-function/c
                                                  (make/c ,pred-name (quote ,pred-name))
                                                  (make/c any/c 'any/c))
                                                 (lambda (this) (vector-ref this ,(car (cdr fields))))
                                                 (quote ,function-name))))


;; TODO: Go _back_ to an implementation that just uses S-Expressions and thats it for internal
;; representation. Don't have special AST implementation other than in the core.
;; Another solution that will get me part of the way there, is treating quasiquote like quote in the
;; parser

(define (enumerate start accum lst)
  (if (empty? lst)
      (reverse accum)
      (enumerate (+ start 1)
                 (cons (list (car lst) start)
                       accum)
                 (cdr lst))))

(map (lambda (field)
                (let ((function-name (concat-symbols 'set- struct-name '- (car fields) '!))
                      (pred-name (concat-symbols struct-name '?)))
                  `(define ,function-name
                     (bind/c (make-function/c
                              (make/c ,pred-name (quote ,pred-name))
                              (make/c any/c 'any/c)
                              (make/c any/c 'any/c))
                             (lambda (this value) (vector-set! this ,(car (cdr field)) value))
                             (quote ,function-name)))))
              (enumerate 2 '() fields))
