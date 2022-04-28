
;; Default arguments themselves need to be resolves at run time somehow.
;; The expression for the argument should be moved inside the body, and then at runtime
;; We dispatch on the arguments that are present in order
; (define (test [apple <expr>] [bananas <expr>])
;     ...)

; (test) ;; '()
; (test 10) ;; '(10)
; (test 10 20) ;; '(10 20)

;; This should translate to something like this:
; (define (test . rest)
;     ;; Bind apple to the rest of the argument
;     (let ((apple (if (try-list-ref rest 0) 
;                      (list-ref rest 0) 
;                      <expr>))
;           (bananas 
;             (if (try-list-ref rest 1)
;                 (list-ref rest 1)
;                 <expr>)))
;           body))

(define (enumerate start accum lst)
  (if (empty? lst)
      (reverse accum)
      (enumerate (+ start 1)
                 (cons (list (car lst) start)
                       accum)
                 (cdr lst))))

(define (%lambda% args body)
    (let (
          (non-default-bindings (filter (lambda (x) (not (pair? x))) args))
          (bindings
            (transduce
                ;; Now we have attached the index of the list
                ;; To the iteration
                (enumerate 0 '() args)
                ;; extract out the arguments that have a default associated
                ;; So from the argument list like so:
                ;; (a b [c <expr>] [d <expr>])
                ;; We will get out ([c <expr>] [d <expr>])
                (filtering (lambda (x) (pair? (car x))))
                ;; Map to the let form of (binding expr)
                (mapping (lambda (x)
                    ;; ( (x, expr), index )
                    ;; TODO: clean this up
                    (let ((var-name (car (car x)))
                          (expr (car (cdr (car x))))
                          (index (car (cdr x))))
                          `(,var-name (let ((,var-name (try-list-ref !!dummy-rest-arg!! ,index)))
                                            (if ,var-name ,var-name ,expr))))))
                (into-list))))
        ;; TODO: Yes I understand this violates the macro writers bill of rights
        ;; that being said I'm only doing this as a proof of concept anyway so it can be rewritten
        ;; to be simpler and put the weight on the compiler later
        (if (equal? (length args) (length non-default-bindings))
            `(lambda ,args ,body)
            ; (displayln "hello world")
            `(lambda (,@non-default-bindings . !!dummy-rest-arg!!)
                (let (,@bindings) ,body))
            ;     ,(if bindings
            ;         `(let (,@bindings) ,body)
            ;         body))
                    
                    
                    
                    )))
        


            


(%lambda% '(x y [z "applesauce"]) '(list x y z))
