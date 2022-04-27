
;; Default arguments themselves need to be resolves at run time somehow.
;; The expression for the argument should be moved inside the body, and then at runtime
;; We dispatch on the arguments that are present in order
(define (test [apple <expr>] [bananas <expr>])
    ...)

(test) ;; '()
(test 10) ;; '(10)
(test 10 20) ;; '(10 20)

;; This should translate to something like this:
(define (test . rest)
    ;; Bind apple to the rest of the argument
    (let ((apple (if (try-list-ref rest 0) 
                     (list-ref rest 0) 
                     <expr>))
          (bananas 
            (if (try-list-ref rest 1)
                (list-ref rest 1)
                <expr>)))
          body))

