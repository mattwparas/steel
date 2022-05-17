

(define (keyword? symbol)
    (unless (symbol? symbol) (return! #false))
    (equal? (-> symbol 
                (symbol->string)
                (string->list)
                (car))
            #\:))

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



; (drop-while (lambda (x) (not (keyword? x))) '(applesauce bananas :keyword blagh))

; (member )

(define (%lambda-keyword% args body)
    ;; TODO: Using define here causes a bug with the internal define expansion
    ; (define keyword-args (drop-while (lambda (x) (not (keyword? x))) args))

    (let ((keyword-args (drop-while (lambda (x) (not (keyword? x))) args))
          (non-keyword-args (take-while (lambda (x) (not (keyword? x))) args)))
        (when (odd? (length keyword-args))
            (error! "keyword arguments malformed - each option requires a value"))

        (let ((keyword-map (apply hash keyword-args)))
            (when (not (all keyword? (hash-keys->list keyword-map)))
                (error! "Non keyword arguments found after the first keyword argument"))

            (displayln keyword-args)
            (displayln non-keyword-args)

            (let ((bindings 
                    (transduce keyword-map
                       (mapping (lambda (x)
                            (let* ((keyword (list-ref x 0))
                                  (original-var-name (list-ref x 1)))
                                  (expr (if (pair? var-name) (list-ref var-name 1) original-var-name))
                                  (var-name (if (pair? var-name) (list-ref var-name 0) original-var-name)))

                                (displayln keyword)
                                (displayln var-name)
                                (displayln expr)

                                `(,var-name (let ((,var-name (hash-try-get !!dummy-rest-arg!! (quote ,keyword))))
                                              (if ,var-name
                                                  ,var-name
                                                  (if 
                                                    ,(pair? original-var-name) ,expr 
                                                    (error! "Function application missing required keyword argument: " (quote ,keyword))))))))
                       (into-list))))
                `(lambda (,@non-keyword-args . !!dummy-rest-arg!!)
                    (let ((!!dummy-rest-arg!! (apply hash !!dummy-rest-arg!!)))
                        (let (,@bindings) ,body)))))))



;   (when (odd? (length options))
;     (error! "make-struct options are malformed - each option requires a value"))
