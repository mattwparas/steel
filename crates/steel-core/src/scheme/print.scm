(require-builtin steel/base)
(require "#%private/steel/control"
         (for-syntax "#%private/steel/control"))

(provide displayln
         display
         #%print
         #%top-level-print)

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

(define (display-impl obj)
  ;; Collect cycles
  (define cycle-collector (#%private-cycle-collector obj))

  (for-each (λ (obj)
              (when (int? (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "#")
                (simple-display (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "=")
                (#%top-level-print obj cycle-collector)
                (newline)))
            (#%private-cycle-collector-values cycle-collector))

  ;; Symbols are funny
  (when (or (symbol? obj) (list? obj))
    (simple-display "'"))

  (#%top-level-print obj cycle-collector))

(define display
  (case-lambda
    [(obj) (display-impl obj)]
    [(obj port)
     (parameterize ([current-output-port port])
       (display-impl obj))]))

;;@doc
;; Displays the given argument(s), with a space between them, finishing with a new line.
;;
;; # Example
;; ```scheme
;; (displayln "Hello world!") ;; Prints "Hello world!"
;; (displayln "foo" "bar") ;; Prints "foo bar"
;; ```
(define (displayln . objs)

  (cond
    [(= (length objs) 1)

     (display (car objs))
     (newline)]
    [else

     (for-each (lambda (x)
                 (display x)
                 (simple-display " "))
               objs)
     (newline)]))

(define (#%top-level-print obj collector)
  (cond
    [(symbol? obj) (simple-display (symbol->string obj))]
    [(atom? obj) (simple-display obj)]
    [(function? obj) (simple-display obj)]
    ;; There is a cycle!
    ; [(int? (#%private-cycle-collector-get collector obj))
    ;  (simple-display "#" (#%private-cycle-collector-get collector obj) "#")]
    [(list? obj)
     (simple-display "(")
     (when (not (empty? obj))
       (#%print (car obj) collector)
       (for-each (λ (obj)
                   (simple-display " ")
                   (#%print obj collector))
                 (cdr obj)))
     (simple-display ")")]

    [(#%private-struct? obj)

     (let ([printer (#%struct-property-ref obj '#:printer)])

       (cond
         [(function? printer) (printer obj (lambda (x) (#%print x collector)))]

         ;; Truthiness here needs to be addressed
         [printer
          (simple-display "#<")
          (simple-display (symbol->string (#%struct-property-ref obj '#:name)))
          (simple-display ">")]

         [else (simple-display obj)]))]

    [(set? obj)
     (cond
       [(= (hashset-length obj) 0) (simple-display "(set)")]
       [else
        (simple-display "(set ")

        (let ([set-as-list (hashset->list obj)])

          (#%print (car set-as-list) collector)
          (for-each (λ (obj)
                      (simple-display " ")
                      (#%print obj collector)
                      collector)
                    (cdr set-as-list))
          (simple-display ")"))])]

    [(vector? obj)
     (let ([list-obj (vector->list obj)])
       (simple-display "'#(")
       (when (not (empty? list-obj))
         (#%print (car list-obj) collector)
         (for-each (λ (obj)
                     (simple-display " ")
                     (#%print obj collector))
                   (cdr list-obj)))
       (simple-display ")"))]

    [(hash? obj)
     (simple-display "'#hash(")
     ;; TODO: This should use the private transduce
     (let ([hash-as-list-of-pairs (transduce obj (into-list))])

       (cond
         [(empty? hash-as-list-of-pairs) (simple-display ")")]
         [else

          (simple-display "(")
          (#%print (caar hash-as-list-of-pairs) collector)
          (simple-display " . ")
          (#%print (cadar hash-as-list-of-pairs) collector)
          (simple-display ")")

          (for-each (λ (obj)
                      (simple-display " (")
                      (#%print (car obj) collector)
                      (simple-display " . ")
                      (#%print (list-ref obj 1) collector)
                      (simple-display ")"))
                    (cdr hash-as-list-of-pairs))

          (simple-display ")")]))]

    [else (simple-display obj)]))

(define (#%print obj collector)
  (cond
    [(string? obj)
     (display "\"")
     (simple-display obj)
     (display "\"")]
    [(symbol? obj) (simple-display (symbol->string obj))]
    [(atom? obj) (simple-display obj)]
    [(function? obj) (simple-display obj)]
    [(void? obj) (simple-display obj)]
    ;; There is a cycle!
    [(int? (#%private-cycle-collector-get collector obj))
     (simple-display "#")
     (simple-display (#%private-cycle-collector-get collector obj))
     (simple-display "#")]
    [(list? obj)
     (simple-display "(")
     (when (not (empty? obj))
       (#%print (car obj) collector)
       (for-each (λ (obj)
                   (simple-display " ")
                   (#%print obj collector))
                 (cdr obj)))
     (simple-display ")")]

    [(#%private-struct? obj)

     (let ([printer (#%struct-property-ref obj '#:printer)])

       (cond
         [(function? printer) (printer obj (lambda (x) (#%print x collector)))]
         [printer
          (simple-display "#<")
          (simple-display (symbol->string (#%struct-property-ref obj '#:name)))
          (simple-display ">")]

         [else (simple-display obj)]))]

    [(set? obj)
     (cond
       [(= (hashset-length obj) 0) (simple-display "(set)")]
       [else
        (simple-display "(set ")

        (let ([set-as-list (hashset->list obj)])

          (#%print (car set-as-list) collector)
          (for-each (λ (obj)
                      (simple-display " ")
                      (#%print obj collector)
                      collector)
                    (cdr set-as-list))
          (simple-display ")"))])]

    [(hash? obj)
     (simple-display "'#hash(")
     ;; TODO: This should use the private transduce
     (let ([hash-as-list-of-pairs (transduce obj (into-list))])

       (cond
         [(empty? hash-as-list-of-pairs) (simple-display ")")]
         [else

          (simple-display "(")
          (#%print (caar hash-as-list-of-pairs) collector)
          (simple-display " . ")
          (#%print (cadar hash-as-list-of-pairs) collector)
          (simple-display ")")

          (for-each (λ (obj)
                      (simple-display " (")
                      (#%print (car obj) collector)
                      (simple-display " . ")
                      (#%print (list-ref obj 1) collector)
                      (simple-display ")"))
                    (cdr hash-as-list-of-pairs))

          (simple-display ")")]))]

    [else (simple-display obj)]))
