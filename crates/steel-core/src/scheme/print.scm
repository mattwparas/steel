(require-builtin steel/base)
(require "#%private/steel/control"
         (for-syntax "#%private/steel/control"))

(provide displayln
         display
         #%display
         #%top-level-display
         print
         println)

(define println
  (case-lambda
    [(obj)
     (print obj)
     (newline)]
    [(obj port)
     (print obj port)
     (newline port)]))

(define print
  (case-lambda
    [(obj) (print-impl obj)]
    [(obj port)
     (parameterize ([current-output-port port])
       (print-impl obj))]))

(define (print-impl obj)
  (define cycle-collector (#%private-cycle-collector obj))

  (for-each (λ (obj)
              (when (int? (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "#")
                (simple-display (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "=")
                (#%top-level-print obj cycle-collector)
                (newline)))
            (#%private-cycle-collector-values cycle-collector))

  (#%top-level-print obj cycle-collector))

(define (ormap pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (ormap pred (cdr lst))]))

(define (#%top-level-print obj collector)
  (cond
    [(symbol? obj)
     (simple-display "'")
     (let* ([sym (symbol->string obj)]
            [lst (string->list sym)])
       (if (ormap char-whitespace? lst)
           (begin
             (simple-display "|")
             (for-each (λ (x)
                         (when (char=? x #\|)
                           (simple-display "\\"))
                         (simple-display x))
                       lst)
             (simple-display "|"))
           (simple-display sym)))]
    [(char? obj) (write obj)]
    [(string? obj) (write obj)]
    [(atom? obj) (simple-display obj)]
    [(list? obj)
     (simple-display "'(")
     (when (not (empty? obj))
       (#%print (car obj) collector)
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 (cdr obj)))
     (simple-display ")")]
    [(pair? obj)
     (simple-display "'(")
     (#%print (car obj) collector)
     (simple-display " . ")
     (#%print (cdr obj) collector)
     (simple-display ")")]
    [(vector? obj)
     (let ([list-obj (vector->list obj)])
       (simple-display "'#(")
       (when (not (empty? list-obj))
         (#%print (car list-obj) collector)
         (for-each (λ (x)
                     (simple-display " ")
                     (#%print x collector))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(set? obj)
     (let ([list-obj (hashset->list obj)])
       (simple-display "(set")
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 list-obj)
       (simple-display ")"))]
    [(hash? obj)
     (let ([list-obj (transduce obj (into-list))])
       (simple-display "'#hash(")
       (when (not (empty? list-obj))
         (simple-display "(")
         (#%print (caar list-obj) collector)
         (simple-display " . ")
         (#%print (cadar list-obj) collector)
         (simple-display ")")
         (for-each (λ (obj)
                     (simple-display "(")
                     (#%print (car obj) collector)
                     (simple-display " . ")
                     (#%print (cdr obj) collector)
                     (simple-display ")"))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(#%private-struct? obj)
     (let ([printer (#%struct-property-ref obj '#:printer)])
       (if (function? printer)
           (printer obj (λ (x) (#%print x collector)))
           (simple-display obj)))]
    [else (simple-display obj)]))

(define (#%print obj collector)
  (cond
    [(symbol? obj)
     (let* ([sym (symbol->string obj)]
            [lst (string->list sym)])
       (if (ormap char-whitespace? lst)
           (begin
             (simple-display "|")
             (for-each (λ (x)
                         (when (char=? x #\|)
                           (simple-display "\\"))
                         (simple-display x))
                       lst)
             (simple-display "|"))
           (simple-display sym)))]
    [(char? obj) (write obj)]
    [(string? obj) (write obj)]
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
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 (cdr obj)))
     (simple-display ")")]
    [(pair? obj)
     (simple-display "(")
     (#%print (car obj) collector)
     (simple-display " . ")
     (#%print (cdr obj) collector)
     (simple-display ")")]
    [(vector? obj)
     (let ([list-obj (vector->list obj)])
       (simple-display "#(")
       (when (not (empty? list-obj))
         (#%print (car list-obj) collector)
         (for-each (λ (x)
                     (simple-display " ")
                     (#%print x collector))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(set? obj)
     (let ([list-obj (hashset->list obj)])
       (simple-display "(set")
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 list-obj)
       (simple-display ")"))]
    [(hash? obj)
     (let ([list-obj (transduce obj (into-list))])
       (simple-display "#hash(")
       (when (not (empty? list-obj))
         (simple-display "(")
         (#%print (caar list-obj) collector)
         (simple-display " . ")
         (#%print (cadar list-obj) collector)
         (simple-display ")")
         (for-each (λ (obj)
                     (simple-display "(")
                     (#%print (car obj) collector)
                     (simple-display " . ")
                     (#%print (cdr obj) collector)
                     (simple-display ")"))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(#%private-struct? obj)
     (let ([printer (#%struct-property-ref obj '#:printer)])
       (if (function? printer)
           (printer obj (λ (x) (#%print x collector)))
           (simple-display obj)))]
    [else (simple-display obj)]))

(define (display-impl obj)
  ;; Collect cycles
  (define cycle-collector (#%private-cycle-collector obj))

  (for-each (λ (obj)
              (when (int? (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "#")
                (simple-display (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "=")
                (#%top-level-display obj cycle-collector)
                (newline)))
            (#%private-cycle-collector-values cycle-collector))

  (#%top-level-display obj cycle-collector))

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

(define (#%top-level-display obj collector)
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
       (#%display (car obj) collector)
       (for-each (λ (obj)
                   (simple-display " ")
                   (#%display obj collector))
                 (cdr obj)))
     (simple-display ")")]

    [(#%private-struct? obj)

     (let ([printer (#%struct-property-ref obj '#:printer)])

       (cond
         [(function? printer) (printer obj (lambda (x) (#%display x collector)))]

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

          (#%display (car set-as-list) collector)
          (for-each (λ (obj)
                      (simple-display " ")
                      (#%display obj collector)
                      collector)
                    (cdr set-as-list))
          (simple-display ")"))])]

    [(vector? obj)
     (let ([list-obj (vector->list obj)])
       (simple-display "#(")
       (when (not (empty? list-obj))
         (#%display (car list-obj) collector)
         (for-each (λ (obj)
                     (simple-display " ")
                     (#%display obj collector))
                   (cdr list-obj)))
       (simple-display ")"))]

    [(hash? obj)
     (simple-display "#hash(")
     ;; TODO: This should use the private transduce
     (let ([hash-as-list-of-pairs (transduce obj (into-list))])

       (cond
         [(empty? hash-as-list-of-pairs) (simple-display ")")]
         [else

          (simple-display "(")
          (#%display (caar hash-as-list-of-pairs) collector)
          (simple-display " . ")
          (#%display (cadar hash-as-list-of-pairs) collector)
          (simple-display ")")

          (for-each (λ (obj)
                      (simple-display " (")
                      (#%display (car obj) collector)
                      (simple-display " . ")
                      (#%display (list-ref obj 1) collector)
                      (simple-display ")"))
                    (cdr hash-as-list-of-pairs))

          (simple-display ")")]))]

    [else (simple-display obj)]))

(define (#%display obj collector)
  (cond
    [(string? obj) (simple-display obj)]
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
       (#%display (car obj) collector)
       (for-each (λ (obj)
                   (simple-display " ")
                   (#%display obj collector))
                 (cdr obj)))
     (simple-display ")")]

    [(#%private-struct? obj)

     (let ([printer (#%struct-property-ref obj '#:printer)])

       (cond
         [(function? printer) (printer obj (lambda (x) (#%display x collector)))]
         [printer
          (simple-display "#<")
          (simple-display (symbol->string (#%struct-property-ref obj '#:name)))
          (simple-display ">")]

         [else (simple-display obj)]))]

    [(vector? obj)
     (let ([list-obj (vector->list obj)])
       (simple-display "#(")
       (when (not (empty? list-obj))
         (#%display (car list-obj) collector)
         (for-each (λ (obj)
                     (simple-display " ")
                     (#%display obj collector))
                   (cdr list-obj)))
       (simple-display ")"))]

    [(set? obj)
     (cond
       [(= (hashset-length obj) 0) (simple-display "(set)")]
       [else
        (simple-display "(set ")

        (let ([set-as-list (hashset->list obj)])

          (#%display (car set-as-list) collector)
          (for-each (λ (obj)
                      (simple-display " ")
                      (#%display obj collector)
                      collector)
                    (cdr set-as-list))
          (simple-display ")"))])]

    [(hash? obj)
     (simple-display "#hash(")
     ;; TODO: This should use the private transduce
     (let ([hash-as-list-of-pairs (transduce obj (into-list))])

       (cond
         [(empty? hash-as-list-of-pairs) (simple-display ")")]
         [else

          (simple-display "(")
          (#%display (caar hash-as-list-of-pairs) collector)
          (simple-display " . ")
          (#%display (cadar hash-as-list-of-pairs) collector)
          (simple-display ")")

          (for-each (λ (obj)
                      (simple-display " (")
                      (#%display (car obj) collector)
                      (simple-display " . ")
                      (#%display (list-ref obj 1) collector)
                      (simple-display ")"))
                    (cdr hash-as-list-of-pairs))

          (simple-display ")")]))]

    [else (simple-display obj)]))
