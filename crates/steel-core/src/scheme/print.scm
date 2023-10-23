(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

(define (print obj)

  ;; Symbols are funny
  (when (or (symbol? obj) (list? obj))
    (display "'"))

  (#%print obj)
  (newline))

(define (#%print obj)
  (cond
    [(symbol? obj) (display (symbol->string obj))]
    [(atom? obj) (display obj)]
    [(function? obj) (display obj)]
    [(list? obj)
     (display "(")
     (#%print (car obj))
     (for-each (λ (obj)
                 (display " ")
                 (#%print obj))
               (cdr obj))
     (display ")")]

    [(struct? obj)

     (let ([printer (#%struct-property-ref obj '#:printer)])

       (cond
         [(function? printer) (printer obj #%print)]

         [else
          (display "#<")
          (display (symbol->string (#%struct-property-ref obj '#:name)))
          (display ">")]))]

    [(hash? obj)
     (display "'#hash(")
     ;; TODO: This should use the private transduce
     (let ([hash-as-list-of-pairs (transduce obj (into-list))])

       (display "(")
       (#%print (caar hash-as-list-of-pairs))
       (display " . ")
       (#%print (cadar hash-as-list-of-pairs))
       (display ")")

       (for-each (λ (obj)
                   (display " (")
                   (#%print (car obj))
                   (display " . ")
                   (#%print (list-ref obj 1))
                   (display ")"))
                 (cdr hash-as-list-of-pairs))

       (display ")"))]

    [else (error "Trying to print an unhandled object!: " obj)]))
