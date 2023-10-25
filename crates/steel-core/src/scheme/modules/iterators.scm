(require-builtin steel/base)

(provide StreamIterator
         IntoIterator
         ITERATOR-FINISHED
         iter-next
         into-iter
         iter-for-each
         ;; For defining generators out of functions directly
         (for-syntax make-generator!)
         (for-syntax define/generator))

(struct StreamIterator
        (iter-instance stream-empty-function stream-first-function stream-next-function))

(struct IntoIterator (iter-object next-function) #:prop:procedure 1)

;; Use the builtin "iterator finished" symbol
(define ITERATOR-FINISHED (load-from-module! %-builtin-module-steel/meta '#%iterator-finished))

(define (iter-finished? value)
  (eq? value ITERATOR-FINISHED))

(define (iter-next into-iterator)
  (into-iterator (IntoIterator-iter-object into-iterator)))

;; Generically get the iterator
(define (into-iter obj)
  ;; Check if this is a builtin type - if so, delegate to the underlying iterator
  (define maybe-builtin-iterator (value->iterator obj))

  (if maybe-builtin-iterator
      (IntoIterator maybe-builtin-iterator iter-next!)
      ((#%struct-property-ref obj '#:prop:into-iter) obj)))

;; Call value for each thing
(define (iter-for-each iter func)

  (define next-value (iter-next iter))

  (if (iter-finished? next-value)
      void
      (begin
        (func next-value)
        (iter-for-each iter func))))

(define UNEVALUATED 'unevaluated)

(define-syntax make-generator!
  (syntax-rules (yield)

    [(make-generator! (args ...) body ...)
     (lambda (args ...)

       (let* ([iterator-object UNEVALUATED]
              [temp (#%box (lambda () (make-generator! "INTERNAL" iterator-object body ...)))])

         (set! iterator-object temp)

         ;; Produce iterator object - the iterator-object itself is not particularly
         ;; meaningful.
         (IntoIterator iterator-object
                       (lambda (generator-func) ;; Pass the function down

                         ((#%unbox generator-func)) ;; Call it
                         ))))]

    [(make-generator! (args ...) body)
     (lambda (args ...)

       ;; Unevaluated
       (define iterator-object (#%box (lambda () (make-generator! "INTERNAL" iterator-object body))))

       ;; Produce iterator object - the iterator-object itself is not particularly
       ;; meaningful.
       (IntoIterator iterator-object
                     (lambda (generator-func) ;; Pass the function down

                       ((#%unbox generator-func)) ;; Call it
                       )))]

    [(make-generator! "INTERNAL" generator-id (yield x))
     (let ([result x])
       (#%set-box! generator-id (lambda () ITERATOR-FINISHED))
       result)]

    [(make-generator! "INTERNAL" generator-id (yield x) body ...)

     ;; Freeze the result, set the next function to be the rest of the body
     (let ([result x])
       (#%set-box! generator-id (lambda () (make-generator! "INTERNAL" generator-id body ...)))
       result)]

    [(make-generator! "INTERNAL" generator-id x xs ...)

     (begin
       x
       (make-generator! "INTERNAL" generator-id xs ...))]

    [(make-generator! "INTERNAL" generator-id x) x]))

(define-syntax define/generator
  (syntax-rules ()
    [(define/generator (name args ...) body ...)
     (define name (make-generator! (args ...) body ...))]))
