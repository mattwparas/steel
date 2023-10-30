(require-builtin steel/base)

(provide StreamIterator
         IntoIterator
         ITERATOR-FINISHED
         iter-next
         into-iter
         iter-for-each
         stream-iter-for-each
         into-stream-iter
         ;; For defining generators out of functions directly
         (for-syntax make-generator!)
         (for-syntax define/generator))

(struct StreamIterator
        (iter-instance stream-empty-function stream-first-function stream-next-function))

(define (stream-iter-for-each iter func)

  (define obj-stream-empty-function (StreamIterator-stream-empty-function iter))
  (define obj-stream-first-function (StreamIterator-stream-first-function iter))
  (define obj-stream-next-function (StreamIterator-stream-next-function iter))

  (let loop ([obj (StreamIterator-iter-instance iter)])
    (if (obj-stream-empty-function obj)
        void
        (begin
          (func (obj-stream-first-function obj))
          (loop (obj-stream-next-function obj))))))

(define (list-stream-iterator l)
  (StreamIterator l empty? car cdr))

(define (into-stream-iter obj)
  (cond
    [(list? obj) (list-stream-iterator obj)]
    [(#%private-struct? obj) ((#%struct-property-ref obj '#:prop:into-stream) obj)]

    [else (error "into-stream implementation not found for object" obj)]))

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
