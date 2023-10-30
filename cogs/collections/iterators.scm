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
