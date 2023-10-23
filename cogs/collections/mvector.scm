(require-builtin #%private/steel/mvector as private.)

;; Clean this up
;; Make the built in API just use immutable-vector
(provide vector?
         immutable-vector?
         mutable-vector?
         make-vector
         vector
         mutable-vector->list)

(struct StreamIterator
        (iter-instance stream-empty-function stream-first-function stream-next-function))

;; Call the struct as a function
(struct IntoIterator (iter-object next-function) #:prop:procedure 1)

(define ITERATOR-FINISHED 'done)

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

(define (iter-for-each iter func)

  (define next-value (iter-next iter))

  (if (iter-finished? next-value)
      void
      (begin
        (func next-value)
        (iter-for-each iter func))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct MutableVector (inner)
  #:mutable
  #:prop:into-iter
  (lambda (object) (IntoIterator (MutableVectorIterator object 0) MutableVectorIterator-next))
  #:printer (lambda (this printer-function)
              (printer-function "'#(")
              (cond
                [(mutable-vector-empty? this) void]
                [else
                 (printer-function (mutable-vector-ref this 0))
                 (mutable-vector-for-each this
                                          (lambda (elem)
                                            (printer-function " ")
                                            (printer-function elem))
                                          1)
                 (printer-function ")")])))

;;@doc
;; Check if the value is an immutable vector
(define immutable-vector? (load-from-module! %-builtin-module-steel/identity 'vector?))

;;@doc
;; Check whether the value is a mutable vector
(define mutable-vector? MutableVector?)

;;@doc
;; Checks whether the value is a vector (mutable or immutable)
(define vector? (lambda (x) (or (immutable-vector? x) (MutableVector? x))))

;;@doc
;; Create a vector of length k, optionally initialized with each
;; slot filled with value v.
(define make-vector
  (case-lambda
    [(k)
     (when (< k 0)
       (error "make-vector requires a positive integer, found " k))

     (list->mutable-vector (map (lambda (_) void) (range 0 k)))]
    [(k v)
     (when (< k 0)
       (error "make-vector requires a positive integer, found " k))

     (list->mutable-vector (map (lambda (_) v) (range 0 k)))]))

(define (make-mutable-vector)
  (MutableVector (private.make-mutable-vector)))

(define (mutable-vector-ref vector index)
  (private.mutable-vector-ref (MutableVector-inner vector) index))

(define (mutable-vector-set! vector index value)
  (private.mutable-vector-set! (MutableVector-inner vector) index value))

(define (mutable-vector-push! vector value)
  (private.mutable-vector-push! (MutableVector-inner vector) value))

(define (mutable-vector->list vector)
  (private.mutable-vector->list (MutableVector-inner vector)))

(define (mutable-vector-empty? vector)
  (private.mutable-vector-empty? (MutableVector-inner vector)))

(define (mutable-vector-len vector)
  (private.mutable-vector-len (MutableVector-inner vector)))

(define (list->mutable-vector list)
  (MutableVector (private.mutable-vector-from-list list)))

(define (mut-vector . args)
  (MutableVector (private.mutable-vector-from-list args)))

(define vector mut-vector)

(struct MutableVectorIterator (vec idx) #:mutable)

(define (MutableVectorIterator-next iter)

  (define offset (MutableVectorIterator-idx iter))
  (define vec (MutableVectorIterator-vec iter))

  (cond
    [(mutable-vector-empty? vec) ITERATOR-FINISHED]
    [(>= offset (mutable-vector-len vec)) ITERATOR-FINISHED]
    [else
     (let ([return-value (mutable-vector-ref vec offset)])
       (set-MutableVectorIterator-idx! iter (+ offset 1))
       return-value)]))

(define (mutable-vector-for-each vec func offset)

  (let loop ([func func] [iterator (MutableVectorIterator vec offset)])

    (let ([next (MutableVectorIterator-next iterator)])
      (if (eq? next ITERATOR-FINISHED)
          void

          (begin
            (func next)
            (loop func iterator))))))
