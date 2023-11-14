(require-builtin steel/base)
(require-builtin #%private/steel/mvector as private.)
(require "steel/iterators")

;; Clean this up
;; Make the built in API just use immutable-vector
(provide vector?
         immutable-vector?
         mutable-vector?
         make-vector
         vector
         mutable-vector->list
         mutable-vector-set!
         mutable-vector-ref
         mutable-vector-len
         list->mutable-vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct MutableVector (inner)
  #:mutable
  #:prop:into-iter
  (lambda (object) (IntoIterator (MutableVectorIterator object 0) MutableVectorIterator-next))
  #:printer (lambda (this printer-function)
              (simple-display "'#(")
              (cond
                [(mutable-vector-empty? this) void]
                [else
                 (printer-function (mutable-vector-ref this 0))
                 (mutable-vector-for-each this
                                          (lambda (elem)
                                            (simple-display " ")
                                            (printer-function elem))
                                          1)
                 (simple-display ")")])))

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
  (define inner (MutableVector-inner vector))
  (when (>= index (private.mutable-vector-len inner))
    (error "index out of bounds - attempted to index"
           (private.mutable-vector->list inner)
           "with index: "
           index))
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
