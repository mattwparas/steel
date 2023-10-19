(require-builtin #%private/steel/mvector as private.)

(struct MutableVector (inner)
  #:mutable
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

                 (printer-function ")")
                 (printer-function "\n")])))

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

;; TODO: Design iterator over this
(struct MutableVectorIterator (vec idx) #:mutable #:printer (lambda (x) x))

(define done-iterating 'end)

(define (MutableVectorIterator-next iter)

  (define offset (MutableVectorIterator-idx iter))
  (define vec (MutableVectorIterator-vec iter))

  (cond
    [(mutable-vector-empty? vec) done-iterating]
    [(>= offset (mutable-vector-len vec)) done-iterating]
    [else
     (let ([return-value (mutable-vector-ref vec offset)])
       (set-MutableVectorIterator-idx! iter (+ offset 1))
       return-value)]))

(define (mutable-vector-for-each vec func offset)

  (let loop ([func func] [iterator (MutableVectorIterator vec offset)])

    (let ([next (MutableVectorIterator-next iterator)])
      (if (eq? next done-iterating)
          void

          (begin
            (func next)
            (loop func iterator))))))
