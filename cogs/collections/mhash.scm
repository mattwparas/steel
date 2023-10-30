(require-builtin #%private/steel/mhash as private.)

(struct mutable-hash (inner) #:mutable)

(define (mhash-set! mhash key value)
  (private.mhash-set! (mutable-hash-inner mhash) key value))

(define (mhash-ref mhash key)
  (private.mhash-ref (mutable-hash-inner mhash) key))

(define (mhash)
  (mutable-hash (private.mhash)))

(define (loop)
  (define my-hash (mhash))

  (mhash-set! my-hash 'foo 'bar)
  (mhash-set! my-hash 'bar 'foo)

  (mhash-set! my-hash 'baz my-hash)

  (loop))
