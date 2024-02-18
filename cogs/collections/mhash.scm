(provide (contract/out mhash-set! (->/c mhash? any/c any/c any/c))
         (contract/out mhash-ref (->/c mhash? any/c any/c))
         mhash
         (contract/out mhash->hash (->/c mhash? hash?))
         (contract/out mhash-length (->/c mhash? int?))
         (contract/out mhash-contains? (->/c mhash? bool?))
         (contract/out mhash-keys->list (->/c mhash? list?))
         (contract/out mhash-values->list (->/c mhash? list?))
         (contract/out mhash->list (->/c mhash? list?))
         (contract/out mhash? (->/c any/c bool?))
         no-contract-mhash-ref)

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

;; Manually box the hash map
(struct mutable-hash (inner)
  #:printer
  (lambda (obj printer)
    (simple-display "'#mhash(")
    (let ([hash-as-list-of-pairs (transduce (#%unbox (mutable-hash-inner obj)) (into-list))])
      (cond
        [(empty? hash-as-list-of-pairs) (simple-display ")")]
        [else

         (simple-display "(")
         (printer (caar hash-as-list-of-pairs))
         (simple-display " . ")
         (printer (cadar hash-as-list-of-pairs))
         (simple-display ")")

         (for-each (λ (obj)
                     (simple-display " (")
                     (printer (car obj))
                     (simple-display " . ")
                     (printer (list-ref obj 1))
                     (simple-display ")"))
                   (cdr hash-as-list-of-pairs))

         (simple-display ")")]))))

(define mhash? mutable-hash?)

;;@doc
;; Mutably update the hashmap in place, setting the key and value accordingly.
(define (mhash-set! mhash key value)
  (swap-with-expr (mutable-hash-inner mhash) (lambda (h) (hash-insert h key value))))

;;@doc
;; Fetch the value for the given key
(define (mhash-ref mhash key)
  (hash-ref (#%unbox (mutable-hash-inner mhash)) key))

(define no-contract-mhash-ref mhash-ref)

;;@doc
;; Construct a mutable hash map from the given key value pairs
(define (mhash . args)
  (mutable-hash (#%box (apply hash args))))

;;@doc
;; If you want to call any methods that
;; exist for an immutable hash, just delegate here.
;;
;; This conversion is very inexpensive, and does not copy
;; the entire vector.
(define (mhash->hash mh)
  (#%unbox (mutable-hash-inner mh)))

;;@doc
;; Get the length of the mutable hash table. The length is defined
;; as the number of key value pairs.
(define (mhash-length mh)
  (hash-length (mhash->hash mh)))

;;@doc
;; Check if this mutable hash contains a key
(define (mhash-contains? mh key)
  (hash-contains? (mhash->hash mh) key))

;;@doc
;; Get the keys of this mutable hash map as a list
(define (mhash-keys->list mh)
  (hash-keys->list (mhash->hash mh)))

;;@doc
;; Get the values of this mutable hash map as a list
(define (mhash-values->list mh)
  (hash-values->list (mhash->hash mh)))

;;@doc
;; Convert this mutable hash into an association list, which
;; in this case is a list of pairs.
(define (mhash->list mh)
  (transduce (mhash->hash mh) (into-list)))

;; Swap the contents of the boxed value
;; in place, so that we can perform in place
;; updates where relevant.
;;
;; Note: This might actually just be slower, but
;; for the sake of experimenting we'll go with it.
(define (swap-with-expr boxed-value thunk)
  ;; Replace the inner box with void
  (let ([previous (#%set-box! boxed-value void)])
    (with-handler (lambda (err) (#%set-box! boxed-value previous))
                  (#%set-box! boxed-value (thunk previous)))))
