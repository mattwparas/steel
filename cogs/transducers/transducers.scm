(require "steel/iterators")

(provide list-transduce
         new-transduce
         hashmap-transduce
         hashset-transduce
         vector-transduce
         string-transduce
         bytevector-u8-transduce
         bytevector-transduce
         port-transduce
         generator-transduce
         tmap
         tfilter
         tremove
         tfilter-map
         tflat-map
         treplace
         tdrop
         tdrop-while
         tappend-map
         tflatten
         tdelete-neighbor-duplicates
         tdelete-duplicates
         tsegment
         tpartition
         tenumerate
         tlog
         tadd-between
         tinterleave
         tzip
         ttake
         ttake-while
         tconcatenate
         textend
         rcons
         reverse-rcons
         new-into-hashmap
         new-into-hashset
         new-into-immutable-vector
         new-into-vector
         new-into-for-each
         new-into-list
         new-into-string
         new-into-sum
         new-into-product
         new-into-max
         new-into-min
         new-into-last
         new-into-nth
         new-into-reducer
         rany
         revery
         rcount
         compose)

(define (compose . functions)
  (define (make-chain thunk chain)
    (lambda args (call-with-values (lambda () (apply thunk args)) chain)))
  (if (null? functions)
      values
      (fold make-chain (car functions) (cdr functions))))

(struct reduced (val))

(define unreduce reduced-val)

;; helper function which ensures x is reduced.
(define (ensure-reduced x)
  (if (reduced? x)
      x
      (reduced x)))

;; helper function that wraps a reduced value twice since reducing functions (like list-reduce)
;; unwraps them. tconcatenate is a good example: it re-uses it's reducer on it's input using list-reduce.
;; If that reduction finishes early and returns a reduced value, list-reduce would "unreduce"
;; that value and try to continue the transducing process.
(define (preserving-reduced reducer)
  (lambda (a b)
    (let ([return (reducer a b)])
      (if (reduced? return)
          (reduced return)
          return))))

(define (list-reduce f identity lst)
  (if (null? lst)
      identity
      (%plain-let ((v (f identity (car lst))))
                  (if (reduced? v)
                      (unreduce v)
                      (list-reduce f v (cdr lst))))))

(define (vector-reduce f identity vec)
  (let ([len (vector-length vec)])
    (let loop ([i 0]
               [acc identity])
      (if (= i len)
          acc
          (let ([acc (f acc (vector-ref vec i))])
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

;; Note: this is the same as the string-reduce
(define (iterator-reduce f identity hmap)
  (let ([iter (value->iterator hmap)])
    (let loop ([acc identity])
      (let ([next (iter-next! iter)])
        ;; TODO: Change this to eq? and make iterator-finished
        ;; static instead of thread local
        (if (equal? ITERATOR-FINISHED next)
            acc
            (let ([acc (f acc next)])
              (if (reduced? acc)
                  (unreduce acc)
                  (loop acc))))))))

(define (string-reduce f identity str)
  (let ([iter (value->iterator str)])
    (let loop ([acc identity])
      (let ([next (iter-next! iter)])
        ;; TODO: Change this to eq? and make iterator-finished
        ;; static instead of thread local
        (if (equal? ITERATOR-FINISHED next)
            acc
            (let ([acc (f acc next)])
              (if (reduced? acc)
                  (unreduce acc)
                  (loop acc))))))))

(define (bytevector-u8-reduce f identity vec)
  (let ([len (bytes-length vec)])
    (let loop ([i 0]
               [acc identity])
      (if (= i len)
          acc
          (let ([acc (f acc (bytes-ref vec i))])
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (port-reduce f identity reader port)
  (let loop ([val (reader port)]
             [acc identity])
    (if (eof-object? val)
        acc
        (let ([acc (f acc val)])
          (if (reduced? acc)
              (unreduce acc)
              (loop (reader port) acc))))))

(define (generator-reduce f identity gen)
  (let loop ([val (gen)]
             [acc identity])
    (if (eof-object? val)
        acc
        (let ([acc (f acc val)])
          (if (reduced? acc)
              (unreduce acc)
              (loop (gen) acc))))))

(struct <Nothing> ())

(define nothing (<Nothing>))
(define nothing? <Nothing>?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reducing functions meant to be used at the end at the transducing
;; process.    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;@doc
;; A transducer-friendly cons with the empty list as identity
;; This function accepts multiple arguments:
;;   * 0 arguments, returns the empty list as the identity
;;   * 1 argument, a list, returns the reverse of that list
;;   * 2 arguments, a list and the element, then returns a new list with the element consed to the list
;;
;; Used to build up a list during a transduction:
;; ```scheme
;; (list-transduce (tfilter odd?) rcons (list 1 2 3 4)) ;; => '(1 3)
;; ```
(define rcons
  (case-lambda
    [() '()]
    [(lst) (reverse lst)]
    [(lst x) (cons x lst)]))

(define new-into-list rcons)

(define new-into-hashmap
  (case-lambda
    [() (hash)]
    [(h) h]
    [(h pair) (hash-insert h (car pair) (cdr pair))]))

(define new-into-hashset
  (case-lambda
    [() (hashset)]
    [(h) h]
    [(h x) (hashset-insert h x)]))

(define new-into-immutable-vector
  (case-lambda
    [() (immutable-vector)]
    [(h) h]
    [(h x) (immutable-vector-push h x)]))

(define (new-into-vector)
  (define vec (vector))
  (case-lambda
    [() vec]
    [(v) v]
    [(v elem)
     (vector-push! v elem)
     v]))

(define (new-into-for-each func)
  (case-lambda
    [() void]
    [(v) v]
    [(v elem)
     (func elem)
     void]))

(define new-into-string
  (case-lambda
    [() (string)]
    [(v) v]
    [(v elem) (string-push v elem)]))

(define new-into-sum
  (case-lambda
    [() 0]
    [(v) v]
    [(v elem) (+ v elem)]))

(define new-into-product
  (case-lambda
    [() 0]
    [(v) v]
    [(v elem) (* v elem)]))

(define new-into-max
  (case-lambda
    [() #f]
    [(v) v]
    [(v elem)
     (if v
         (max v elem)
         elem)]))

(define new-into-min
  (case-lambda
    [() #f]
    [(v) v]
    [(v elem)
     (if v
         (min v elem)
         elem)]))

(define new-into-last
  (case-lambda
    [() #f]
    [(v) v]
    [(v elem) elem]))

(define (new-into-nth n)
  (define iter 0)
  (case-lambda
    [() #f]
    [(v) v]
    [(v elem)
     (if (= iter n)
         (reduced elem)
         (begin
           (set! iter (+ 1 iter))
           elem))]))

(define (new-into-reducer func init)
  (case-lambda
    [() init]
    [(v) v]
    [(v elem) (func v elem)]))

;;@doc
;; A transducer-friendly cons with the empty list as identity. Acts like rcons, however will reverse
;; the resulting list.
;;
;; This function accepts multiple arguments:
;;   * 0 arguments, returns the empty list as the identity
;;   * 1 argument, a list, returns that list
;;   * 2 arguments, a list and the element, then returns a new list with the element consed to the list
;;
;; Used to build up a list during a transduction:
;; ```scheme
;; (list-transduce (tfilter odd?) rcons (list 1 2 3 4)) ;; => '(3 1)
;; ```
(define reverse-rcons
  (case-lambda
    [() '()]
    [(lst) lst]
    [(lst x) (cons x lst)]))

;;@doc
;; Use this as the f in transduce to count the amount of elements passed through.
;;
;; ```scheme
;; (list-transduce (tfilter odd?) tcount (list 1 2 3)) ;; => 2
;; ```
(define rcount
  (case-lambda
    [() 0]
    [(result) result]
    [(result input) (+ 1 result)]))

;;@doc
;; These two take a predicate and returns reducing functions that behave
;; like any and every from srfi-1
(define (rany pred)
  (case-lambda
    [() #f]
    [(result) result]
    [(result input)
     (let ([test (pred input)])
       (if test
           (reduced test)
           #f))]))

(define (revery pred)
  (case-lambda
    [() #t]
    [(result) result]
    [(result input)
     (let ([test (pred input)])
       (if (and result test)
           test
           (reduced #f)))]))

(struct PortTransducer (reader port))

(define new-transduce
  (case-lambda
    [(xform f coll)
     (cond
       [(list? coll) (list-transduce xform f coll)]
       [(hash? coll) (hashmap-transduce xform f coll)]
       [(set? coll) (hashset-transduce xform f coll)]
       [(vector? coll) (vector-transduce xform f coll)]
       [(string? coll) (string-transduce xform f coll)]
       [(bytes? coll) (bytevector-u8-transduce xform f coll)]
       ;; TODO: Fix this with transduce
       ; [(port? coll) (port-transduce xform f coll)]
       [else (error "unimplemented: " coll)])]

    [(xform f init coll)
     (cond
       [(list? coll) (list-transduce xform f init coll)]
       [(hash? coll) (hashmap-transduce xform f init coll)]
       [(set? coll) (hashset-transduce xform f init coll)]
       [(vector? coll) (vector-transduce xform f init coll)]
       [(string? coll) (string-transduce xform f init coll)]
       [(bytes? coll) (bytevector-u8-transduce xform f init coll)]
       ; [(port? coll) (port-transduce xform f init coll)]
       [else (error "unimplemented")])]))

;; Note: this, strings, and hash sets all use the same
;; underlying implementation. This should also be generic
;; over custom iterators as well.
(define hashmap-transduce
  (case-lambda
    [(xform f coll) (hashmap-transduce xform f (f) coll)]
    [(xform f init coll)
     (let* ([xf (xform f)]
            [result (iterator-reduce xf init coll)])
       (xf result))]))

(define hashset-transduce
  (case-lambda
    [(xform f coll) (hashset-transduce xform f (f) coll)]
    [(xform f init coll)
     (let* ([xf (xform f)]
            [result (iterator-reduce xf init coll)])
       (xf result))]))

(define list-transduce
  (case-lambda
    [(xform f coll) (list-transduce xform f (f) coll)]
    [(xform f init coll)
     (let* ([xf (xform f)]
            [result (list-reduce xf init coll)])
       (xf result))]))

(define vector-transduce
  (case-lambda
    [(xform f coll) (vector-transduce xform f (f) coll)]
    [(xform f init coll)
     (let* ([xf (xform f)]
            [result (vector-reduce xf init coll)])
       (xf result))]))

(define string-transduce
  (case-lambda
    [(xform f coll) (string-transduce xform f (f) coll)]
    [(xform f init coll)
     (let* ([xf (xform f)]
            [result (string-reduce xf init coll)])
       (xf result))]))

(define bytevector-u8-transduce
  (case-lambda
    [(xform f coll) (bytevector-u8-transduce xform f (f) coll)]
    [(xform f init coll)
     (let* ([xf (xform f)]
            [result (bytevector-u8-reduce xf init coll)])
       (xf result))]))

(define bytevector-transduce
  (case-lambda
    [(xform f coll) (bytevector-transduce xform f (f) coll)]
    [(xform f init coll)
     (let* ([xf (xform f)]
            [result (bytevector-u8-reduce xf init coll)])
       (xf result))]))

(define port-transduce
  (case-lambda
    [(xform f by) (generator-transduce xform f by)]
    [(xform f by port) (port-transduce xform f (f) by port)]
    [(xform f init by port)
     (let* ([xf (xform f)]
            [result (port-reduce xf init by port)])
       (xf result))]))

(define generator-transduce
  (case-lambda
    [(xform f gen) (generator-transduce xform f (f) gen)]
    [(xform f init gen)
     (let* ([xf (xform f)]
            [result (generator-reduce xf init gen)])
       (xf result))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transducers!    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmap f)
  (lambda (reducer)
    (case-lambda
      [() (reducer)]
      [(result) (reducer result)]
      [(result input) (reducer result (f input))])))

(define (tfilter pred)
  (lambda (reducer)
    (case-lambda
      [() (reducer)]
      [(result) (reducer result)]
      [(result input)
       (if (pred input)
           (reducer result input)
           result)])))

(define (tremove pred)
  (lambda (reducer)
    (case-lambda
      [() (reducer)]
      [(result) (reducer result)]
      [(result input)
       (if (not (pred input))
           (reducer result input)
           result)])))

(define (tfilter-map f)
  (compose (tmap f) (tfilter values)))

(define (hash-table-ref/default m key default)
  (if (hash-contains? m key)
      (hash-get m key)
      default))

(define (make-replacer map)
  (cond
    [(list? map)
     (lambda (x)
       (let ([replacer? (assoc x map)])
         (if replacer?
             (cdr replacer?)
             x)))]
    [(hash? map) (lambda (x) (hash-table-ref/default map x x))]
    [(procedure? map) map]
    [else (error "Unsupported mapping in treplace" map)]))

(define (treplace map)
  (tmap (make-replacer map)))

(define (tdrop n)
  (lambda (reducer)
    (let ([new-n (+ 1 n)])
      (case-lambda
        [() (reducer)]
        [(result) (reducer result)]
        [(result input)
         (set! new-n (- new-n 1))
         (if (positive? new-n)
             result
             (reducer result input))]))))

(define (tdrop-while pred)
  (lambda (reducer)
    (let ([drop? #t])
      (case-lambda
        [() (reducer)]
        [(result) (reducer result)]
        [(result input)
         (if (and (pred input) drop?)
             result
             (begin
               (set! drop? #f)
               (reducer result input)))]))))

(define (ttake n)
  (define (positive? x)
    (> x 0))
  (lambda (reducer)
    ;; we need to reset new-n for every new transduction
    (let ([new-n n])
      (case-lambda
        [() (reducer)]
        [(result) (reducer result)]
        [(result input)
         (let ([result (if (positive? new-n)
                           (reducer result input)
                           result)])
           (set! new-n (- new-n 1))
           (if (not (positive? new-n))
               (ensure-reduced result)
               result))]))))

(define ttake-while
  (case-lambda
    [(pred) (ttake-while pred (lambda (result input) result))]
    [(pred retf)
     (lambda (reducer)
       (let ([take? #t])
         (case-lambda
           [() (reducer)]
           [(result) (reducer result)]
           [(result input)
            (if (and take? (pred input))
                (reducer result input)
                (begin
                  (set! take? #f)
                  (ensure-reduced (retf result input))))])))]))

(define (tconcatenate reducer)
  (let ([preserving-reducer (preserving-reduced reducer)])
    (case-lambda
      [() (reducer)]
      [(result) (reducer result)]
      [(result input) (list-reduce preserving-reducer result input)])))

(define (textend collection)
  (lambda (reducer)
    (let ([preserving-reducer (preserving-reduced reducer)])
      (case-lambda
        [() (reducer)]
        ;; Continue with the collection?
        [(result)

         (cond
           [(list? result) (reducer (list-reduce preserving-reducer result collection))]
           [(hash? result) (reducer (iterator-reduce preserving-reducer result collection))]
           [(set? result) (reducer (iterator-reduce preserving-reducer result collection))]
           [(vector? result) (reducer (iterator-reduce preserving-reducer result collection))]
           [(string? result) (reducer (string-reduce preserving-reducer result collection))]
           [(bytes? result) (reducer (bytevector-u8-reduce preserving-reducer result collection))]
           ;; Idk, what would we put here?
           ; [(port? result) (port-reduce (preserving-reduced (tflatten reducer)) result input)]
           [else (reducer result)])]
        [(result input) (reducer result input)]))))

(define (tappend-map f)
  (compose (tmap f) tconcatenate))

(define (tflat-map f)
  (compose (tmap f) tflatten))

;;@doc
;; Flattens everything and passes each value through the reducer
;;
;; ```scheme
;; (list-transduce tflatten rcons (list 1 2 (list 3 4 '(5 6) 7 8))) ;; => (1 2 3 4 5 6 7 8)
;; ```
(define tflatten
  (lambda (reducer)
    (case-lambda
      [() '()]
      [(result) (reducer result)]
      [(result input)

       (cond
         [(list? result) (list-reduce (preserving-reduced (tflatten reducer)) result input)]
         [(hash? result) (iterator-reduce (preserving-reduced (tflatten reducer)) result input)]
         [(set? result) (iterator-reduce (preserving-reduced (tflatten reducer)) result input)]
         [(vector? result) (iterator-reduce (preserving-reduced (tflatten reducer)) result input)]
         [(string? result) (string-reduce (preserving-reduced (tflatten reducer)) result input)]
         [(bytes? result) (bytevector-u8-reduce (preserving-reduced (tflatten reducer)) result input)]
         ;; Idk, what would we put here?
         ; [(port? result) (port-reduce (preserving-reduced (tflatten reducer)) result input)]
         [else (reducer result input)])])))

;;@doc
;; removes duplicate consecutive elements
(define tdelete-neighbor-duplicates
  (case-lambda
    [() (tdelete-neighbor-duplicates equal?)]
    [(equality-pred?)
     (lambda (reducer)
       (let ([prev nothing])
         (case-lambda
           [() (reducer)]
           [(result) (reducer result)]
           [(result input)
            (if (equality-pred? prev input)
                result
                (begin
                  (set! prev input)
                  (reducer result input)))])))]))

;; Deletes all duplicates that passes through.
(define (tdelete-duplicates)
  (lambda (reducer)
    (let ([already-seen (box (hashset))])
      (case-lambda
        [() (reducer)]
        [(result) (reducer result)]
        [(result input)
         (if (hashset-contains? (unbox already-seen) input)
             result
             (begin
               (set-box! already-seen (hashset-insert (unbox already-seen) input))
               (reducer result input)))]))))

;; Partitions the input into lists of N items. If the input stops it flushes whatever
;; it has collected, which may be shorter than n.
(define (tsegment n)
  (if (not (and (integer? n) (positive? n)))
      (error "argument to tsegment must be a positive integer")
      (lambda (reducer)
        (let ([i 0]
              [collect (make-vector n)])
          (case-lambda
            [() (reducer)]
            [(result)
             ;; if there is anything collected when we are asked to quit
             ;; we flush it to the remaining transducers
             (let ([result (if (zero? i)
                               result
                               (reducer result (vector->list collect 0 i)))])
               (set! i 0)
               ;; now finally, pass it downstreams
               (if (reduced? result)
                   (reducer (unreduce result))
                   (reducer result)))]
            [(result input)
             (vector-set! collect i input)
             (set! i (+ i 1))
             ;; If we have collected enough input we can pass it on downstream
             (if (< i n)
                 result
                 (let ([next-input (vector->list collect 0 i)])
                   (set! i 0)
                   (reducer result next-input)))])))))

(define (tpartition f)
  (lambda (reducer)
    (let* ([prev nothing]
           [collect '()])
      (case-lambda
        [() (reducer)]
        [(result)
         (let ([result (if (null? collect)
                           result
                           (reducer result (reverse collect)))])
           (set! collect '())
           (if (reduced? result)
               (reducer (unreduce result))
               (reducer result)))]
        [(result input)
         (let ([fout (f input)])
           (cond
             [(or (equal? fout prev) (nothing? prev)) ; collect
              (set! prev fout)
              (set! collect (cons input collect))
              result]
             [else ; flush what we collected already to the reducer
              (let ([next-input (reverse collect)])
                (set! prev fout)
                (set! collect (list input))
                (reducer result next-input))]))]))))

;;@doc
;; Interposes element between each value pushed through the transduction.
(define (tadd-between elem)
  (lambda (reducer)
    (let ([send-elem? #f])
      (case-lambda
        [() (reducer)]
        [(result) (reducer result)]
        [(result input)
         (if send-elem?
             (let ([result (reducer result elem)])
               (if (reduced? result)
                   result
                   (reducer result input)))
             (begin
               (set! send-elem? #t)
               (reducer result input)))]))))

(define (tzip elems)
  (define iter (value->iterator elems))
  (lambda (reducer)
    (case-lambda
      [() (reducer)]
      [(result) (reducer result)]
      [(result input)
       ;; Zip together into a pair
       (define next (iter-next! iter))
       (reducer result (cons input next))])))

(define (tinterleave elems)
  (define iter (value->iterator elems))
  (lambda (reducer)
    (let ([send-elem? #f])
      (case-lambda
        [() (reducer)]
        [(result) (reducer result)]
        [(result input)
         (if send-elem?
             ;; TODO: Properly return reduced? if this is eof
             ;; So what I mean is actually use a transducer instead
             ;; of the iterator directly since it can abstract over the things
             (let ([result (reducer result (iter-next! iter))])
               (if (reduced? result)
                   result
                   (reducer result input)))
             (begin
               (set! send-elem? #t)
               (reducer result input)))]))))

;;@doc
;; indexes every value passed through in a cons pair as in (index . value). By default starts at 0
(define tenumerate
  (case-lambda
    [() (tenumerate 0)]
    [(n)
     (lambda (reducer)
       (let ([n n])
         (case-lambda
           [() (reducer)]
           [(result) (reducer result)]
           [(result input)
            (let ([input (list n input)])
              (set! n (+ n 1))
              (reducer result input))])))]))

(define tlog
  (case-lambda
    [() (tlog (lambda (result input) (displayln input)))]
    [(log-function)
     (lambda (reducer)
       (case-lambda
         [() (reducer)]
         [(result) (reducer result)]
         [(result input)
          (log-function result input)
          (reducer result input)]))]))
