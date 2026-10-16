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
         compose
         make-reducer
         reducer?
         reduced
         reduced?
         unreduce
         ensure-reduced)

;; A reducer is its three parts: `init` makes the first result, `step` adds a
;; value to a result, and `finish` turns the last result into the answer. A
;; transduction calls `step` directly, with exactly two arguments, which is
;; much cheaper than calling a `case-lambda` that has to sort out how many it
;; was given. A reducer can still be called like an SRFI 171 one, with zero, one
;; or two arguments.
(struct Reducer (init finish step call) #:prop:procedure 3)

(define (make-reducer init finish step)
  (Reducer init
           finish
           step
           (case-lambda
             [() (init)]
             [(result) (finish result)]
             [(result input) (step result input)])))

(define reducer? Reducer?)

;; Any SRFI 171 reducer, which is a procedure taking zero, one or two arguments
(define (->reducer reducer)
  (if (Reducer? reducer)
      reducer
      (Reducer (lambda () (reducer))
               (lambda (result) (reducer result))
               (lambda (result input) (reducer result input))
               reducer)))

;; A reducer with a new `step`, and the same `init` and `finish`
(define (with-step reducer step)
  (make-reducer (Reducer-init reducer) (Reducer-finish reducer) step))

;; A reducer with a new `step` and `finish`
(define (with-step-finish reducer step finish)
  (make-reducer (Reducer-init reducer) finish step))

(define (compose . functions)
  (cond
    [(null? functions) (lambda (x) x)]
    [(null? (cdr functions)) (car functions)]
    [else
     (let ([first (car functions)]
           [rest (apply compose (cdr functions))])
       (lambda (x) (first (rest x))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;; Reduced ;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Reduced (value))

(define reduced Reduced)
(define reduced? Reduced?)
(define unreduce Reduced-value)

(define (ensure-reduced x)
  (if (Reduced? x)
      x
      (Reduced x)))

;; A step that wraps a reduced result once more, for a reduction run inside a
;; step: the inner one unwraps it, and the outer one still sees it reduced.
(define (preserving-reduced step)
  (lambda (a b)
    (let ([return (step a b)])
      (if (Reduced? return)
          (Reduced return)
          return))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Collections ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each returns the result, unwrapped if a step stopped early

(define (list-reduce step acc lst)
  (if (null? lst)
      acc
      (let ([acc (step acc (car lst))])
        (if (Reduced? acc)
            (Reduced-value acc)
            (list-reduce step acc (cdr lst))))))

(define (vector-reduce step acc vec)
  (let ([len (vector-length vec)])
    (let loop ([i 0]
               [acc acc])
      (if (= i len)
          acc
          (let ([acc (step acc (vector-ref vec i))])
            (if (Reduced? acc)
                (Reduced-value acc)
                (loop (+ i 1) acc)))))))

(define (bytevector-u8-reduce step acc vec)
  (let ([len (bytes-length vec)])
    (let loop ([i 0]
               [acc acc])
      (if (= i len)
          acc
          (let ([acc (step acc (bytes-ref vec i))])
            (if (Reduced? acc)
                (Reduced-value acc)
                (loop (+ i 1) acc)))))))

;; Hash maps, hash sets and strings
(define (iterator-reduce step acc coll)
  (let ([iter (value->iterator coll)])
    (let loop ([acc acc])
      (let ([next (iter-next! iter)])
        (if (eq? ITERATOR-FINISHED next)
            acc
            (let ([acc (step acc next)])
              (if (Reduced? acc)
                  (Reduced-value acc)
                  (loop acc))))))))

(define (port-reduce step acc reader port)
  (let loop ([acc acc])
    (let ([val (reader port)])
      (if (eof-object? val)
          acc
          (let ([acc (step acc val)])
            (if (Reduced? acc)
                (Reduced-value acc)
                (loop acc)))))))

(define (generator-reduce step acc gen)
  (let loop ([acc acc])
    (let ([val (gen)])
      (if (eof-object? val)
          acc
          (let ([acc (step acc val)])
            (if (Reduced? acc)
                (Reduced-value acc)
                (loop acc)))))))

;; Any collection, or #f when `coll` isn't one
(define (collection-reduce step acc coll)
  (cond
    [(list? coll) (list-reduce step acc coll)]
    [(vector? coll) (vector-reduce step acc coll)]
    [(or (hash? coll) (set? coll) (string? coll)) (iterator-reduce step acc coll)]
    [(bytes? coll) (bytevector-u8-reduce step acc coll)]
    [else (error "transduce: not a collection: " coll)]))

(define (collection? x)
  (or (list? x) (vector? x) (hash? x) (set? x) (string? x) (bytes? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Reducers ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identity x)
  x)

;; Named rather than written as lambdas where they're used, since the jit
;; leaves a function that makes a lambda without captures to the interpreter.
(define (false-thunk)
  #f)
(define (true-thunk)
  #t)
(define (void-thunk)
  void)
(define (keep-result result input)
  result)

;;@doc
;; A transducer-friendly cons with the empty list as identity. Its result is
;; in the order the values came.
;;
;; ```scheme
;; (list-transduce (tfilter odd?) rcons (list 1 2 3 4)) ;; => '(1 3)
;; ```
(define rcons (make-reducer (lambda () '()) reverse (lambda (lst x) (cons x lst))))

(define new-into-list rcons)

(define new-into-hashmap
  (make-reducer (lambda () (hash)) identity (lambda (h pair) (hash-insert h (car pair) (cdr pair)))))

(define new-into-hashset (make-reducer (lambda () (hashset)) identity hashset-insert))

(define new-into-immutable-vector
  (make-reducer (lambda () (immutable-vector)) identity immutable-vector-push))

(define (new-into-vector)
  (define vec (vector))
  (make-reducer (lambda () vec)
                identity
                (lambda (v elem)
                  (vector-push! v elem)
                  v)))

(define (new-into-for-each func)
  (make-reducer void-thunk
                identity
                (lambda (v elem)
                  (func elem)
                  void)))

(define new-into-string (make-reducer (lambda () (string)) identity string-push))

(define new-into-sum (make-reducer (lambda () 0) identity (lambda (v elem) (+ v elem))))

(define new-into-product (make-reducer (lambda () 1) identity (lambda (v elem) (* v elem))))

(define new-into-max
  (make-reducer false-thunk
                identity
                (lambda (v elem)
                  (if v
                      (max v elem)
                      elem))))

(define new-into-min
  (make-reducer false-thunk
                identity
                (lambda (v elem)
                  (if v
                      (min v elem)
                      elem))))

(define new-into-last (make-reducer false-thunk identity (lambda (v elem) elem)))

(define (new-into-nth n)
  (define iter 0)
  (make-reducer false-thunk
                identity
                (lambda (v elem)
                  (if (= iter n)
                      (Reduced elem)
                      (begin
                        (set! iter (+ 1 iter))
                        elem)))))

(define (new-into-reducer func init)
  (make-reducer (lambda () init) identity (lambda (v elem) (func v elem))))

;;@doc
;; Like rcons, but its result is in the reverse of the order the values came.
;;
;; ```scheme
;; (list-transduce (tfilter odd?) reverse-rcons (list 1 2 3 4)) ;; => '(3 1)
;; ```
(define reverse-rcons (make-reducer (lambda () '()) identity (lambda (lst x) (cons x lst))))

;;@doc
;; Counts the values that reach it.
;;
;; ```scheme
;; (list-transduce (tfilter odd?) rcount (list 1 2 3)) ;; => 2
;; ```
(define rcount (make-reducer (lambda () 0) identity (lambda (result input) (+ 1 result))))

;;@doc
;; Reducers that behave like any and every from srfi-1
(define (rany pred)
  (make-reducer false-thunk
                identity
                (lambda (result input)
                  (let ([test (pred input)])
                    (if test
                        (Reduced test)
                        #f)))))

(define (revery pred)
  (make-reducer true-thunk
                identity
                (lambda (result input)
                  (let ([test (pred input)])
                    (if (and result test)
                        test
                        (Reduced #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Transducing ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Applies `xform` to `f`, runs `reduce` over the result's step, and finishes
(define (run xform f init reduce)
  (let ([xf (->reducer (xform f))])
    ((Reducer-finish xf) (reduce (Reducer-step xf) init))))

(define (first-result f)
  ((Reducer-init (->reducer f))))

(define new-transduce
  (case-lambda
    [(xform f coll) (new-transduce xform f (first-result f) coll)]
    [(xform f init coll)
     (unless (collection? coll)
       (error "unimplemented: " coll))
     (run xform f init (lambda (step acc) (collection-reduce step acc coll)))]))

(define hashmap-transduce
  (case-lambda
    [(xform f coll) (hashmap-transduce xform f (first-result f) coll)]
    [(xform f init coll) (run xform f init (lambda (step acc) (iterator-reduce step acc coll)))]))

(define hashset-transduce hashmap-transduce)

(define list-transduce
  (case-lambda
    [(xform f coll) (list-transduce xform f (first-result f) coll)]
    [(xform f init coll) (run xform f init (lambda (step acc) (list-reduce step acc coll)))]))

(define vector-transduce
  (case-lambda
    [(xform f coll) (vector-transduce xform f (first-result f) coll)]
    [(xform f init coll) (run xform f init (lambda (step acc) (vector-reduce step acc coll)))]))

(define string-transduce
  (case-lambda
    [(xform f coll) (string-transduce xform f (first-result f) coll)]
    [(xform f init coll) (run xform f init (lambda (step acc) (iterator-reduce step acc coll)))]))

(define bytevector-u8-transduce
  (case-lambda
    [(xform f coll) (bytevector-u8-transduce xform f (first-result f) coll)]
    [(xform f init coll)
     (run xform f init (lambda (step acc) (bytevector-u8-reduce step acc coll)))]))

(define bytevector-transduce bytevector-u8-transduce)

(define port-transduce
  (case-lambda
    [(xform f by) (generator-transduce xform f by)]
    [(xform f by port) (port-transduce xform f (first-result f) by port)]
    [(xform f init by port) (run xform f init (lambda (step acc) (port-reduce step acc by port)))]))

(define generator-transduce
  (case-lambda
    [(xform f gen) (generator-transduce xform f (first-result f) gen)]
    [(xform f init gen) (run xform f init (lambda (step acc) (generator-reduce step acc gen)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;; Transducers ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each takes the reducer after it and returns the one its values go into.

(define (tmap f)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)])
      (with-step reducer (lambda (result input) (step result (f input)))))))

(define (tfilter pred)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)])
      (with-step reducer
                 (lambda (result input)
                   (if (pred input)
                       (step result input)
                       result))))))

(define (tremove pred)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)])
      (with-step reducer
                 (lambda (result input)
                   (if (pred input)
                       result
                       (step result input)))))))

(define (tfilter-map f)
  (compose (tmap f) (tfilter identity)))

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
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [left n])
      (with-step reducer
                 (lambda (result input)
                   (if (> left 0)
                       (begin
                         (set! left (- left 1))
                         result)
                       (step result input)))))))

(define (tdrop-while pred)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [drop? #t])
      (with-step reducer
                 (lambda (result input)
                   (if (and drop? (pred input))
                       result
                       (begin
                         (set! drop? #f)
                         (step result input))))))))

(define (ttake n)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [left n])
      (with-step reducer
                 (lambda (result input)
                   (let ([result (if (> left 0)
                                     (step result input)
                                     result)])
                     (set! left (- left 1))
                     (if (> left 0)
                         result
                         (ensure-reduced result))))))))

(define ttake-while
  (case-lambda
    [(pred) (ttake-while pred keep-result)]
    [(pred retf)
     (lambda (reducer)
       (let* ([reducer (->reducer reducer)]
              [step (Reducer-step reducer)]
              [take? #t])
         (with-step reducer
                    (lambda (result input)
                      (if (and take? (pred input))
                          (step result input)
                          (begin
                            (set! take? #f)
                            (ensure-reduced (retf result input))))))))]))

;; Each value is a list, whose values go on one at a time
(define (tconcatenate reducer)
  (let* ([reducer (->reducer reducer)]
         [preserving (preserving-reduced (Reducer-step reducer))])
    (with-step reducer (lambda (result input) (list-reduce preserving result input)))))

;; After every value before it, the values of `collection`
(define (textend collection)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [finish (Reducer-finish reducer)])
      (with-step-finish reducer
                        step
                        (lambda (result) (finish (collection-reduce step result collection)))))))

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
(define (tflatten reducer)
  (let* ([reducer (->reducer reducer)]
         [step (Reducer-step reducer)])
    (define (flat result input)
      (if (collection? input)
          (collection-reduce preserving result input)
          (step result input)))
    (define preserving (preserving-reduced flat))
    (with-step reducer flat)))

;; A marker no value passed through a transduction can be
(struct Nothing ())
(define nothing (Nothing))

;;@doc
;; removes duplicate consecutive elements
(define tdelete-neighbor-duplicates
  (case-lambda
    [() (tdelete-neighbor-duplicates equal?)]
    [(equality-pred?)
     (lambda (reducer)
       (let* ([reducer (->reducer reducer)]
              [step (Reducer-step reducer)]
              [prev nothing])
         (with-step reducer
                    (lambda (result input)
                      (if (and (not (eq? prev nothing)) (equality-pred? prev input))
                          result
                          (begin
                            (set! prev input)
                            (step result input)))))))]))

;; Deletes all duplicates that passes through.
(define (tdelete-duplicates)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [seen (hashset)])
      (with-step reducer
                 (lambda (result input)
                   (if (hashset-contains? seen input)
                       result
                       (begin
                         (set! seen (hashset-insert seen input))
                         (step result input))))))))

;; Partitions the input into lists of N items. If the input stops it flushes whatever
;; it has collected, which may be shorter than n.
(define (tsegment n)
  (if (not (and (integer? n) (positive? n)))
      (error "argument to tsegment must be a positive integer")
      (lambda (reducer)
        (let* ([reducer (->reducer reducer)]
               [step (Reducer-step reducer)]
               [finish (Reducer-finish reducer)]
               [i 0]
               [collect (make-vector n)])
          (with-step-finish reducer
                            (lambda (result input)
                              (vector-set! collect i input)
                              (set! i (+ i 1))
                              (if (< i n)
                                  result
                                  (let ([next-input (vector->list collect 0 i)])
                                    (set! i 0)
                                    (step result next-input))))
                            (lambda (result)
                              (let ([result (if (zero? i)
                                                result
                                                (step result (vector->list collect 0 i)))])
                                (set! i 0)
                                (finish (if (Reduced? result)
                                            (Reduced-value result)
                                            result)))))))))

(define (tpartition f)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [finish (Reducer-finish reducer)]
           [prev nothing]
           [collect '()])
      (with-step-finish reducer
                        (lambda (result input)
                          (let ([fout (f input)])
                            (if (or (eq? prev nothing) (equal? fout prev))
                                (begin
                                  (set! prev fout)
                                  (set! collect (cons input collect))
                                  result)
                                (let ([next-input (reverse collect)])
                                  (set! prev fout)
                                  (set! collect (list input))
                                  (step result next-input)))))
                        (lambda (result)
                          (let ([result (if (null? collect)
                                            result
                                            (step result (reverse collect)))])
                            (set! collect '())
                            (finish (if (Reduced? result)
                                        (Reduced-value result)
                                        result))))))))

;;@doc
;; Interposes element between each value pushed through the transduction.
(define (tadd-between elem)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [send-elem? #f])
      (with-step reducer
                 (lambda (result input)
                   (if send-elem?
                       (let ([result (step result elem)])
                         (if (Reduced? result)
                             result
                             (step result input)))
                       (begin
                         (set! send-elem? #t)
                         (step result input))))))))

(define (tzip elems)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [iter (value->iterator elems)])
      (with-step reducer (lambda (result input) (step result (cons input (iter-next! iter))))))))

(define (tinterleave elems)
  (lambda (reducer)
    (let* ([reducer (->reducer reducer)]
           [step (Reducer-step reducer)]
           [iter (value->iterator elems)]
           [send-elem? #f])
      (with-step reducer
                 (lambda (result input)
                   (if send-elem?
                       (let ([result (step result (iter-next! iter))])
                         (if (Reduced? result)
                             result
                             (step result input)))
                       (begin
                         (set! send-elem? #t)
                         (step result input))))))))

;;@doc
;; Pairs each value with its index, as `(list index value)`. Starts at 0 unless given another.
(define tenumerate
  (case-lambda
    [() (tenumerate 0)]
    [(n)
     (lambda (reducer)
       (let* ([reducer (->reducer reducer)]
              [step (Reducer-step reducer)]
              [i n])
         (with-step reducer
                    (lambda (result input)
                      (let ([input (list i input)])
                        (set! i (+ i 1))
                        (step result input))))))]))

(define tlog
  (case-lambda
    [() (tlog (lambda (result input) (displayln input)))]
    [(log-function)
     (lambda (reducer)
       (let* ([reducer (->reducer reducer)]
              [step (Reducer-step reducer)])
         (with-step reducer
                    (lambda (result input)
                      (log-function result input)
                      (step result input)))))]))
