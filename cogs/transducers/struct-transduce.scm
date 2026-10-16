(provide transduce
         compose
         mapping
         filtering
         taking
         dropping
         flat-mapping
         flattening
         extending
         enumerating
         zipping
         interleaving
         into-sum
         into-product
         into-max
         into-min
         into-count
         into-nth
         into-list
         into-vector
         into-hashmap
         into-hashset
         into-string
         into-last
         into-for-each
         into-reducer
         transducer?
         reducer?)

(require "#%private/steel/streams")

(struct Reduced (value))

;; Where a stage sends its values: `step` takes the accumulated result and a
;; value, and `finish` turns the final result into what `transduce` returns.
;; A pair, which is cheaper to make and drop than a struct.
(define (Sink step finish)
  (cons step finish))
(define (Sink-step sink)
  (car sink))
(define (Sink-finish sink)
  (cdr sink))

;; A marker no value passed through a transduction can be
(struct Marker (name))

(define (ensure-reduced value)
  (if (Reduced? value)
      value
      (Reduced value)))

(define (unreduce value)
  (if (Reduced? value)
      (Reduced-value value)
      value))

(define (transduce-error kind message . value)
  (apply #%transduce-error kind message value))

;;;;;;;;;;;;;;;;;;;;;;;;;; Collections ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each returns the result, still wrapped if a step stopped early.

(define (reduce-list step acc lst)
  (if (null? lst)
      acc
      (let ([acc (step acc (car lst))])
        (if (Reduced? acc)
            acc
            (reduce-list step acc (cdr lst))))))

(define (reduce-vector step acc vec i len)
  (if (= i len)
      acc
      (let ([acc (step acc (vector-ref vec i))])
        (if (Reduced? acc)
            acc
            (reduce-vector step acc vec (+ i 1) len)))))

;; Hash maps give each entry as a list of its key and value
(define (reduce-entries step acc entries)
  (if (null? entries)
      acc
      (let ([acc (step acc (list (car (car entries)) (cdr (car entries))))])
        (if (Reduced? acc)
            acc
            (reduce-entries step acc (cdr entries))))))

;; The rest of a stream is only forced when its next value is needed
(define (reduce-stream step acc stream)
  (if (stream-empty? stream)
      acc
      (let ([acc (step acc (stream-car stream))])
        (if (Reduced? acc)
            acc
            (reduce-stream step acc (stream-rest stream))))))

(define (stream-rest stream)
  (let ([rest ((#%stream-cdr stream))])
    (if (stream? rest)
        rest
        (transduce-error 'type-mismatch "the rest of a stream has to be a stream, found: " rest))))

(define stream? #%stream?)

;; `what` names the transducer that brought `coll` in, for the error
(define (reduce-collection step acc coll what)
  (cond
    [(list? coll) (reduce-list step acc coll)]
    [(immutable-vector? coll) (reduce-vector step acc coll 0 (vector-length coll))]
    ;; A copy, so changing the vector while it's being read doesn't
    [(mutable-vector? coll) (reduce-list step acc (mutable-vector->list coll))]
    [(string? coll) (reduce-list step acc (string->list coll))]
    [(set? coll) (reduce-list step acc (hashset->list coll))]
    [(hash? coll) (reduce-entries step acc (hash->list coll))]
    [(stream? coll) (reduce-stream step acc coll)]
    [else
     (if what
         (transduce-error 'type-mismatch
                          (string-append what " expected a traversable value, found: ")
                          coll)
         (transduce-error 'type-mismatch "value unable to be converted to an iterable: " coll))]))

(define (entry->list entry)
  (list (car entry) (cdr entry)))

;; A function that returns the next value of `coll` each call, or `cursor-end`
;; once there are none. A value that isn't a collection raises on the first
;; call, so a transduction that never needs it doesn't.
(define cursor-end (Marker 'end))

(define (cursor coll what)
  (define items #f)
  (define (start!)
    (set! items
          (cond
            [(list? coll) coll]
            [(immutable-vector? coll) (vector->list coll)]
            [(mutable-vector? coll) (mutable-vector->list coll)]
            [(string? coll) (string->list coll)]
            [(set? coll) (hashset->list coll)]
            [(hash? coll) (map entry->list (hash->list coll))]
            [(stream? coll) coll]
            [else
             (transduce-error 'type-mismatch
                              (string-append what " expected a traversable value, found: ")
                              coll)])))
  (define first? #t)
  (lambda ()
    (when first?
      (start!))
    (cond
      [(stream? items)
       (unless first?
         (unless (stream-empty? items)
           (set! items (stream-rest items))))
       (set! first? #f)
       (if (stream-empty? items)
           cursor-end
           (stream-car items))]
      [else
       (set! first? #f)
       (if (null? items)
           cursor-end
           (let ([value (car items)])
             (set! items (cdr items))
             value))])))

;;;;;;;;;;;;;;;;;;;;;;;;;; Stages ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-stage f sink)
  (let ([step (Sink-step sink)]) (Sink (lambda (acc x) (step acc (f x))) (Sink-finish sink))))

(define (filter-stage pred sink)
  (let ([step (Sink-step sink)])
    (Sink (lambda (acc x)
            (if (pred x)
                (step acc x)
                acc))
          (Sink-finish sink))))

;; Once it has passed on `n` values it stops, without asking for more
(define (take-stage n sink)
  (let ([step (Sink-step sink)]
        [taken 0])
    (Sink (lambda (acc x)
            (if (>= taken n)
                (ensure-reduced acc)
                (begin
                  (set! taken (+ taken 1))
                  (let ([acc (step acc x)])
                    (if (>= taken n)
                        (ensure-reduced acc)
                        acc)))))
          (Sink-finish sink))))

(define (drop-stage n sink)
  (let ([step (Sink-step sink)]
        [dropped 0])
    (Sink (lambda (acc x)
            (if (< dropped n)
                (begin
                  (set! dropped (+ dropped 1))
                  acc)
                (step acc x)))
          (Sink-finish sink))))

(define (flat-map-stage f sink)
  (let ([step (Sink-step sink)])
    (Sink (lambda (acc x) (reduce-collection step acc (f x) "flatten")) (Sink-finish sink))))

(define (flatten-stage sink)
  (let ([step (Sink-step sink)])
    (Sink (lambda (acc x) (reduce-collection step acc x "flatten")) (Sink-finish sink))))

;; After everything before it, the values of `coll`, unless what comes after
;; it already stopped
(define (extend-stage coll sink)
  (let* ([step (Sink-step sink)]
         [stopped #f]
         [watched (lambda (acc x)
                    (let ([acc (step acc x)])
                      (when (Reduced? acc)
                        (set! stopped #t))
                      acc))])
    (Sink watched
          (lambda (acc)
            ((Sink-finish sink) (if stopped
                                    acc
                                    (unreduce (reduce-collection watched acc coll "extending"))))))))

(define (enumerate-stage sink)
  (let ([step (Sink-step sink)]
        [i 0])
    (Sink (lambda (acc x)
            (let ([index i])
              (set! i (+ i 1))
              (step acc (list index x))))
          (Sink-finish sink))))

;; Ends with the shorter of the two
(define (zip-stage coll sink)
  (let ([step (Sink-step sink)]
        [next (cursor coll "zipping")])
    (Sink (lambda (acc x)
            (let ([y (next)])
              (if (eq? y cursor-end)
                  (ensure-reduced acc)
                  (step acc (list x y)))))
          (Sink-finish sink))))

;; One value from before it, then one of `coll`, and the rest of either once
;; the other runs out
(define (interleave-stage coll sink)
  (let* ([step (Sink-step sink)]
         [next (cursor coll "interleaving")]
         [other-done #f]
         [stopped #f]
         [watched (lambda (acc x)
                    (let ([acc (step acc x)])
                      (when (Reduced? acc)
                        (set! stopped #t))
                      acc))]
         [pull (lambda ()
                 (if other-done
                     cursor-end
                     (let ([y (next)])
                       (when (eq? y cursor-end)
                         (set! other-done #t))
                       y)))])
    (define (drain acc)
      (let ([y (pull)])
        (if (eq? y cursor-end)
            acc
            (let ([acc (watched acc y)])
              (if (Reduced? acc)
                  acc
                  (drain acc))))))
    (Sink (lambda (acc x)
            (let ([acc (watched acc x)])
              (if (Reduced? acc)
                  acc
                  (let ([y (pull)])
                    (if (eq? y cursor-end)
                        acc
                        (watched acc y))))))
          (lambda (acc)
            ((Sink-finish sink) (if stopped
                                    acc
                                    (unreduce (drain acc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Reducers ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nothing yet, for the reducers that need a first value
(define nothing (Marker 'nothing))

(define (identity x)
  x)

;; The primitives `+`, `*`, `>` and `<`, called as functions so that a bad value
;; raises what it does from rust
(define add (#%black-box +))
(define multiply (#%black-box *))
(define greater (#%black-box >))
(define less (#%black-box <))

;; The steps below are named functions rather than lambdas, since the jit
;; leaves a function that makes a lambda without captures to the interpreter.
;; The arithmetic is inline for numbers, and calls the primitive for anything
;; else so that it raises the primitive's error.

(define (cons-step acc x)
  (cons x acc))

(define (reversed-vector acc)
  (list->vector (reverse acc)))

(define (add-step acc x)
  (if (and (number? acc) (number? x))
      (+ acc x)
      (add acc x)))

(define (multiply-step acc x)
  (if (and (number? acc) (number? x))
      (* acc x)
      (multiply acc x)))

(define (count-step acc x)
  (+ acc 1))

(define (last-step acc x)
  x)

(define (hashset-step acc x)
  (hashset-insert acc x))

(define (max-step acc x)
  (if (or (eq? acc nothing)
          (if (and (real? x) (real? acc))
              (> x acc)
              (greater x acc)))
      x
      acc))

(define (min-step acc x)
  (if (or (eq? acc nothing)
          (if (and (real? x) (real? acc))
              (< x acc)
              (less x acc)))
      x
      acc))

(define (string-step acc x)
  (cons (cond
          [(string? x) x]
          [(char? x) (string x)]
          [else (to-string x)])
        acc))

(define (string-finish acc)
  (apply string-append (reverse acc)))

(define (last-finish acc)
  (if (eq? acc nothing)
      (transduce-error 'generic
                       "`last` found empty list - `last` requires at least one element in the sequence")
      acc))

(define (nth-finish acc)
  (if (eq? acc nothing)
      (transduce-error 'generic "`nth` - index given is greater than the length of the iterator")
      acc))

(define (best step name)
  (Sink step
        (lambda (acc)
          (if (eq? acc nothing)
              (transduce-error 'generic
                               (string-append "`"
                                              name
                                              "` found empty list - `"
                                              name
                                              "` requires at least one element in the sequence"))
              acc))))

(define (callable? f)
  (or (function? f) (procedure? f)))

(define (hash-entry acc x)
  (cond
    [(list? x)
     (if (= (length x) 2)
         (hash-insert acc (car x) (cadr x))
         (transduce-error 'generic
                          "Hashmap iterator expects an iterable with two elements, found: "
                          x))]
    [(immutable-vector? x)
     (if (= (vector-length x) 2)
         (hash-insert acc (vector-ref x 0) (vector-ref x 1))
         (transduce-error 'generic
                          "Hashmap iterator expects an iterable with two elements, found: "
                          x))]
    [(pair? x) (hash-insert acc (car x) (cdr x))]
    [else
     (transduce-error 'type-mismatch
                      "Unable to convert: "
                      x
                      " to pair that can be used to construct a hashmap")]))


;;;;;;;;;;;;;;;;;;;;;;;;;; Transducers ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A transducer of one stage: the function that puts it in front of a sink,
;; and its argument
(struct Stage (build arg))

;; Transducers composed into one: their stages, last first, which is the order
;; they're put in front of the reducer. `invalid` is the first `taking` or
;; `dropping` with a negative count, as its name and count, or #f.
(struct Composed (stages invalid))

(define (transducer? value)
  (or (Stage? value) (Composed? value)))

(define (single build arg)
  (Stage build arg))

(define (stage-invalid stage)
  (let ([build (Stage-build stage)]
        [n (Stage-arg stage)])
    (cond
      [(and (eq? build take-stage) (< n 0)) (cons "taking" n)]
      [(and (eq? build drop-stage) (< n 0)) (cons "dropping" n)]
      [else #f])))

(define (invalid-of t)
  (if (Stage? t)
      (stage-invalid t)
      (Composed-invalid t)))

(define (stages-of t)
  (if (Stage? t)
      (list t)
      (Composed-stages t)))

(define (flatten-build arg sink)
  (flatten-stage sink))

(define (enumerate-build arg sink)
  (enumerate-stage sink))

;; The rust constructors check their arguments. A value the checks here don't
;; recognise goes to the rust one, which raises its error, or accepts it.
(define (transducer-function? f)
  (and (function? f) (not (continuation? f))))

(define (traversable? value)
  (or (list? value)
      (immutable-vector? value)
      (string? value)
      (set? value)
      (hash? value)
      (stream? value)))

(define (mapping f)
  (unless (transducer-function? f)
    (#%prim.mapping f))
  (single map-stage f))

(define (filtering f)
  (unless (transducer-function? f)
    (#%prim.filtering f))
  (single filter-stage f))

(define (flat-mapping f)
  (unless (transducer-function? f)
    (#%prim.flat-mapping f))
  (single flat-map-stage f))

(define (taking n)
  (unless (fixnum? n)
    (#%prim.taking n))
  (Stage take-stage n))

(define (dropping n)
  (unless (fixnum? n)
    (#%prim.dropping n))
  (Stage drop-stage n))

(define flattening-transducer (single flatten-build #f))
(define (flattening)
  flattening-transducer)

(define enumerating-transducer (single enumerate-build #f))
(define (enumerating)
  enumerating-transducer)

(define (extending coll)
  (unless (traversable? coll)
    (#%prim.extending coll))
  (single extend-stage coll))

(define (zipping coll)
  (unless (traversable? coll)
    (#%prim.zipping coll))
  (single zip-stage coll))

(define (interleaving coll)
  (unless (traversable? coll)
    (#%prim.interleaving coll))
  (single interleave-stage coll))

;; `a` then `b`, in the order values go through them
(define (compose-two a b)
  (Composed (append (stages-of b) (stages-of a)) (or (invalid-of a) (invalid-of b))))

(define (compose . transducers)
  (let loop ([ts transducers]
             [acc (Composed '() #f)])
    (cond
      [(null? ts) acc]
      [(transducer? (car ts)) (loop (cdr ts) (compose-two acc (car ts)))]
      [else (transduce-error 'type-mismatch "compose only accepts transducers")])))

;;;;;;;;;;;;;;;;;;;;;;;;;; Reducers ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `start` is the first result paired with the sink, or a function of `arg`
;; that makes that pair for a reducer that keeps state or takes an argument.
(struct Reducer (start arg))

(define (reducer? value)
  (Reducer? value))

(define (fixed first sink)
  (Reducer (cons first sink) #f))

(define list-reducer (fixed '() (Sink cons-step reverse)))
(define vector-reducer (fixed '() (Sink cons-step reversed-vector)))
(define sum-reducer (fixed 0 (Sink add-step identity)))
(define product-reducer (fixed 1 (Sink multiply-step identity)))
(define max-reducer (fixed nothing (best max-step "max")))
(define min-reducer (fixed nothing (best min-step "min")))
(define count-reducer (fixed 0 (Sink count-step identity)))
(define last-reducer (fixed nothing (Sink last-step last-finish)))
(define hashset-reducer (fixed (hashset) (Sink hashset-step identity)))
(define hashmap-reducer (fixed (hash) (Sink hash-entry identity)))
(define string-reducer (fixed '() (Sink string-step string-finish)))

(define (into-list)
  list-reducer)
(define (into-vector)
  vector-reducer)
(define (into-sum)
  sum-reducer)
(define (into-product)
  product-reducer)
(define (into-max)
  max-reducer)
(define (into-min)
  min-reducer)
(define (into-count)
  count-reducer)
(define (into-last)
  last-reducer)
(define (into-hashset)
  hashset-reducer)
(define (into-hashmap)
  hashmap-reducer)
(define (into-string)
  string-reducer)

(define (nth-start n)
  (let ([i 0])
    (cons nothing
          (Sink (lambda (acc x)
                  (if (= i n)
                      (Reduced x)
                      (begin
                        (set! i (+ i 1))
                        acc)))
                nth-finish))))

(define (into-nth n)
  (unless (and (fixnum? n) (>= n 0))
    (#%prim.into-nth n))
  (Reducer nth-start n))

(define (for-each-start f)
  (cons void
        (Sink (lambda (acc x)
                (unless (callable? f)
                  (transduce-error 'type-mismatch "for-each expected a function, found: " f))
                (f x)
                acc)
              identity)))

(define (into-for-each f)
  (Reducer for-each-start f))

(define (reducer-start parts)
  (let ([f (car parts)])
    (cons (cdr parts)
          (Sink (lambda (acc x)
                  (unless (callable? f)
                    (transduce-error 'type-mismatch "reduce expected a function"))
                  (f acc x))
                identity))))

(define (into-reducer f init)
  (Reducer reducer-start (cons f init)))

(define (reducer-first-and-sink reducer)
  (let ([start (Reducer-start reducer)])
    (if (pair? start)
        start
        (start (Reducer-arg reducer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; transduce ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Where `(taking 0)` is, with the sink after it. Nothing before it runs.
(struct Cut (sink))

;; One stage in front of `sink`
(define (put-stage stage sink)
  (let ([build (Stage-build stage)])
    (if (and (eq? build take-stage) (eqv? (Stage-arg stage) 0))
        (Cut sink)
        (build (Stage-arg stage) sink))))

;; Stages, last first, in front of `sink`
(define (put-stages stages sink)
  (if (null? stages)
      sink
      (let ([sink (put-stage (car stages) sink)])
        (if (Cut? sink)
            sink
            (put-stages (cdr stages) sink)))))

;; Every transducer in `ts`, in front of `sink`, the last one nearest it.
;; The last element of `ts` is the reducer.
(define (put-transducers ts sink)
  (if (null? (cdr ts))
      sink
      (let ([after (put-transducers (cdr ts) sink)])
        (if (Cut? after)
            after
            (let ([t (car ts)])
              (if (Stage? t)
                  (put-stage t after)
                  (put-stages (Composed-stages t) after)))))))

;; Checks the arguments in the order rust does, then returns the reducer
(define (checked-reducer args)
  (let loop ([rest (cdr args)]
             [invalid #f])
    (if (null? (cdr rest))
        (let ([reducer (car rest)]
              [coll (car args)])
          (unless (Reducer? reducer)
            (transduce-error 'type-mismatch
                             "transduce requires that the last argument be a reducer, found: "
                             reducer))
          (unless (or (list? coll)
                      (vector? coll)
                      (string? coll)
                      (set? coll)
                      (hash? coll)
                      (stream? coll))
            (transduce-error 'type-mismatch "value unable to be converted to an iterable: " coll))
          (when invalid
            (transduce-error 'contract-violation
                             (string-append (car invalid) " expects a non-negative integer, found: ")
                             (cdr invalid)))
          reducer)
        (let ([t (car rest)])
          (unless (transducer? t)
            (transduce-error 'type-mismatch "transduce expects a transducer, found: " t))
          (loop (cdr rest) (or invalid (invalid-of t)))))))

;;@doc
;; Runs the values of a collection through transducers, then collects them
;; with a reducer.
;;
;; (transduce collection transducer ... reducer) -> any/c
;;
;; # Examples
;; ```scheme
;; (transduce (list 1 2 3) (mapping (λ (x) (+ x 1))) (into-list)) ;; => '(2 3 4)
;; ```
(define (transduce . args)
  (unless (and (pair? args) (pair? (cdr args)))
    (transduce-error 'arity-mismatch
                     (string-append "transduce expects at least 2 arguments, found: "
                                    (number->string (length args)))))
  (let* ([reducer (checked-reducer args)]
         [start (reducer-first-and-sink reducer)]
         [sink (put-transducers (cdr args) (cdr start))])
    (if (Cut? sink)
        ((Sink-finish (Cut-sink sink)) (car start))
        ((Sink-finish sink)
         (unreduce (reduce-collection (Sink-step sink) (car start) (car args) #f))))))
