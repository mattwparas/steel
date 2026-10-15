(provide transduce
         #%scheme-transduce)

(require "#%private/steel/streams")

;; `transduce` runs a chain made only of native functions in rust, which calls
;; them directly. Anything else runs here, where the compiler and the jit see
;; every call. Both give the same results, in the same order, with the same
;; errors.

;; What a reduction step returns to stop the transduction early
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

;; Stages and reducers come from `#%transducer-stages` and `#%reducer-parts` with
;; their kinds as numbers, which those functions list.

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

;; The first result paired with where values go
(define (reducer->sink parts)
  (case (car parts)
    [(6) (cons '() (Sink cons-step reverse))]
    [(7) (cons '() (Sink cons-step reversed-vector))]
    [(0) (cons 0 (Sink add-step identity))]
    [(1) (cons 1 (Sink multiply-step identity))]
    [(2) (cons nothing (best max-step "max"))]
    [(3) (cons nothing (best min-step "min"))]
    [(4) (cons 0 (Sink count-step identity))]
    [(11) (cons nothing (Sink last-step last-finish))]
    [(5)
     (let ([n (cadr parts)]
           [i 0])
       (cons
        nothing
        (Sink (lambda (acc x)
                (if (= i n)
                    (Reduced x)
                    (begin
                      (set! i (+ i 1))
                      acc)))
              nth-finish)))]
    [(9) (cons (hashset) (Sink hashset-step identity))]
    [(8) (cons (hash) (Sink hash-entry identity))]
    [(10) (cons '() (Sink string-step string-finish))]
    [(12)
     (let ([f (cadr parts)])
       (cons void
             (Sink (lambda (acc x)
                     (unless (callable? f)
                       (transduce-error 'type-mismatch "for-each expected a function, found: " f))
                     (f x)
                     acc)
                   identity)))]
    [(13)
     (let ([f (cadr parts)])
       (cons (caddr parts)
             (Sink (lambda (acc x)
                     (unless (callable? f)
                       (transduce-error 'type-mismatch "reduce expected a function"))
                     (f acc x))
                   identity)))]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;; transduce ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stage->sink stage sink)
  (let ([argument (cdr stage)])
    (case (car stage)
      [(0) (map-stage argument sink)]
      [(1) (filter-stage argument sink)]
      [(2) (take-stage argument sink)]
      [(3) (drop-stage argument sink)]
      [(4) (flat-map-stage argument sink)]
      [(5) (flatten-stage sink)]
      [(6) (extend-stage argument sink)]
      [(7) (enumerate-stage sink)]
      [(8) (zip-stage argument sink)]
      [(9) (interleave-stage argument sink)]
      [else (transduce-error 'generic "transduce: this transducer is not implemented")])))

;; `stages` come last first, from `#%transduce-plan`, which also checked them
(define (run-transduce coll stages parts)
  (let ([start (reducer->sink parts)])
    ;; `(taking 0)` asks for nothing, so nothing before it runs
    (let build ([stages stages]
                [sink (cdr start)])
      (cond
        [(null? stages)
         ((Sink-finish sink) (unreduce (reduce-collection (Sink-step sink) (car start) coll #f)))]
        [(and (= (car (car stages)) 2) (= (cdr (car stages)) 0)) ((Sink-finish sink) (car start))]
        [else (build (cdr stages) (stage->sink (car stages) sink))]))))

;; Whether rust can run a stage: it calls native functions directly, but
;; streams are scheme values it can't read
(define (native-function-stage? stage)
  (case (car stage)
    [(0 1 4) (#%function-pointer? (cdr stage))]
    [(6 8 9) (not (stream? (cdr stage)))]
    [else #t]))

(define (native-reducer? parts)
  (case (car parts)
    [(12 13) (#%function-pointer? (cadr parts))]
    [else #t]))

(define (all-native? stages)
  (cond
    [(null? stages) #t]
    [(native-function-stage? (car stages)) (all-native? (cdr stages))]
    [else #f]))

(define (plan args)
  (#%transduce-plan args (and (pair? args) (stream? (car args)))))

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
  (let ([plan (plan args)])
    (if (and (not (stream? (car args))) (native-reducer? (cdr plan)) (all-native? (car plan)))
        (apply #%prim.transduce args)
        (run-transduce (car args) (car plan) (cdr plan)))))

;; `transduce` without the rust path, for comparing the two
(define (#%scheme-transduce . args)
  (let ([plan (plan args)]) (run-transduce (car args) (car plan) (cdr plan))))
