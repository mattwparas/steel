(require "cogs/transducers/transducers.scm")

(define (split-last lst)
  (define (loop accum lst)
    (if (empty? (cdr lst))
        (cons (reverse accum) (car lst))
        (loop (cons (car lst) accum) (cdr lst))))
  (loop '() lst))

(define (builtin-transducer->userspace p)
  (define kind (car p))
  (define func (cdr p))
  (case kind
    [(0) (tmap func)]
    [(1) (tfilter func)]
    [(2) (ttake func)]
    [(3) (tdrop func)]
    [(4) (tflat-map func)]
    [(5) tflatten]
    [(6) (tsegment func)]
    [(7) (ttake-while func)]
    [(8) (tdrop-while func)]
    [(9) (error "implement extend")]
    [(10) (error "implement cycle")]
    [(11) tenumerate]
    [(12) (error "implement zip")]
    [(13) (error "implement interleaving")]
    [(14) (error "implement map pair")]
    [else p]))

(define (builtin-reduce->userspace reducer)
  (define kind (#%reducer->int reducer))
  (cond
    [(int? kind)
     (case kind
       ; [(0) (new-into-sum)]
       ; [(1) (new-into-product)]
       ; [(2) (new-into-max)]
       ; [(3) (new-into-min)]
       [(4) (rcount)]
       [(5) (new-into-list)]
       [(6) (new-into-vector)]
       [(7) (new-into-hashmap)]
       [(8) (new-into-hashset)]
       ; [(9) (new-into-string)]
       ; [(10) (new-into-last)]
       ; [(11) (new-into-for-each)]
       ; [(12) (new-into-nth)]
       ; [(13) (new-into-reducer)]
       [else (error "Unknown or unimplemented reducer: " kind)])]

    [(list? kind) (error "unimplemented generic reducer")]
    [(pair? kind) (new-into-for-each (cdr kind))]
    [else (error "unknown reducer")]))

(define (all func lst)
  (if (null? lst)
      #t
      (if (func (car lst))
          (all func (cdr lst))
          #f)))

(define (any lst)
  (if (null? lst)
      #f
      (if (car lst)
          #t
          (any (cdr lst)))))

(define (all-function-pointer? p)
  (all #%function-pointer? p))

(define (builtin->transducer transducers)
  ;; Check: If these all
  (define conversions (#%transducers->funcs transducers))

  (define converted (map (lambda (p) (builtin-transducer->userspace p)) conversions))

  converted)

;; TODO: Provide a shim API for converting
;; these back to the new way of doing things
(define (transduce collection . args)
  (define split (split-last args))
  ;; Use the old API, convert to the new API
  (define transducers (builtin->transducer (car split)))

  ;; TODO: map the reducers as well!
  (define reducer (builtin-reduce->userspace (cdr split)))

  (new-transduce (cond
                   [(empty? transducers) (tmap (lambda (x) x))]
                   [(> (length transducers) 1) (apply compose transducers)]
                   [else (car transducers)])
                 reducer
                 collection))
