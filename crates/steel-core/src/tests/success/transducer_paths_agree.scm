;; `transduce` runs a chain of native functions in rust and anything else in
;; scheme. Both have to agree on every result, every error, and the order of
;; every call they make. Streams are scheme values that only the scheme path
;; reads, so where one is involved the rust path gets the same values as a
;; list, and the comparison leaves out when the stream was forced. What the
;; scheme path forces is checked on its own below.

(define log '())
(define (note! . what)
  (set! log (cons what log)))

;; Each collection is made fresh, since running one can change it. Hash sets
;; and maps are shared, since two with the same contents can iterate in
;; different orders.
(define a-hashset (hashset 1 2 3))
(define a-hashmap (hash 'a 1 'b 2))
(define (finite-stream)
  (stream-cons 1
               (lambda ()
                 (note! 'force 2)
                 (stream-cons 2
                              (lambda ()
                                (note! 'force 3)
                                (stream-cons 3 (lambda () empty-stream)))))))

(define (naturals n)
  (stream-cons n
               (lambda ()
                 (note! 'force (+ n 1))
                 (naturals (+ n 1)))))

(define collections
  (list (list 'empty (lambda () '()))
        (list 'list (lambda () (list 1 2 3 4 5 6)))
        (list 'nested (lambda () (list (list 1 2) (vector 3) "ab" '())))
        (list 'vector (lambda () (vector 1 2 3)))
        (list 'mutable-vector (lambda () (mutable-vector 1 2 3)))
        (list 'string (lambda () "abc"))
        (list 'hashset (lambda () a-hashset))
        (list 'hashmap (lambda () a-hashmap))
        (list 'stream finite-stream (lambda () (list 1 2 3)))
        (list 'not-a-collection (lambda () 5))))

;; Chains that end on their own, and those that need something to stop an
;; infinite stream
(define (logged-map x)
  (note! 'map x)
  (list x))
(define (logged-filter x)
  (note! 'filter x)
  (not (equal? x 2)))
(define (logged-flat-map x)
  (note! 'flat-map x)
  (list x x))

(define chains
  (list (list 'none (lambda () '()))
        (list 'map (lambda () (list (mapping logged-map))))
        (list 'map-native (lambda () (list (mapping list))))
        (list 'filter (lambda () (list (filtering logged-filter))))
        (list 'filter-native (lambda () (list (filtering number?))))
        (list 'take-0 (lambda () (list (taking 0))))
        (list 'take-2 (lambda () (list (taking 2))))
        (list 'take-10 (lambda () (list (taking 10))))
        (list 'drop-1 (lambda () (list (dropping 1))))
        (list 'drop-10 (lambda () (list (dropping 10))))
        (list 'flat-map (lambda () (list (flat-mapping logged-flat-map))))
        (list 'flat-map-native (lambda () (list (flat-mapping list))))
        (list 'flatten (lambda () (list (flattening))))
        (list 'extend-list (lambda () (list (extending '(9 8)))))
        (list 'extend-string (lambda () (list (extending "xy"))))
        (list 'extend-set (lambda () (list (extending (hashset 7)))))
        (list 'extend-stream
              (lambda () (list (extending (finite-stream))))
              (lambda () (list (extending '(1 2 3)))))
        (list 'enumerate (lambda () (list (enumerating))))
        (list 'zip (lambda () (list (zipping '(a b)))))
        (list 'zip-stream
              (lambda () (list (zipping (naturals 10))))
              (lambda () (list (zipping (range 10 40)))))
        (list 'interleave (lambda () (list (interleaving '(x y z w)))))
        (list 'interleave-short (lambda () (list (interleaving '(x)))))
        (list 'take-then-extend (lambda () (list (taking 2) (extending '(9)))))
        (list 'extend-then-take (lambda () (list (extending '(9)) (taking 2))))
        (list 'map-then-take (lambda () (list (mapping logged-map) (taking 1))))
        (list 'take-0-then-map (lambda () (list (taking 0) (mapping logged-map))))
        (list 'take-0-then-extend (lambda () (list (taking 0) (extending '(9)))))
        (list 'enumerate-take-interleave
              (lambda () (list (enumerating) (taking 2) (interleaving '(q)))))
        (list 'drop-flat-map-take
              (lambda () (list (dropping 1) (flat-mapping logged-flat-map) (taking 3))))
        (list 'composed
              (lambda () (list (compose (mapping logged-map) (filtering logged-filter)))))
        (list 'interleave-take (lambda () (list (interleaving '(x y z w)) (taking 3))))))

(define (logged-for-each x)
  (note! 'for-each x))

(define reducers
  (list (list 'list into-list)
        (list 'vector into-vector)
        (list 'sum into-sum)
        (list 'product into-product)
        (list 'max into-max)
        (list 'min into-min)
        (list 'count into-count)
        (list 'last into-last)
        (list 'nth-1 (lambda () (into-nth 1)))
        (list 'nth-50 (lambda () (into-nth 50)))
        (list 'hashset into-hashset)
        (list 'hashmap into-hashmap)
        (list 'string into-string)
        (list 'for-each (lambda () (into-for-each logged-for-each)))
        (list 'reducer (lambda () (into-reducer (lambda (acc x) (cons x acc)) '())))))

;; What one path does: its result or error, and what was called on the way
(define (outcome run collection chain reducer)
  (set! log '())
  (let ([result (with-handler (lambda (e) (list 'error (error-object-message e)))
                              (list 'ok (apply run (append (list collection) chain (list reducer)))))])
    (list result (reverse log))))

(define (rust . args)
  (apply #%prim.transduce args))

(define mismatches '())
(define runs 0)

;; A case's values for the scheme path, and for the rust path
(define (scheme-version case)
  ((cadr case)))
(define (rust-version case)
  (if (null? (cddr case))
      ((cadr case))
      ((caddr case))))
(define (has-list-version? case)
  (not (null? (cddr case))))

(define (without-forcing outcome)
  (list (car outcome) (filter (lambda (entry) (not (equal? (car entry) 'force))) (cadr outcome))))

(define (compare! collection-case chain-case reducer-case)
  (let* ([a (outcome rust
                     (rust-version collection-case)
                     (rust-version chain-case)
                     ((cadr reducer-case)))]
         [b (outcome #%scheme-transduce
                     (scheme-version collection-case)
                     (scheme-version chain-case)
                     ((cadr reducer-case)))]
         [streams? (or (has-list-version? collection-case) (has-list-version? chain-case))]
         [a (if streams? (without-forcing a) a)]
         [b (if streams? (without-forcing b) b)])
    (set! runs (+ runs 1))
    (unless (equal? a b)
      (set! mismatches
            (cons (list (car collection-case) (car chain-case) (car reducer-case) 'rust a 'scheme b)
                  mismatches)))))

(for-each (lambda (collection-case)
            (for-each (lambda (chain-case)
                        (for-each (lambda (reducer-case)
                                    (compare! collection-case chain-case reducer-case))
                                  reducers))
                      chains))
          collections)

;; An infinite stream, with chains that stop on their own
(define infinite (list 'naturals (lambda () (naturals 0)) (lambda () (range 0 50))))
(for-each (lambda (chain-case)
            (for-each (lambda (reducer-case) (compare! infinite chain-case reducer-case))
                      (list (list 'list into-list) (list 'count into-count))))
          (list (list 'take-3 (lambda () (list (taking 3))))
                (list 'map-take (lambda () (list (mapping logged-map) (taking 2))))
                (list 'zip-short (lambda () (list (zipping '(a b)))))
                (list 'enumerate-take (lambda () (list (enumerating) (taking 2))))))

(for-each (lambda (m) (displayln m)) (reverse mismatches))
(unless (null? mismatches)
  (error "rust and scheme transduce disagree" (length mismatches) "of" runs))

;; A stream's rest is forced only when its next value is needed
(define (forced-by thunk)
  (set! log '())
  (let ([result (thunk)])
    (list result (reverse log))))
(assert! (equal? (forced-by (lambda () (transduce (naturals 0) (taking 3) (into-list))))
                 '((0 1 2) ((force 1) (force 2)))))
(assert! (equal? (forced-by (lambda () (transduce (finite-stream) (into-count))))
                 '(3 ((force 2) (force 3)))))
(assert! (equal? (forced-by (lambda () (transduce (list 1 2) (zipping (naturals 10)) (into-list))))
                 '(((1 10) (2 11)) ((force 11)))))
(assert! (equal? (forced-by (lambda () (transduce (list 1) (interleaving (naturals 10)) (taking 3) (into-list))))
                 '((1 10 11) ((force 11)))))
(assert! (equal? (forced-by (lambda () (transduce (list 1 2) (extending (naturals 5)) (taking 3) (into-list))))
                 '((1 2 5) ())))

;; A continuation captured in a function `transduce` calls can be called again
;; once it has returned, since the scheme path runs it like any other code.
(define (reenter)
  (define kept #f)
  (define runs 0)
  (define result
    (transduce (list 1 2 3)
               (mapping (lambda (x)
                          (call/cc (lambda (k)
                                     (when (= x 2)
                                       (set! kept k))
                                     x))))
               (into-list)))
  (set! runs (+ runs 1))
  (if (= runs 1)
      (kept 20)
      (list runs result)))
(assert! (equal? (reenter) '(2 (1 20 3))))
