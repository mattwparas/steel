;; Struct constructors, getters and predicates, reached directly and through a
;; value the jit can't resolve at compile time.
(struct Pair (a b))
(struct Quad (a b c d))

(define (id x) x)
(define (via f . args) (apply f args))

(define p (Pair 1 2))
(define q (Quad 'w 'x 'y 'z))

(assert! (equal? (list (Pair? p) (Quad? q) (Pair? q) (Pair-a p) (Pair-b p))
                 '(#true #true #false 1 2)))
(assert! (equal? (list (Quad-a q) (Quad-b q) (Quad-c q) (Quad-d q)) '(w x y z)))
(assert! (Pair? (via Pair 10 20)))
(assert! (Quad? (via Quad 1 2 3 4)))
(assert! (equal? (Pair-a (via Pair 10 20)) 10))
(assert! (equal? (Quad-d (via Quad 1 2 3 4)) 4))

(define (branchy a b c) (Quad a (if c (id b) 0) c (and c (id a))))

(assert! (equal? ((lambda (f) (Quad-a (f 'A 'B 'C))) branchy) 'A))
(assert! (equal? ((lambda (f) (Quad-b (f 'A 'B 'C))) branchy) 'B))
(assert! (equal? ((lambda (f) (Quad-c (f 'A 'B 'C))) branchy) 'C))
(assert! (equal? ((lambda (f) (Quad-d (f 'A 'B 'C))) branchy) 'A))
