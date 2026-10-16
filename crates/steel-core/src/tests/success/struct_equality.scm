;; equal? and hashing compare a transparent struct by its fields, leaving out
;; the ones it names in #:equality-skip, and an opaque struct by identity.

(struct opaque (a b))
(struct clear (a b) #:transparent)
(struct cached (x y memo) #:transparent #:equality-skip (memo))
(struct cell (v) #:mutable)
(struct tcell (v) #:mutable #:transparent)
(struct mcached (x memo) #:mutable #:transparent #:equality-skip (memo))

(define o (opaque 1 2))
(assert! (equal? o o))
(assert! (not (equal? (opaque 1 2) (opaque 1 2))))
(assert! (equal? (clear 1 2) (clear 1 2)))
(assert! (not (equal? (clear 1 2) (clear 1 3))))

(assert! (equal? (cached 1 2 'a) (cached 1 2 'b)))
(assert! (not (equal? (cached 1 3 'a) (cached 1 2 'a))))
(assert! (= (hash-code (cached 1 2 'a)) (hash-code (cached 1 2 'b))))
(assert! (eq? (hash-try-get (hash (cached 1 2 'a) 'found) (cached 1 2 'other)) 'found))
(assert! (hashset-contains? (hashset (cached 1 2 'a)) (cached 1 2 'b)))

(assert! (not (equal? (cell 1) (cell 1))))
(assert! (equal? (tcell 1) (tcell 1)))
(assert! (equal? (mcached 1 'a) (mcached 1 'b)))

;; An opaque key is found by identity, also after it changes
(define key (cell 1))
(define table (hash key 'x))
(set-cell-v! key 2)
(assert! (eq? (hash-try-get table key) 'x))
(assert! (not (hash-try-get (hash o 'yes) (opaque 1 2))))
(assert! (eq? (hash-try-get (hash o 'yes) o) 'yes))

;; Inside other values
(assert! (equal? (list (clear 1 (cached 2 3 'a))) (list (clear 1 (cached 2 3 'b)))))
(assert! (not (equal? (vector (opaque 1 2)) (vector (opaque 1 2)))))
(assert! (equal? (vector o) (vector o)))
(assert! (not (member (opaque 1 2) (list (opaque 1 2)))))
(assert! (if (member o (list o)) #t #f))

;; The built in types compare by their contents
(assert! (equal? (Some 1) (Some 1)))
(assert! (equal? (None) (None)))
(assert! (equal? (Ok (list 1)) (Ok (list 1))))
(assert! (equal? (Err "x") (Err "x")))
(assert! (not (equal? (Ok 1) (Err 1))))
