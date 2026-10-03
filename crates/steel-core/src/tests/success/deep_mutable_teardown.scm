;; Dropping deep chains of mutable structs and boxes must not recurse once
;; per link: a released box's contents are dropped in place only while
;; releases nest shallowly.
(struct mpair (a d) #:mutable)

(define (build-chain n)
  (let loop ([i 0]
             [acc '()])
    (if (= i n)
        acc
        (loop (+ i 1) (mpair i acc)))))

(define (chain-length c)
  (let loop ([c c]
             [n 0])
    (if (mpair? c)
        (loop (mpair-d c) (+ n 1))
        n)))

(define (build-and-drop n)
  (chain-length (build-chain n)))

(assert! (equal? (build-and-drop 1000000) 1000000))
(assert! (equal? (build-and-drop 1000000) 1000000))

;; Nested single field mutable structs, each the only handle to the next.
(struct cell (v) #:mutable)
(define (nest n)
  (let loop ([i 0]
             [c 'bottom])
    (if (= i n)
        c
        (loop (+ i 1) (cell c)))))
(define (depth c)
  (let loop ([c c]
             [n 0])
    (if (cell? c)
        (loop (cell-v c) (+ n 1))
        n)))
(assert! (equal? (depth (nest 1000000)) 1000000))

;; A chain through mutable vectors.
(define (vec-chain n)
  (let loop ([i 0]
             [acc #f])
    (if (= i n)
        acc
        (loop (+ i 1) (mutable-vector i acc)))))
(define (vec-length v)
  (let loop ([v v]
             [n 0])
    (if v
        (loop (vector-ref v 1) (+ n 1))
        n)))
(assert! (equal? (vec-length (vec-chain 1000000)) 1000000))
