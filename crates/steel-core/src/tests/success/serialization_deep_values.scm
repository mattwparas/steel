;; Values nested very deeply: a long chain of pairs, lists in lists, and so
;; on. Serializing one, turning it to bytes and back, reading it and dropping
;; it take no more stack for a deep value than for a shallow one, on a thread
;; with a small stack as well as on this one.
(define depth 100000)

(define (pair-chain n acc)
  (if (= n 0) acc (pair-chain (- n 1) (cons n acc))))
(define (pairs-in-cars n acc)
  (if (= n 0) acc (pairs-in-cars (- n 1) (cons acc n))))
(define (lists-in-lists n acc)
  (if (= n 0) acc (lists-in-lists (- n 1) (list acc))))
(define (vectors-in-vectors n acc)
  (if (= n 0) acc (vectors-in-vectors (- n 1) (immutable-vector acc))))
(define (hashes-in-hashes n acc)
  (if (= n 0) acc (hashes-in-hashes (- n 1) (hash 'in acc))))
(define (boxes-in-boxes n acc)
  (if (= n 0) acc (boxes-in-boxes (- n 1) (box acc))))
(define (mixed n acc)
  (if (= n 0)
      acc
      (mixed (- n 1)
             (let ([kind (modulo n 4)])
               (cond
                 [(= kind 0) (cons acc n)]
                 [(= kind 1) (list acc)]
                 [(= kind 2) (immutable-vector acc)]
                 [else (hash 'in acc)])))))

;; How deep each kind goes, counted without going down the stack.
(define (down-cdrs v n)
  (if (pair? v) (down-cdrs (cdr v) (+ n 1)) n))
(define (down-cars v n)
  (if (pair? v) (down-cars (car v) (+ n 1)) n))
(define (down-lists v n)
  (if (and (list? v) (not (null? v))) (down-lists (car v) (+ n 1)) n))
(define (down-vectors v n)
  (if (vector? v) (down-vectors (vector-ref v 0) (+ n 1)) n))
(define (down-hashes v n)
  (if (hash? v) (down-hashes (hash-ref v 'in) (+ n 1)) n))
(define (down-boxes v n)
  (if (symbol? v) n (down-boxes (unbox v) (+ n 1))))
(define (down-mixed v n)
  (cond
    [(symbol? v) n]
    [(hash? v) (down-mixed (hash-ref v 'in) (+ n 1))]
    [(vector? v) (down-mixed (vector-ref v 0) (+ n 1))]
    [(and (list? v) (not (null? v))) (down-mixed (car v) (+ n 1))]
    [else (down-mixed (car v) (+ n 1))]))

(define (by-value v) (deserialize-value (serialize-value v)))
(define (by-bytes v) (deserialize-value (bytes->serialized (serialized->bytes (serialize-value v)))))

(define (comes-back-as-deep? make down)
  (and (= (down (by-value (make depth 'end)) 0) depth)
       (= (down (by-bytes (make depth 'end)) 0) depth)))

(define (all-kinds)
  (and (comes-back-as-deep? pair-chain down-cdrs)
       (comes-back-as-deep? pairs-in-cars down-cars)
       (comes-back-as-deep? lists-in-lists down-lists)
       (comes-back-as-deep? vectors-in-vectors down-vectors)
       (comes-back-as-deep? hashes-in-hashes down-hashes)
       (comes-back-as-deep? boxes-in-boxes down-boxes)
       (comes-back-as-deep? mixed down-mixed)))

(assert! (all-kinds))
;; A thread other than the first has less stack to start with.
(assert! (thread-join! (spawn-native-thread all-kinds)))

;; A serialized value that deep can be read again and again, and let go of.
(define (read-twice)
  (let* ([serialized (serialize-value (pair-chain depth 'end))]
         [first (deserialize-value serialized)]
         [second (deserialize-value serialized)])
    (and (= (down-cdrs first 0) depth) (= (down-cdrs second 0) depth))))
(assert! (read-twice))
(assert! (thread-join! (spawn-native-thread read-twice)))

;; The values keep what they hold on the way.
(define labelled (by-bytes (pair-chain depth 'end)))
(assert! (= (car labelled) 1))
(assert! (= (car (cdr labelled)) 2))
(define (last-cdr v)
  (if (pair? v) (last-cdr (cdr v)) v))
(assert! (eq? (last-cdr labelled) 'end))

;; Long chains of pairs can be made and let go of with nothing serialized.
(define (make-and-drop make)
  (let ([chain (make 400000 'end)])
    (pair? chain)))
(assert! (make-and-drop pair-chain))
(assert! (make-and-drop pairs-in-cars))
(assert! (thread-join! (spawn-native-thread (lambda () (and (make-and-drop pair-chain) (make-and-drop pairs-in-cars))))))
;; A chain that something else still holds part of.
(define tail (pair-chain 1000 'end))
(define (drop-front)
  (let ([chain (pair-chain 200000 tail)])
    (pair? chain)))
(assert! (drop-front))
(assert! (= (down-cdrs tail 0) 1000))

;; There is no depth at which it stops. A longer chain is only more of the
;; same work as a shorter one.
(assert! (= (down-cdrs (by-bytes (pair-chain 500000 'end)) 0) 500000))
(assert! (= (down-lists (by-value (lists-in-lists 500000 'end)) 0) 500000))

;; Bytes say which form they are in, and any other is refused.
(define (refused? thunk)
  (eq? 'refused (with-handler (lambda (e) 'refused) (thunk))))
(define honest (serialized->bytes (serialize-value (list 1 2 3))))
(define (with-first-byte payload first)
  (let ([out (bytes first)])
    (let copy ([i 1])
      (when (< i (bytes-length payload))
        (bytes-push! out (bytes-ref payload i))
        (copy (+ i 1))))
    out))
(assert! (equal? (deserialize-value (bytes->serialized (with-first-byte honest (bytes-ref honest 0))))
                 (list 1 2 3)))
(assert! (refused? (lambda () (bytes->serialized (with-first-byte honest 0)))))
(assert! (refused? (lambda () (bytes->serialized (with-first-byte honest 1)))))
(assert! (refused? (lambda () (bytes->serialized (with-first-byte honest 255)))))
(assert! (refused? (lambda () (bytes->serialized (bytes)))))
