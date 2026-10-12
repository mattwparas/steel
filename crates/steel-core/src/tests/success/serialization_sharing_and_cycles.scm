;; What a serialized value keeps of how its parts are shared, which parts
;; hold which, and what hash maps and sets are keyed on.

(define (by-value v) (deserialize-value (serialize-value v)))
(define (by-bytes v) (deserialize-value (bytes->serialized (serialized->bytes (serialize-value v)))))

(struct pt (x y))
(struct cell (contents) #:mutable #:transparent)

(define (check-both check)
  (assert! (check by-value))
  (assert! (check by-bytes)))

;; Boxes, mutable vectors and mutable structs as keys. Each hashes as what
;; it holds, so it has to hold it by the time the map is made.
(check-both (lambda (copy) (= 2 (hashset-length (copy (hashset (box 1) (box 2)))))))
(check-both (lambda (copy)
              (let ([h (copy (hash (mutable-vector 1 2) 'a (mutable-vector 3) 'b))])
                (and (= 2 (hash-length h))
                     (eq? 'a (hash-try-get h (mutable-vector 1 2)))
                     (eq? 'b (hash-try-get h (mutable-vector 3)))))))
(check-both (lambda (copy)
              (let ([h (copy (hash (cell 1) 'a (cell 2) 'b))])
                (and (= 2 (hash-length h))
                     (eq? 'a (hash-try-get h (cell 1)))
                     (eq? 'b (hash-try-get h (cell 2)))))))
(check-both (lambda (copy)
              (let ([h (copy (hash (list (box 1)) 'a (list (box (mutable-vector 2))) 'b))])
                (and (= 2 (hash-length h))
                     (eq? 'a (hash-try-get h (list (box 1))))
                     (eq? 'b (hash-try-get h (list (box (mutable-vector 2)))))))))
;; A map keyed on boxes, inside a box that one of its values refers back to.
(check-both (lambda (copy)
              (let* ([outer (box #f)]
                     [h (hash (box 1) outer (box 2) 'two)])
                (set-box! outer h)
                (let ([r (copy outer)])
                  (and (= 2 (hash-length (unbox r)))
                       (eq? r (hash-try-get (unbox r) (box 1)))
                       (eq? 'two (hash-try-get (unbox r) (box 2))))))))
;; A hash set that is itself a key.
(check-both (lambda (copy)
              (let ([h (copy (hash (hashset (box 1)) 'a (hashset (box 2)) 'b))])
                (and (= 2 (hash-length h)) (eq? 'b (hash-try-get h (hashset (box 2))))))))

;; A cycle that leaves a box and comes back to it through values that
;; can't be changed, starting from each of them.
(check-both (lambda (copy)
              (let* ([b (box #f)]
                     [p (cons b 1)])
                (set-box! b (cons p 2))
                (let ([r (copy p)]) (eq? (car (unbox (car r))) r)))))
(check-both (lambda (copy)
              (let* ([b (box #f)]
                     [v (immutable-vector b)])
                (set-box! b (hash 'k v))
                (let ([r (copy v)]) (eq? (hash-ref (unbox (vector-ref r 0)) 'k) r)))))
(check-both (lambda (copy)
              (let* ([b (box #f)]
                     [p (pt b 1)])
                (set-box! b (immutable-vector p))
                (let ([r (copy p)]) (eq? (vector-ref (unbox (pt-x r)) 0) r)))))
(check-both (lambda (copy)
              (let* ([b (box #f)]
                     [f (lambda () (unbox b))])
                (set-box! b (hash 'me f))
                (let ([r (copy f)]) (eq? (hash-ref (r) 'me) r)))))
(check-both (lambda (copy)
              (let* ([b (box #f)]
                     [l (list b 'tail)])
                (set-box! b (list l))
                (let ([r (copy l)]) (eq? (car (car (unbox (car r)))) (car r))))))
(check-both (lambda (copy)
              (let* ([v (mutable-vector #f 'x)]
                     [c (cell (list (cons v 1)))])
                (vector-set! v 0 (immutable-vector c c))
                (let ([r (copy c)])
                  (and (eq? (vector-ref (vector-ref (car (car (cell-contents r))) 0) 0) r)
                       (eq? (vector-ref (vector-ref (car (car (cell-contents r))) 0) 1) r))))))
;; Two cycles that share a part.
(check-both (lambda (copy)
              (let* ([a (box #f)]
                     [b (box #f)]
                     [shared (list a b)])
                (set-box! a (cons shared 'a))
                (set-box! b (cons shared 'b))
                (let ([r (copy shared)])
                  (and (eq? (car (unbox (car r))) (car (unbox (cadr r))))
                       (eq? 'a (cdr (unbox (car r))))
                       (eq? 'b (cdr (unbox (cadr r)))))))))

;; A list referred to twice is one list, and what is serialized grows with
;; the lists there are and not with the ways there are to reach them.
(check-both (lambda (copy)
              (let* ([shared (list 1 2 3)]
                     [r (copy (list shared shared (box shared)))])
                (and (equal? (car r) (list 1 2 3))
                     (eq? (car r) (cadr r))
                     (eq? (car r) (unbox (caddr r)))))))
(define (doubled n)
  (let loop ([i 0]
             [l (list 1)])
    (if (= i n) l (loop (+ i 1) (list l l)))))
(define (depth-of l n)
  (if (pair? (car l)) (depth-of (car l) (+ n 1)) n))
(let ([s (serialize-value (doubled 60))])
  (assert! (< (bytes-length (serialized->bytes s)) 100000))
  (let ([r (deserialize-value s)])
    (assert! (= 60 (depth-of r 0)))
    (assert! (eq? (car r) (cadr r)))))
;; A list and its tail are two lists.
(check-both (lambda (copy)
              (let* ([whole (list 1 2 3 4)]
                     [r (copy (list whole (cdr whole) (cddr whole) whole))])
                (and (equal? r (list (list 1 2 3 4) (list 2 3 4) (list 3 4) (list 1 2 3 4)))
                     (eq? (car r) (cadddr r))))))
(check-both (lambda (copy)
              (let* ([long (range 0 1000)]
                     [r (copy (list long (list-tail long 300) (list-tail long 999)))])
                (and (equal? (car r) long)
                     (equal? (cadr r) (list-tail long 300))
                     (equal? (caddr r) (list 999))))))

;; Tails are sent as part of their list, and a shared tail is sent once. The
;; size grows with the number of elements, and sending the result on is no
;; bigger.
(define (tails l)
  (if (null? l) '() (cons l (tails (cdr l)))))
(define (bytes-of v)
  (bytes-length (serialized->bytes (serialize-value v))))
(define (no-bigger-than-a-few? v whole)
  (< (bytes-of v) (* 4 (bytes-of whole))))
(let* ([long (range 0 3000)]
       [all (tails long)])
  (assert! (no-bigger-than-a-few? all long))
  (check-both (lambda (copy)
                (let ([r (copy all)])
                  (and (equal? r all) (no-bigger-than-a-few? r long))))))
;; Tails before the list
(let* ([long (range 0 3000)]
       [backwards (list (list-tail long 2000) (cddr long) (cdr long) long)])
  (check-both (lambda (copy)
                (let ([r (copy backwards)])
                  (and (equal? r backwards) (no-bigger-than-a-few? r long))))))
;; Lists consed onto one list
(let* ([long (range 0 3000)]
       [fronts (map (lambda (i) (cons i long)) (range 0 200))])
  (check-both (lambda (copy)
                (let ([r (copy fronts)])
                  (and (equal? r fronts) (no-bigger-than-a-few? r long))))))
;; A list that starts with another list's elements and goes on
(check-both (lambda (copy)
              (let* ([start (range 0 600)]
                     [longer (append start (range 1000 1600))]
                     [r (copy (list start longer start))])
                (and (equal? (car r) start)
                     (equal? (cadr r) longer)
                     (= 1200 (length (cadr r)))))))

;; A bytevector referred to twice is one bytevector.
(check-both (lambda (copy)
              (let* ([b (bytes 1 2 3)]
                     [r (copy (list b b (bytes 1 2 3)))])
                (bytes-set! (car r) 0 99)
                (and (eq? (car r) (cadr r))
                     (= 99 (bytes-ref (cadr r) 0))
                     (= 1 (bytes-ref (caddr r) 0))))))
