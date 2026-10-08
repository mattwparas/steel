;; Loops that count a fixnum up or down to a bound. Compiled code runs these a
;; stretch of trips at a time and only looks for other threads between
;; stretches, so the answers are checked for counts on both sides of a
;; stretch's end, and threads in such loops still have to stop when asked.
(require-builtin steel/time)

(define (up-below i n acc)
  (if (< i n) (up-below (+ i 1) n (+ acc i)) acc))
(define (up-through i n acc)
  (if (<= i n) (up-through (+ i 1) n (+ acc i)) acc))
(define (up-until-equal i n acc)
  (if (= i n) acc (up-until-equal (+ i 1) n (+ acc i))))
(define (up-by-three i n acc)
  (if (>= i n) acc (up-by-three (+ i 3) n (+ acc 1))))
(define (down-to-zero i acc)
  (if (< i 0) acc (down-to-zero (- i 1) (+ acc i))))
(define (down-above i n acc)
  (if (> i n) (down-above (- i 1) n (+ acc i)) acc))
(define (down-until-equal i acc)
  (if (= i 0) acc (down-until-equal (- i 1) (+ acc i))))
(define (down-by-seven i n acc)
  (if (<= i n) acc (down-by-seven (- i 7) n (+ acc 1))))
(define (through-a-vector v i acc)
  (if (>= i (vector-length v)) acc (through-a-vector v (+ i 1) (+ acc (vector-ref v i)))))
(define (below-a-constant i acc)
  (if (< i 5000) (below-a-constant (+ i 1) (+ acc i)) acc))

;; 0 + 1 + ... + (n - 1)
(define (triangle n) (quotient (* n (- n 1)) 2))
(define (trips from to step) (if (>= from to) 0 (quotient (+ (- to from) (- step 1)) step)))

(define (check n)
  (assert! (= (up-below 0 n 0) (triangle n)))
  (assert! (= (up-through 0 n 0) (triangle (+ n 1))))
  (assert! (= (up-until-equal 0 n 0) (triangle n)))
  (assert! (= (up-by-three 0 n 0) (trips 0 n 3)))
  (assert! (= (down-to-zero n 0) (triangle (+ n 1))))
  (assert! (= (down-above n 0 0) (triangle (+ n 1))))
  (assert! (= (down-until-equal n 0) (triangle (+ n 1))))
  (assert! (= (down-by-seven n 0 0) (trips 0 n 7)))
  ;; Starting part way, and from the far side of zero.
  (assert! (= (up-below 5 n 0) (- (triangle (max n 5)) (triangle 5))))
  (assert! (= (up-below -10 n 0) (- (triangle n) 55)))
  ;; Already past the bound, so no trips at all.
  (assert! (= (up-below (+ n 1) n 7) 7))
  (assert! (= (down-above -1 0 7) 7))
  (assert! (= (down-to-zero -1 7) 7)))

;; Enough of each to be compiled, then the counts around the end of a
;; stretch, and of several.
(define (warm n)
  (when (> n 0)
    (check 300)
    (warm (- n 1))))
(warm 200)
(for-each check (list 0 1 2 1022 1023 1024 1025 1026 2047 2048 2049 3071 3072 3073 10000 65537))

(define numbers (mutable-vector))
(define (fill n)
  (when (> n 0)
    (vector-push! numbers n)
    (fill (- n 1))))
(fill 5000)
(define (again n)
  (when (> n 0)
    (assert! (= (through-a-vector numbers 0 0) (triangle 5001)))
    (assert! (= (below-a-constant 0 0) (triangle 5000)))
    (again (- n 1))))
(again 200)

;; A count that runs into the end of the fixnums carries on past it.
(define near-the-end 4611686018427387000)
(assert! (= (up-by-three near-the-end (+ near-the-end 30000) 0) 10000))

;; Threads counting toward bounds they will not reach while the test runs.
;; A collection stops each of them, and so does an interrupt.
(define far 1000000000000000)
(define (raised? thread)
  (eq? 'raised (with-handler (lambda (e) 'raised) (thread-join! thread))))
(define counting
  (list (spawn-native-thread (lambda () (up-below 0 far 0)))
        (spawn-native-thread (lambda () (up-until-equal 0 far 0)))
        (spawn-native-thread (lambda () (down-above 0 (- far) 0)))
        (spawn-native-thread (lambda () (up-by-three 0 far 0)))
        (spawn-native-thread (lambda () (down-until-equal -1 0)))))
(time/sleep-ms 20)
(#%gc-collect)
(#%gc-collect)
(for-each thread-interrupt counting)
(#%gc-collect)
(for-each (lambda (thread) (assert! (raised? thread))) counting)
