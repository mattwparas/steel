;; Loops and self-recursive functions whose argument slots have a guessable
;; type get a second compiled copy with those slots typed (on by default,
;; STEEL_JIT_SPECIALIZE_LOOPS=0 turns it off). The generic copy moves into it
;; once the slots hold those types, and it moves back out whenever one might not. Every transition has to leave the loop exactly
;; where the interpreter would have it.

(define big 9223372036854775807)

;; the plain case
(define (sum-to n acc) (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
(assert! (equal? (sum-to 1000 0) 500500))

;; entered with a float: the guard in the generic copy never passes
(assert! (equal? (sum-to 4 0.5) 10.5))

;; the accumulator stops being a fixnum part way: the specialized copy has to
;; hand back to the generic one
(define (turns-float n acc) (if (= n 0) acc (turns-float (- n 1) (if (= n 5) (+ acc 0.5) (+ acc 1)))))
(assert! (equal? (turns-float 10 0) 9.5))

;; alternates every iteration, so control bounces between the copies
(define (bounce n acc) (if (= n 0) acc (bounce (- n 1) (if (even? n) (+ acc 1) (+ acc 1.0)))))
(assert! (equal? (bounce 10 0) 10.0))

;; the accumulator overflows into a bignum while specialized
(define (doubling n acc) (if (= n 0) acc (doubling (- n 1) (* acc 2))))
(assert! (equal? (doubling 70 1) 1180591620717411303424))

(define (counting-up n acc) (if (= n 0) acc (counting-up (- n 1) (+ acc 1))))
(assert! (equal? (counting-up 3 (- big 1)) 9223372036854775809))

;; a fixnum index next to a vector that never changes
(define (fill! v i n) (if (>= i n) v (begin (vector-set! v i (* i i)) (fill! v (+ i 1) n))))
(assert! (equal? (fill! (make-vector 5 0) 0 5) (vector 0 1 4 9 16)))

(define (vector-sum v i acc) (if (< i 0) acc (vector-sum v (- i 1) (+ acc (vector-ref v i)))))
(assert! (equal? (vector-sum (vector 1 2 3 4) 3 0) 10))
;; a non-fixnum element reaching the accumulator
(assert! (equal? (vector-sum (vector 1 2.5 3 4) 3 0) 10.5))

;; the arguments swap places
(define (swap-down a b steps) (if (= steps 0) (list a b) (swap-down (- b 1) a (- steps 1))))
(assert! (equal? (swap-down 10 20 3) '(18 9)))
(define (swap-steps a b steps) (if (= steps 0) (list a b) (swap-steps b (- a 1) (- steps 1))))
(assert! (equal? (swap-steps 10 20 4) '(8 18)))

;; an argument mutated by set! is never assumed to be a fixnum
(define (mutates n acc)
  (if (= n 0)
      acc
      (begin
        (set! acc (if (= n 1) "done" (+ acc 1)))
        (mutates (- n 1) acc))))
(assert! (equal? (mutates 3 0) "done"))

;; nested loops, both specialized
(define (inner j acc) (if (= j 0) acc (inner (- j 1) (+ acc j))))
(define (outer i acc) (if (= i 0) acc (outer (- i 1) (+ acc (inner i 0)))))
(assert! (equal? (outer 10 0) 220))

;; a seeded slot receiving a string
(define (maybe-string n x) (if (= n 0) x (maybe-string (- n 1) (if (= n 1) "s" (+ x 1)))))
(assert! (equal? (maybe-string 5 0) "s"))

;; an error raised while specialized is still caught
(define (bad-index v i) (if (= i 10) 'never (bad-index v (+ i (vector-ref v i)))))
(assert! (equal? (with-handler (lambda (e) 'caught) (bad-index (vector 1 1 1) 0)) 'caught))
(define (adds-string n acc) (if (= n 0) acc (adds-string (- n 1) (+ acc (if (= n 3) "s" 1)))))
(assert! (equal? (with-handler (lambda (e) 'caught) (adds-string 5 0)) 'caught))

;; results of different types leaving the loop
(define (classify n) (if (= n 0) 'zero (if (= n 1) "one" (classify (- n 2)))))
(assert! (equal? (classify 10) 'zero))
(assert! (equal? (classify 11) "one"))

;; a closure argument riding along, the quicksort shape
(define (find-first less? v i n) (if (>= i n) #f (if (less? (vector-ref v i) 3) i (find-first less? v (+ i 1) n))))
(assert! (equal? (find-first < (vector 5 4 2 1) 0 4) 2))
(assert! (equal? (find-first < (vector 5 4 4 4) 0 4) #f))

;; the generic copy decides on entry, per call: the same loop called with floats
;; and then with fixnums takes a different copy each time
(define (count-down n acc) (if (<= n 0) acc (count-down (- n 1) (+ acc 1))))
(assert! (equal? (count-down 3.5 0) 4))
(assert! (equal? (count-down 3 0) 3))
(assert! (equal? (count-down 3 0.0) 3.0))
(assert! (equal? (count-down 4 0) 4))

;; float literals mark a slot as carrying floats, and that spreads to the slots
;; it is combined with
(define (float-loop i sum) (if (< i 0.) sum (float-loop (- i 1.) (+ i sum))))
(assert! (equal? (float-loop 3. 0.) 6.))

;; specialized for a fixnum counter while an untouched float rides along
(define (scale n x acc) (if (= n 0) acc (scale (- n 1) x (+ acc x))))
(assert! (equal? (scale 4 0.5 0.) 2.))

;; ---- recursion that is not a loop ---------------------------------------------------
;; A recursive function gets a specialized copy too. Its self calls whose
;; arguments are proven to fit go straight to that copy, and their results are
;; assumed to have the function's own return type when every return has it.
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(assert! (equal? (fib 20) 6765))
;; entered with a float: the generic copy runs it
(assert! (equal? (fib 5.0) 5.0))

(define (tak x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))
(assert! (equal? (tak 18 12 6) 7))

(define (ack m n) (cond [(= m 0) (+ n 1)] [(= n 0) (ack (- m 1) 1)] [else (ack (- m 1) (ack m (- n 1)))]))
(assert! (equal? (ack 2 3) 9))

;; overflow deep in the recursion: every specialized frame above it has to
;; notice and hand over
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(assert! (equal? (fact 25) 15511210043330985984000000))
(define (sum-down n) (if (= n 0) 0 (+ 4611686018427387904 (sum-down (- n 1)))))
(assert! (equal? (sum-down 3) 13835058055282163712))
;; the same after warming up, so the overflow happens in a compiled frame, with
;; the constant operand spilled under the call: the exit has to write it back
(sum-down 1)
(sum-down 1)
(assert! (equal? (list (sum-down 2) (sum-down 3) (sum-down 4))
                 '(9223372036854775808 13835058055282163712 18446744073709551616)))
(define (sum-down-from-big n)
  (if (= n 0) 4611686018427387904 (+ 4611686018427387904 (sum-down-from-big (- n 1)))))
(sum-down-from-big 0)
(sum-down-from-big 0)
(assert! (equal? (list (sum-down-from-big 1) (sum-down-from-big 2))
                 '(9223372036854775808 13835058055282163712)))
;; and a register pending under the call rather than a constant
(define (sum-n-down n) (if (= n 0) 0 (+ n (sum-n-down (- n 1)))))
(sum-n-down 1)
(sum-n-down 1)
(assert! (equal? (sum-n-down 1000) 500500))

;; not every return is a fixnum, so results cannot be assumed to be
(define (maybe-count n) (if (= n 0) #f (let ([r (maybe-count (- n 1))]) (if r (+ r 1) 1))))
(assert! (equal? (maybe-count 5) 5))
(define (mixed-return n) (if (= n 0) 0.5 (+ 1 (mixed-return (- n 1)))))
(assert! (equal? (mixed-return 3) 3.5))

;; a float shows up part way down
(define (half-down n) (if (< n 1) n (+ 1 (half-down (if (= n 4) 3.5 (- n 1))))))
(assert! (equal? (half-down 6) 6.5))

;; deep enough to go through the trampoline instead of a native call
(define (depth n) (if (= n 0) 0 (+ 1 (depth (- n 1)))))
(assert! (equal? (depth 100000) 100000))

;; an error raised in the middle of the recursion is still caught
(define (fails-at n) (if (= n 0) (car '()) (+ 1 (fails-at (- n 1)))))
(assert! (equal? (with-handler (lambda (e) 'caught) (fails-at 10)) 'caught))
