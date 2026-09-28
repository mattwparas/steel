;; Self recursive functions whose jitted self calls push no frame (see
;; `frameless_self_call` in jit2/cgen.rs). Frames are only rebuilt when something
;; leaves native code, so every test here forces one of those exits at depth.

(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

;; Fixnum overflow deep inside the recursion: the innermost call deopts to the
;; interpreter with every frameless caller above it still pending.
(define (grow n x)
  (if (= n 0) x (+ x (grow (- n 1) x))))
(assert-equal! 235195986939796737000 (grow 50 4611686018427387000))
(assert-equal! 3003 (grow 1000 3))

;; Deep enough to run out of native stack: the call that doesn't fit hands off
;; to the interpreter, and ~100k frames have to be rebuilt in order.
(define (depth n)
  (if (= n 0) 0 (+ 1 (depth (- n 1)))))
(assert-equal! 200000 (depth 200000))

;; Two self calls per level, and the let bindings ack's unrolled body has.
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (ack m n)
  (cond
    [(= m 0) (+ n 1)]
    [(= n 0) (ack (- m 1) 1)]
    [else (ack (- m 1) (ack m (- n 1)))]))
(assert-equal! 75025 (fib 25))
(assert-equal! 21 (ack 2 9))
(assert-equal! 253 (ack 3 5))

;; Overflow at a different depth on every call, so deopts come from many
;; nesting levels over the run.
(define (tri n x)
  (if (= n 0) x (+ (tri (- n 1) x) n)))
(define (sweep k acc)
  (if (= k 0) acc (sweep (- k 1) (+ acc (tri 30 (- 4611686018427387903 (* k 3)))))))
(assert-equal! 922337203685477613300 (sweep 200 0))

;; Entered over and over from outside, so the frame count native code keeps
;; for the interpreter has to come out right every time.
(define (many k acc)
  (if (= k 0) acc (many (- k 1) (+ acc (fib 15) (ack 2 3)))))
(assert-equal! 1857000 (many 3000 0))

;; A deopt under a handler.
(assert-equal! 50728546202701266900 (with-handler (lambda (e) 'caught) (grow 10 4611686018427387900)))

;; Float seeded: `(- n 1.)` on a Float seeded argument has to type as a float,
;; or the self calls never match the seed and nothing goes frameless.
(define (fibfp n)
  (if (< n 2.) n (+ (fibfp (- n 1.)) (fibfp (- n 2.)))))
(assert-equal! 6765. (fibfp 20.))

;; Deep float recursion, through the native stack handoff, with float operands
;; pending in every caller.
(define (fdepth n)
  (if (< n 1.) 0. (+ 1.5 (fdepth (- n 1.)))))
(assert-equal! 300000. (fdepth 200000.))

;; An exact argument takes the generic copy, which must not assume floats. The
;; float constants still make the answer a float, same as the interpreter.
(assert-equal! 6765. (fibfp 20))
