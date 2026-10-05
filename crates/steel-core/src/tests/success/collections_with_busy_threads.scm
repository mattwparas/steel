;; Threads in every shape of loop, none of which stops by itself, while this
;; thread collects and defines globals, both of which stop all the others.
(define stop (box #f))
(define cell (box 0))
(define numbers (mutable-vector 1 2 3 4 5 6 7 8))

;; Calls nothing.
(define (tail-loop n)
  (if (unbox stop) n (tail-loop (+ n 1))))

;; A cycle of tail calls.
(define (ping n)
  (if (unbox stop) n (pong (+ n 1))))
(define (pong n)
  (ping (+ n 1)))

;; Recursion, over and over.
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (recursing rounds)
  (if (unbox stop)
      rounds
      (begin
        (fib 15)
        (recursing (+ rounds 1)))))

;; Reads and writes cells.
(define (cells n)
  (if (unbox stop)
      n
      (begin
        (set-box! cell n)
        (vector-set! numbers (modulo n 8) (vector-ref numbers (modulo (+ n 1) 8)))
        (cells (+ n 1)))))

;; A loop inside a closure, around a loop of its own.
(define (nested)
  (let outer ([rounds 0])
    (if (unbox stop)
        rounds
        (begin
          (let inner ([i 0] [sum 0])
            (when (< i 100)
              (inner (+ i 1) (+ sum i))))
          (outer (+ rounds 1))))))

;; Allocates and calls primitives that aren't compiled in line.
(define (allocating n)
  (if (unbox stop)
      n
      (begin
        (string-append (number->string n) "!")
        (allocating (+ n 1)))))

(define threads
  (list (spawn-native-thread (lambda () (tail-loop 0)))
        (spawn-native-thread (lambda () (ping 0)))
        (spawn-native-thread (lambda () (recursing 0)))
        (spawn-native-thread (lambda () (cells 0)))
        (spawn-native-thread (lambda () (nested)))
        (spawn-native-thread (lambda () (allocating 0)))))

(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (box n)
    (collect (- n 1))))

(collect 40)
(define first-global (unbox cell))
(collect 40)
(define second-global (list first-global))
(collect 40)
(define third-global (mutable-vector second-global))
(collect 40)

(set-box! stop #t)
(for-each (lambda (thread) (assert! (> (thread-join! thread) 0))) threads)
