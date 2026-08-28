(require "common.scm")

;;; FIBC -- FIB using first-class continuations, written by Kent Dybvig

(define (succ n) (+ n 1))
(define (pred n) (- n 1))

;;; fib with peano arithmetic (using numbers) with call/cc

(define (addc x y k)
  (if (zero? y)
      (k x)
      (addc (succ x) (pred y) k)))

(define (fibc x c)
  (if (zero? x)
      (c 0)
      (if (zero? (pred x))
          (c 1)
          (addc (call-with-current-continuation
                 (lambda (c) (fibc (pred x) c)))
                (call-with-current-continuation
                 (lambda (c) (fibc (pred (pred x)) c)))
                c))))

(define (run-benchmark)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fibc"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fibc (hide count input) (hide count (lambda (n) n))))
     (lambda (result) (= result output)))))

(with-input-from-file (bench-input "fibc") run-benchmark)
