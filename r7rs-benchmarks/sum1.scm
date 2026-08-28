(require "common.scm")

;;; SUM1 -- One of the Kernighan and Van Wyk benchmarks.

(define (sumport port sum-so-far)
  (let ((x (read port)))
    (if (eof-object? x)
        sum-so-far
        (sumport port (+ x sum-so-far)))))

(define (sum port)
  (sumport port 0.0))

(define (go input)
  (call-with-input-file input sum))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "sum1"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (<= (abs (- result output)) 1e-9)))))

(with-input-from-file (bench-input "sum1") run-benchmark)
