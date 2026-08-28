(require "common.scm")

;;; FIBFP -- Computes fib(35) using floating point

(define (fibfp n)
  (if (< n 2.)
      n
      (+ (fibfp (- n 1.))
         (fibfp (- n 2.)))))

(define (run-benchmark)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fibfp"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fibfp (hide count input)))
     (lambda (result) (= result output)))))

(with-input-from-file (bench-input "fibfp") run-benchmark)
