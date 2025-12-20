(require "common.scm")

;; TODO: Look into loop unrolling?
;; Otherwise, we'll want to look into
;; better type inference / understanding
;; how to avoid the dispatching cost.
(define (run n)
  (let loop ([i n]
             [sum 0])
    (if (< i 0)
        sum
        (loop (- i 1) (+ i sum)))))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "sum"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (run (hide count input1)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/sum.input" run-benchmark)
