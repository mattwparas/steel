(require "common.scm")

(define (run n)
  (let loop ([i n]
             [sum 0.])
    (if (< i 0.)
        sum
        (loop (- i 1.) (+ i sum)))))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "sumfp"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (run (hide count input1)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/sumfp.input" run-benchmark)

; (define (run-benchmark)
;   (let* ([count 10]
;          [input1 1e6]
;          [output 5.000005e11]
;          [s2 (number->string count)]
;          [s1 (number->string input1)]
;          [name "sumfp"])
;     (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
;                         count
;                         (lambda () (run (hide count input1)))
;                         (lambda (result) (equal? result output)))))

; (run-benchmark)
