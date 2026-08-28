(require "common.scm")

(define (cpstak x y z)

  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1) z x (lambda (v2) (tak (- z 1) x y (lambda (v3) (tak v1 v2 v3 k)))))))))

  (tak x y z (lambda (a) a)))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [input2 (read)]
         [input3 (read)]
         [output (read)]
         [s4 (number->string count)]
         [s3 (number->string input3)]
         [s2 (number->string input2)]
         [s1 (number->string input1)]
         [name "cpstak"])
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda () (cpstak (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? result output)))))

(with-input-from-file (bench-input "cpstak") run-benchmark)
