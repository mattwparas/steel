(require "common.scm")

;; TODO: This takes _way_ too long. Most likely need to optimize how call/cc works internally.

(define (ctak x y z)
  (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (#%prim.call-with-current-continuation
       (lambda (k)
         (ctak-aux k
                   (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k (- x 1) y z)))
                   (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k (- y 1) z x)))
                   (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k (- z 1) x y))))))))

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
         [name "ctak"])
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda () (ctak (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? result output)))))

(with-input-from-file (bench-input "ctak") run-benchmark)
