;;; MBROT -- Generation of Mandelbrot set fractal.

(require "common.scm")

(define (count r i step x y)

  (let ([max-count 64]
        [radius^2 16.0])

    (let ([cr (+ r (* (inexact x) step))]
          [ci (+ i (* (inexact y) step))])

      (let loop ([zr cr]
                 [zi ci]
                 [c 0])
        (if (= c max-count)
            c
            (let ([zr^2 (* zr zr)]
                  [zi^2 (* zi zi)])
              (if (> (+ zr^2 zi^2) radius^2)
                  c
                  (let ([new-zr (+ (- zr^2 zi^2) cr)]
                        [new-zi (+ (* 2.0 (* zr zi)) ci)])
                    (loop new-zr new-zi (+ c 1))))))))))

(define (mbrot matrix r i step n)
  (let loop1 ([y (- n 1)])
    (when (>= y 0)
      (let loop2 ([x (- n 1)])
        (if (>= x 0)
            (begin
              (vector-set! (vector-ref matrix x) y (count r i step x y))
              (loop2 (- x 1)))
            (loop1 (- y 1)))))))

(define (test n)
  (let ([matrix (make-vector n)])
    (let loop ([i (- n 1)])
      (when (>= i 0)
        (vector-set! matrix i (make-vector n))
        (loop (- i 1))))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "mbrot"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (test (hide count input1)))
                        (lambda (result) (= result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/mbrot.input" run-benchmark)
