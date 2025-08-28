;;; STRING -- One of the Kernighan and Van Wyk benchmarks.

(require "common.scm")

(define s "abcdef")

(define (grow)
  (set! s (string-append "123" s "456" s "789"))
  (set! s
        (string-append (substring s (quotient (string-length s) 2) (string-length s))
                       (substring s 0 (+ 1 (quotient (string-length s) 2)))))
  s)

(define (trial n)
  (do ((i 0 (+ i 1))) ((> (string-length s) n) (string-length s)) (grow)))

(define (my-try n)
  (do ((i 0 (+ i 1))) ((>= i 10) (string-length s)) (set! s "abcdef") (trial n)))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "string"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (my-try (hide count input1)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/small-inputs/string.input" run-benchmark)
