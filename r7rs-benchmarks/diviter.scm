(require "common.scm")

;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))

(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (ll (create-n (hide count input1)))
         (name "diviter"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (iterative-div2 ll))
     (lambda (result) (equal? (length result) output)))))

(with-input-from-file (bench-input "diviter") run-benchmark)
