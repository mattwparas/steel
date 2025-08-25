;;; PRIMES -- Compute primes less than n, written by Eric Mohr.

(require "common.scm")

(define (interval-list m n)
  (if (> m n)
      '()
      (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ([remove-multiples (lambda (n l)
                               (if (null? l)
                                   '()
                                   (if (= (remainder (car l) n) 0)
                                       (remove-multiples n (cdr l))
                                       (cons (car l) (remove-multiples n (cdr l))))))])
    (if (null? l)
        '()
        (cons (car l) (sieve (remove-multiples (car l) (cdr l)))))))

(define (primes<= n)
  (sieve (interval-list 2 n)))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "primes"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (primes<= (hide count input1)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/primes.input" run-benchmark)
