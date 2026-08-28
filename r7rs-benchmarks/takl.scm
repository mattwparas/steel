(require "common.scm")

;;; TAKL -- The TAKeuchi function using lists as counters.

(define (listn n)
  (if (= n 0)
      '()
      (cons n (listn (- n 1)))))

(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (output (read))
         (s4 (number->string count))
         (s3 (number->string (length input3)))
         (s2 (number->string (length input2)))
         (s1 (number->string (length input1)))
         (name "takl"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (mas (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? (length result) output)))))

(with-input-from-file (bench-input "takl") run-benchmark)
