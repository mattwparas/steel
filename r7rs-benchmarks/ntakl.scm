;;; NTAKL -- The TAKeuchi function using lists as counters,
;;; with an alternative boolean expression.

(require "common.scm")

(define (listn n)
  (if (= n 0)
      '()
      (cons n (listn (- n 1)))))

(define l18 (listn 18))
(define l12 (listn 12))
(define l6 (listn 6))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z) (mas (cdr y) z x) (mas (cdr z) x y))))

;; Part of the fun of this benchmark is seeing how well the compiler
;; can understand this ridiculous code, which dates back to the original
;; Common Lisp.  So it probably isn't a good idea to improve upon it.

;; (define (shorterp x y)
;;   (and (not (null? y))
;;        (or (null? x)
;;            (shorterp (cdr x)
;;                      (cdr y)))))

;; But SML/NJ runs this benchmark about 15 times as fast when the
;; code above is rewritten as follows, so I tried it for Scheme also.

(define (shorterp x y)
  (cond
    [(null? y) #f]
    [(null? x) #t]
    [else (shorterp (cdr x) (cdr y))]))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [input2 (read)]
         [input3 (read)]
         [output (read)]
         [s4 (number->string count)]
         [s3 (number->string (length input3))]
         [s2 (number->string (length input2))]
         [s1 (number->string (length input1))]
         [name "ntakl"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
                        count
                        (lambda () (mas (hide count input1) (hide count input2) (hide count input3)))
                        (lambda (result) (equal? (length result) output)))))

(with-input-from-file "r7rs-benchmarks/inputs/ntakl.input" run-benchmark)
