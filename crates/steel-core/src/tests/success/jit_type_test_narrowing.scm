;; Where a type test like `fixnum?` holds, the jit knows the type of the tested
;; variable. That knowledge has to end where the arm does, and at a `set!`.

(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

(define (classify pair)
  (let ([a (car pair)]
        [b (cdr pair)])
    (if (fixnum? a)
        (if (fixnum? b)
            (+ a b)
            'not-fixnum)
        (if (float? a)
            (+ a 1.0)
            (if (char? a)
                (char->integer a)
                (if (boolean? a)
                    (not a)
                    'other))))))

(define (reassigned x)
  (let ([a (car x)])
    (if (fixnum? a)
        (begin
          (set! a "changed")
          (string-length a))
        a)))

(define (classify-all n acc)
  (if (= n 0)
      acc
      (classify-all (- n 1)
                    (list (classify (cons 1 2))
                          (classify (cons 1.5 2))
                          (classify (cons #\a 2))
                          (classify (cons #f 2))
                          (classify (cons 'q 2))
                          (classify (cons 1 'z))
                          (classify (cons 4611686018427387904 4611686018427387904))
                          (reassigned (list 5))
                          (reassigned (list "s"))))))

(assert-equal! (list 3 2.5 97 #t 'other 'not-fixnum 9223372036854775808 7 "s")
               (classify-all 20000 '()))

;; A scope that ends after a failed type test still releases what it bound
(define (holds-until-scope-ends value)
  (let ([a value])
    (if (fixnum? a) 'fixnum 'heap)))

(define strong (list 1 2 3))
(define weak (make-weak-box strong))

(define (release-all n)
  (when (> n 0)
    (holds-until-scope-ends (weak-box-value weak))
    (release-all (- n 1))))

(release-all 20000)
(assert-equal! '(1 2 3) (weak-box-value weak))
;; `set!` returns the old value, which a top level form would keep
(begin
  (set! strong #f)
  #t)
(assert-equal! #f (weak-box-value weak))
