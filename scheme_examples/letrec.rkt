; (let ([f void])
;   (let ([ft (let ([g void])
;               (let ([gt (let ([x 5])
;                               (lambda () ...))])
;                     (set! g gt))
;                   (lambda () ... g ...))])
;   (set! f ft))
;   f)


;; Just turn the whole thing into a let-rec?
(define (blah)
    (define x 10)
    (displayln "hello world")
    (define (foo) (bar))
    (define (bar) (foo))
    (foo))

(define (test)
  (let ((x 10) (y 0) (foo void) (bar void))
        (let ((foo-prime (lambda (x) (if (= x 10000) x (bar (+ x 1)))))
              (bar-prime (lambda (x) (if (= x 10000) x (foo (+ x 1))))))
              (set! foo foo-prime)
              (set! bar bar-prime)
              (set! y x))
        (foo 0)))

(define (test-loop)
  (let ((loop void))
    (let ((loop-prime (lambda (x) 
                            (if (= x 10000)
                                x
                                (loop (+ x 1))))))
      (set! loop loop-prime))
    (loop 0)))


(define (test-loop-2)
  (define (loop x)
    (if (= x 10000)
        x
        (loop (+ x 1))))
  (loop 0))


(define (thing)
  (define blagh 10)
  (displayln 15)
  (define blegh 20)
  (displayln 25))



;; TODO this transformation is how I should implement defines
;; Do compile time transformation of AST -> this form

; (let ((is-even? void) (is-odd? void))
;   (let ((is-even?-prime 
;           (lambda (n) (or (zero? n) 
;                           (is-odd? (sub1 n)))))
        
;         (is-odd?-prime
;           (lambda (n) (and (not (zero? n))
;                            (is-even? (sub1 n))))))

;         (set! is-even? is-even?-prime)
;         (set! is-odd? is-odd?-prime))
;   (is-odd? 12))