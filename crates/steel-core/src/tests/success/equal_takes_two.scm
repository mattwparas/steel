;; `equal?` compares two values. With any other number it is an error, whether
;; it is called by name, applied to a list, or passed around as a value.
(define (raised? thunk)
  (eq? 'raised (with-handler (lambda (e) 'raised) (thunk))))

;; Written out with the wrong number.
(assert! (raised? (lambda () (equal? 1))))
(assert! (raised? (lambda () (equal? 1 1 1))))
(assert! (raised? (lambda () (equal? 1 1 1 1 1))))
(assert! (equal? 1 1))
(assert! (not (equal? 1 2)))

;; Applied, it is caught when it is called.
(assert! (raised? (lambda () (apply equal? '()))))
(assert! (raised? (lambda () (apply equal? (list 1)))))
(assert! (raised? (lambda () (apply equal? (list 1 1 1)))))
(assert! (apply equal? (list 1 1)))
(assert! (not (apply equal? (list 1 2))))

;; As a value.
(define (compare-with f . values) (apply f values))
(assert! (raised? (lambda () (compare-with equal? 1))))
(assert! (raised? (lambda () (compare-with equal? 1 1 1))))
(assert! (compare-with equal? (list 1 "a") (list 1 "a")))

;; Two is still two, in a loop that runs often enough to be compiled.
(define (count-equal a b n total)
  (if (= n 0)
      total
      (count-equal a b (- n 1) (if (equal? a b) (+ total 1) total))))
(assert! (= (count-equal (list 1 2) (list 1 2) 3000 0) 3000))
(assert! (= (count-equal (list 1 2) (list 1 3) 3000 0) 0))
(assert! (= (count-equal "s" "s" 3000 0) 3000))

;; And a call with the wrong number, reached often, is refused every time.
(define (refused-each-time n ok)
  (if (= n 0)
      ok
      (refused-each-time (- n 1) (and ok (raised? (lambda () (apply equal? (list n))))))))
(assert! (refused-each-time 2000 #t))
