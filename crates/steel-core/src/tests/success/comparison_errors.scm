(define (raises? thunk)
  (with-handler (lambda (e) #true) (thunk) #false))

(assert! (raises? (lambda () (< "a" 1))))
(assert! (raises? (lambda () (<= 1 'a))))
(assert! (raises? (lambda () (> (list 1) 2))))
(assert! (raises? (lambda () (>= 2 #\a))))

(assert! (raises? (lambda () (< 3 2 "x"))))

(assert! (< 1 2 3))
(assert! (not (< 1 3 2)))
(assert! (<= 1 1 2))
(assert! (> 3 2.5 1/2))
(assert! (>= 2 2 1))
