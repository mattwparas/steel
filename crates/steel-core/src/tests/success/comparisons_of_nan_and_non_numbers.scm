;; NaN orders against nothing, which is false, the same compiled or not
(define (le a) (<= a +nan.0))
(define (lt a b) (< a b))
(define (loop i acc)
  (if (= i 0) acc (loop (- i 1) (list (le 1) (lt +inf.0 (expt 10 30)) (lt (expt 10 30) +nan.0)))))
(assert! (equal? (loop 5000 '()) '(#false #false #false)))

;; One value is checked too
(define (message thunk)
  (with-handler (lambda (e) 'err) (thunk)))
(assert! (equal? (message (lambda () (< 'a))) 'err))
(assert! (equal? (< 1) #true))
(assert! (equal? (< +nan.0) #true))
