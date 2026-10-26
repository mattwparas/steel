;; An exact number against a float compares exactly
(assert! (equal? (< (expt 10 30) +inf.0) #true))
(assert! (equal? (> (expt 10 30) -inf.0) #true))
(assert! (equal? (= (+ (expt 10 30) 1) 1e30) #false))
(assert! (equal? (= 1/3 (/ 1.0 3)) #false))
(assert! (equal? (< 9007199254740992.0 9007199254740993) #true))
(assert! (equal? (= 1/2 0.5) #true))
(assert! (equal? (<= 3/2 1.5) #true))

;; The same compiled
(define (lt a b) (< a b))
(define (loop i acc)
  (if (= i 0) acc (loop (- i 1) (list (lt 9007199254740993 9007199254740992.0) (lt 1 2.5)))))
(assert! (equal? (loop 5000 '()) '(#false #true)))

;; A closure called with the wrong number of arguments raises, compiled or not
(define (two x y) (+ x y))
(define (hot g i acc) (if (= i 0) acc (hot g (- i 1) (+ (g i) acc))))
(assert! (equal? (with-handler (lambda (e) 'arity) (hot two 5000 0)) 'arity))
