;; A function that ends by calling a builtin which calls back in, such as
;; `transduce`. Once it and the callback are both compiled, the callback's
;; return must not make the function's own result go missing. The function
;; is called every way it can be: outright, as a callback itself, and as the
;; body of a thread.

(define (identity-of x)
  x)
(define (doubled items)
  (transduce items (mapping (lambda (x) (* 2 x))) (into-list)))
(define (through-a-named-callback items)
  (transduce items (mapping identity-of) (into-list)))
(define (doubled-twice items)
  (transduce (doubled items) (mapping (lambda (x) (* 2 x))) (into-list)))

(define (check)
  (assert! (equal? (doubled (list 1 2 3)) '(2 4 6)))
  (assert! (equal? (through-a-named-callback (list 1 2 3)) '(1 2 3)))
  (assert! (equal? (doubled-twice (list 1 2 3)) '(4 8 12)))
  ;; As a callback, in tail position there too.
  (assert! (equal? (transduce (list (list 1) (list 2 3)) (mapping (lambda (items) (doubled items))) (into-list))
                   '((2) (4 6))))
  (assert! (equal? (transduce (list (list 1) (list 2 3)) (mapping doubled) (into-list)) '((2) (4 6))))
  (assert! (equal? (transduce (list (list 1) (list 2 3)) (mapping doubled-twice) (into-list))
                   '((4) (8 12)))))

;; Enough times for everything to be compiled, then again.
(define (again n)
  (when (> n 0)
    (check)
    (again (- n 1))))
(again 500)
(check)

;; As the body of a thread, where it is the outermost function.
(define (in-a-thread thunk)
  (thread-join! (spawn-native-thread thunk)))
(define (in-threads n)
  (when (> n 0)
    (assert! (equal? (in-a-thread (lambda () (doubled (list 1 2 3)))) '(2 4 6)))
    (assert! (equal? (in-a-thread (lambda () (transduce (list 1 2 3) (mapping identity-of) (into-list))))
                     '(1 2 3)))
    (in-threads (- n 1))))
(in-threads 30)
(define (three)
  (doubled (list 1 2 3)))
(assert! (equal? (in-a-thread three) '(2 4 6)))
