(define (any? x) #t)


(define any/c (make-flat/c any? 'any/c))
(define even/c (make-flat/c even? 'even/c))
(define odd/c (make-flat/c odd? 'odd/c))
(define string/c (make-flat/c string? 'string/c))


(define test/c
  (make-function/c even/c odd/c string/c))

(define (test-function x y)
  (+ x y)
  "hello world")

(define contracted-function
  (bind/c test/c test-function))


(contracted-function 2 1) ;; should not throw a contract violation
;; (contracted-function 1 2) ;; should throw a contract violation








(define even->even/c (make-function/c even/c even/c))


(define map/c (make-function/c even->even/c odd/c))

(define (fake-map func)
  (+ (func 2) 1))

(define contracted-fake-map (bind/c map/c fake-map))

(define (good-test-func x) (* x 2))
(define (bad-test-func x) (+ x 1))

(define bad-test-func-contract
  (bind/c (make-function/c even/c odd/c) bad-test-func))


(define (range-test) good-test-func)
(define range-contract (make-function/c even->even/c))
(define contracted-range-test
  (bind/c range-contract range-test))


(define/contract (test arg1 arg2 arg3)
  (-> int/c ))
