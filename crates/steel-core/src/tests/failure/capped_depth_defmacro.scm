;; Should stop before we get a stack overflow
(defmacro (foo x) x)

(foo (define bar 10))
