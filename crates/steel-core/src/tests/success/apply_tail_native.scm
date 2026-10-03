;; `apply` in tail position on a compiled closure tail calls into it instead
;; of leaving compiled code. Run hot so the jitted versions are checked.
(define (add3 a b c)
  (+ a b c))
(define (sum-all . xs)
  (if (null? xs) 0 (+ (car xs) (apply sum-all (cdr xs)))))

(define (via-apply f args)
  (apply f args))

(define (make-adder n)
  (lambda (x y) (+ x y n)))
(define add10 (make-adder 10))

(define (run i)
  (list (via-apply add3 (list i 1 2))
        (via-apply sum-all (list 1 2 3 i))
        (via-apply add10 (list i i))
        (via-apply + (list i 5))
        (via-apply list (list i 'x))))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1) (run n))))
(assert! (equal? (loop 3000 '()) '(4 7 12 6 (1 x))))

;; A long chain of tail applies runs in constant stack.
(define (count-down n)
  (if (= n 0)
      'done
      (apply count-down (list (- n 1)))))
(assert! (equal? (count-down 200000) 'done))

;; Two functions that tail apply each other.
(define (ping n)
  (if (= n 0) 'ping (apply pong (list (- n 1)))))
(define (pong n)
  (if (= n 0) 'pong (apply ping (list (- n 1)))))
(assert! (equal? (ping 100001) 'pong))

;; A callee that raises.
(define (fail x)
  (error "failed" x))
(define (try-apply x)
  (with-handler (lambda (e) 'raised) (via-apply fail (list x))))
(define (loop-fail n acc)
  (if (= n 0)
      acc
      (loop-fail (- n 1) (try-apply n))))
(assert! (equal? (loop-fail 2000 #f) 'raised))

;; The wrong number of arguments still raises.
(define (try-arity)
  (with-handler (lambda (e) 'arity) (via-apply add3 (list 1 2))))
(define (loop-arity n acc)
  (if (= n 0)
      acc
      (loop-arity (- n 1) (try-arity))))
(assert! (equal? (loop-arity 2000 #f) 'arity))
