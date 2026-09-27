;; Calls through a function value with more arguments than the per arity call
;; helpers take. The arguments go through the vm stack instead, in and out of
;; tail position. `#%black-box` keeps the inliner from resolving the callee.

(define (many . xs) xs)

(define (sum16 a b c d e f g h i j k l m n o p)
  (+ a b c d e f g h i j k l m n o p))

(define (count-down16 a b c d e f g h i j k l m n o p)
  (if (> a 0)
      (count-down16 (- a 1) b c d e f g h i j k l m n o p)
      (list a p)))

;; Tail call through the parameter.
(define (tail16 f x)
  (f x x x x x x x x x x x x x x x (+ x 1)))

;; Not in tail position, with bindings after the call that depend on where the
;; model thinks the stack ends.
(define (call16 f x)
  (let ([y (* x 3)])
    (let ([r (f x x x x x x x x x x x x x x x (+ x 1))])
      (let ([z (list r y)])
        (list y r z x)))))

;; The function is computed, not read from a register.
(define (pick16 flag f g x)
  ((if flag f g) x x x x x x x x x x x x x x x (+ x 1)))

(define f-many (#%black-box many))
(define f-sum (#%black-box sum16))
(define f-count (#%black-box count-down16))
(define f-apply (#%black-box apply))

(define (warm n)
  (when (> n 0)
    (tail16 f-many 1)
    (tail16 f-sum 1)
    (call16 f-many 1)
    (call16 f-sum 1)
    (pick16 #t f-many f-sum 1)
    (pick16 #f f-many f-sum 1)
    (warm (- n 1))))

(warm 500)

(assert! (equal? (length (tail16 f-many 2)) 16))
(assert! (equal? (tail16 f-sum 2) 33))
(assert! (equal? (tail16 f-count 3) '(0 4)))
(assert! (equal? (+ 1 (tail16 f-sum 2)) 34))

(assert! (equal? (call16 f-sum 2) '(6 33 (33 6) 2)))
(assert! (equal? (cadr (call16 f-many 2)) '(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3)))
(assert! (equal? (call16 f-count 3) '(9 (0 4) ((0 4) 9) 3)))

(assert! (equal? (pick16 #t f-many f-sum 1) '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2)))
(assert! (equal? (pick16 #f f-many f-sum 1) 17))

;; `apply` replaces the calling frame in tail position.
(define (tail-apply16 f x)
  (f + x x x x x x x x x x x x x x (list x)))
(tail-apply16 f-apply 1)
(assert! (equal? (tail-apply16 f-apply 2) 30))
(assert! (equal? (+ 1 (tail-apply16 f-apply 2)) 31))

;; Errors raise from the call, in and out of tail position.
(define (message-of thunk)
  (with-handler (lambda (e) 'caught) (thunk)))

(assert! (equal? (message-of (lambda () (tail16 10 1))) 'caught))
(assert! (equal? (message-of (lambda () (call16 10 1))) 'caught))
(assert! (equal? (message-of (lambda () (tail16 (lambda (x) x) 1))) 'caught))
(assert! (equal? (message-of (lambda () (call16 (lambda (x) x) 1))) 'caught))
(assert! (equal? (message-of (lambda () (tail16 (#%black-box vector-ref) 1))) 'caught))
