;; A `set!` of a boxed variable whose value can spill the stack evaluates the
;; value before reading the box. Run hot so the jitted versions are checked.
(define (bump x)
  (+ x 1))

(define (make-state)
  (define n 0)
  (define c #\a)
  (define log '())
  (lambda (k)
    ;; An `if`, a `let` and a call in the value.
    (set! c (if (> k 2) #\b #\c))
    (set! n (let ([m (* k 2)]) (+ n m)))
    (set! n (bump n))
    ;; The value sets the same variable first.
    (set! n (begin (set! n (+ n 1)) (+ n 1)))
    (set! log (if (even? k) (cons k log) log))
    (list n c (length log))))

(define state (make-state))
(define (drive i last)
  (if (> i 3000)
      last
      (drive (+ i 1) (state i))))
;; Each call adds 2k + 3.
(assert! (equal? (drive 1 #f) (list (+ (* 3000 3001) (* 3 3000)) #\b 1500)))

;; A local box that is not captured, set with a spilling value in a loop.
(define (count-up n)
  (let ([total 0]
        [get (lambda () 0)])
    (let loop ([i 0])
      (when (< i n)
        (set! total (if (odd? i) (+ total (bump i)) total))
        (set! get (lambda () total))
        (loop (+ i 1))))
    (get)))
(define (repeat k acc)
  (if (= k 0)
      acc
      (repeat (- k 1) (count-up 100))))
(assert! (equal? (repeat 300 0) 2550))

;; A raising value leaves the variable unchanged.
(define (make-guarded)
  (define v 'start)
  (lambda (x)
    (with-handler (lambda (e) 'raised) (set! v (let ([y (car x)]) y)))
    v))
(define guarded (make-guarded))
(define (loop-guarded i last)
  (if (= i 0)
      last
      (loop-guarded (- i 1) (list (guarded '(a)) (guarded 5)))))
(assert! (equal? (loop-guarded 2000 #f) '(a a)))
