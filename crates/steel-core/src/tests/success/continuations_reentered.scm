;; Programs that go back into continuations: generators, coroutines,
;; backtracking search. Each continuation here outlives the frame it was
;; captured in and is called after it, most of them many times.

;; A generator over a tree, driven from outside one value at a time.
(define (tree-walker tree)
  (define caller #f)
  (define (walk node)
    (cond
      [(null? node) void]
      [(pair? node)
       (walk (car node))
       (walk (cdr node))]
      [else (call/cc (lambda (rest)
                       (set! resume (lambda () (rest void)))
                       (caller node)))]))
  (define resume
    (lambda ()
      (walk tree)
      (caller 'done)))
  (lambda () (call/cc (lambda (k) (set! caller k) (resume)))))

(define (leaves tree)
  (let ([next (tree-walker tree)])
    (let loop ([acc '()])
      (let ([leaf (next)])
        (if (eq? leaf 'done) (reverse acc) (loop (cons leaf acc)))))))
(assert! (equal? (leaves '((1 2) (3 (4 5)) 6)) '(1 2 3 4 5 6)))
(assert! (equal? (leaves '()) '()))

;; Two trees have the same fringe, found by walking both a leaf at a time.
(define (same-fringe? a b)
  (let ([next-a (tree-walker a)]
        [next-b (tree-walker b)])
    (let loop ()
      (let ([x (next-a)]
            [y (next-b)])
        (cond
          [(not (equal? x y)) #f]
          [(eq? x 'done) #t]
          [else (loop)])))))
(assert! (same-fringe? '(1 (2 3) 4) '((1 2) (3 4))))
(assert! (not (same-fringe? '(1 (2 3) 4) '((1 2) (4 3)))))
(assert! (not (same-fringe? '(1 2 3) '(1 2))))

;; Two coroutines handing control back and forth.
(define (ping-pong rounds)
  (let ([log (box '())]
        [other (box #f)])
    (define (transfer! tag)
      (call/cc (lambda (me)
                 (let ([to (unbox other)])
                   (set-box! other me)
                   (to tag)))))
    (define (player name n)
      (when (> n 0)
        (set-box! log (cons name (unbox log)))
        (transfer! name)
        (player name (- n 1))))
    (call/cc (lambda (finish)
               (call/cc (lambda (start-second)
                          (set-box! other start-second)
                          (player 'ping rounds)
                          (finish 'ping-done)))
               (player 'pong rounds)
               (finish 'pong-done)))
    (reverse (unbox log))))
(assert! (equal? (ping-pong 3) '(ping pong ping pong ping pong)))

;; Backtracking with `amb`: each choice point is a continuation that is gone
;; back into when a later requirement fails.
(define (solve-triple limit)
  (let ([fail-stack (box '())])
    (define (fail)
      (if (null? (unbox fail-stack))
          (error "no more choices")
          (let ([back (car (unbox fail-stack))])
            (set-box! fail-stack (cdr (unbox fail-stack)))
            (back back))))
    (define (amb choices)
      (let ([cc (call/cc (lambda (k) k))])
        (if (null? choices)
            (fail)
            (let ([choice (car choices)])
              (set! choices (cdr choices))
              (set-box! fail-stack (cons cc (unbox fail-stack)))
              choice))))
    (define (must ok) (unless ok (fail)))
    (define (range-list a b) (if (> a b) '() (cons a (range-list (+ a 1) b))))
    (let* ([a (amb (range-list 1 limit))]
           [b (amb (range-list a limit))]
           [c (amb (range-list b limit))])
      (must (= (+ (* a a) (* b b)) (* c c)))
      (list a b c))))
(assert! (equal? (solve-triple 13) '(3 4 5)))

;; The same continuation gone back into from several depths of recursion.
(define (count-reentries n)
  (let* ([saved (box #f)]
         [total (box 0)]
         [v (call/cc (lambda (k) (set-box! saved k) 0))])
    (set-box! total (+ (unbox total) v))
    (define (deeper d)
      (if (= d 0)
          ((unbox saved) 1)
          (+ 1 (deeper (- d 1)))))
    (if (< (unbox total) n)
        (deeper (* 10 (unbox total)))
        (unbox total))))
(assert! (= (count-reentries 25) 25))

;; A continuation that is kept, then a great deal of other work and several
;; collections, then gone back into.
(define (kept-across-collections)
  (let* ([saved (box #f)]
         [passes (box 0)]
         [local (list 'still 'here)]
         [v (call/cc (lambda (k) (set-box! saved k) 'first))])
    (set-box! passes (+ (unbox passes) 1))
    (let churn ([i 0] [junk '()])
      (when (< i 20000)
        (churn (+ i 1) (if (= 0 (modulo i 100)) '() (cons (box i) junk)))))
    (#%gc-collect)
    (#%gc-collect)
    (if (< (unbox passes) 3)
        ((unbox saved) (unbox passes))
        (list v local (unbox passes)))))
(assert! (equal? (kept-across-collections) '(2 (still here) 3)))

;; Locals of the frame a continuation goes back to are as they were when it
;; was captured, and boxes are as they are now.
(define (locals-and-boxes)
  (let* ([b (box 0)]
         [saved (box #f)]
         [n 10]
         [v (call/cc (lambda (k) (set-box! saved k) 0))])
    (set-box! b (+ (unbox b) 1))
    (let ([n (+ n v)])
      (if (< (unbox b) 3)
          ((unbox saved) (* 100 (unbox b)))
          (list n (unbox b))))))
(assert! (equal? (locals-and-boxes) '(210 3)))
