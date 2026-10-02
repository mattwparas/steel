;; A group of local functions that call each other and share many `set!`
;; variables. Lifting would give them more parameters than the arity cap, so
;; they stay as closures; the results must not change either way.
(define (run-machine input)
  (let ([pos 0]
        [len (string-length input)]
        [count-a 0]
        [count-b 0]
        [count-other 0]
        [state 'start]
        [steps 0]
        [last #\space]
        [seen '()])
    (define (next!)
      (set! steps (+ steps 1))
      (if (< pos len)
          (let ([c (string-ref input pos)])
            (set! pos (+ pos 1))
            (set! last c)
            c)
          #f))
    (define (start c)
      (cond
        [(not c) (finish)]
        [(char=? c #\a) (set! count-a (+ count-a 1)) (in-a (next!))]
        [(char=? c #\b) (set! count-b (+ count-b 1)) (in-b (next!))]
        [else (set! count-other (+ count-other 1)) (start (next!))]))
    (define (in-a c)
      (set! state 'a)
      (set! seen (cons 'a seen))
      (start c))
    (define (in-b c)
      (set! state 'b)
      (set! seen (cons 'b seen))
      (start c))
    (define (finish)
      (list count-a count-b count-other state steps last (length seen)))
    (start (next!))))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1) (run-machine "abxxbaab"))))

(assert! (equal? (loop 2000 '()) '(3 3 2 b 9 #\b 6)))
