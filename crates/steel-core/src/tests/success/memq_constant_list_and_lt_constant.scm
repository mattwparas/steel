;; `memq` against a short constant list is a chain of inline compares that
;; returns the matching tail. Run hot so the jitted versions are the ones
;; checked.
(define (f x)
  (let ([r (memq x '(a b c))])
    r))
(define (g x)
  (if (memq x '(#\a 1 #t)) 'yes 'no))

(define (run)
  (list (f 'a)
        (f 'b)
        (f 'c)
        (f 'd)
        (f "a")
        (f (string->symbol "b"))
        (g #\a)
        (g 1)
        (g #t)
        (g #f)
        (g 2)
        (g 'a)
        (g 1.0)))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1) (run))))

(assert! (equal? (loop 3000 '()) '((a b c) (b c) (c) #f #f (b c) yes yes yes no no no no)))

;; `<` with a constant fixnum on top compares inline, and anything else still
;; reaches the primitive.
(define limit 10)
(define (below? x)
  (< x 10))
(define (loop-below n acc)
  (if (= n 0)
      acc
      (loop-below (- n 1) (list (below? 3) (below? 12) (below? 9.5) (below? 10.5) (below? -4)))))
(assert! (equal? (loop-below 3000 '()) '(#t #f #t #f #t)))
(assert! (equal? (with-handler (lambda (e) 'raised) (below? "a")) 'raised))
