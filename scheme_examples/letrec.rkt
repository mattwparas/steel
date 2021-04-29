; (let ([f void])
;   (let ([ft (let ([g void])
;               (let ([gt (let ([x 5])
;                               (lambda () ...))])
;                     (set! g gt))
;                   (lambda () ... g ...))])
;   (set! f ft))
;   f)


;; Just turn the whole thing into a let-rec?
(define (blah)
    (define x 10)
    (displayln "hello world")
    (define (foo x) (if (= x 10000) x (bar (+ x 1))))
    (define (bar x) (if (= x 10000) x (foo (+ x 1))))
    (displayln x)
    (foo 0))

;; TODO
;; handle case where define sequentially uses a define
;; before it
;; this should turn into a let* effectively
(define (blargh)
  (define x (+ 1 2 3 4))
  (define y (+ 1 2 3 4 x))
  (displayln x)
  (displayln y)
  (list 1 2 3 4 5 x y))

(define blargh 
  (lambda () 
    ((lambda (x y) 
      ((lambda (#####x0 #####y1) 
        (begin 
          (set! x #####x0) 
          (set! y (#####y1)) (displayln x) (displayln y) (list 1 2 3 4 5 x y))) (+ 1 2 3 4) (lambda () (+ 1 2 3 4 x)))) 123 123)))


(define blah 
  (lambda () 
    ((lambda (x #####define-conversion1 foo bar) 
      ((lambda (#####x0 #####define-conversion1 #####foo2 #####bar3) 
        (begin 
          (set! x #####x0) 
          (set! foo #####foo2) 
          (set! bar #####bar3) 
          (displayln x) 
          (foo 0))) 
        10 
        (displayln "hello world") 
        (lambda (x) (if (= x 10000) x (bar (+ x 1)))) 
        (lambda (x) (if (= x 10000) x (foo (+ x 1)))))) 
    0 0 0 0)))


(define handle-intern-letter 
  (lambda (char-list lst prefix-chars) 
    ((lambda (char next-prefix) 
      ((lambda (#####char0 #####next-prefix1) 
        (begin 
          (set! char #####char0) 
          (set! next-prefix #####next-prefix1) 
          (if (empty? lst) 
            (begin (list (trie char (create-children (rest char-list) empty next-prefix) #false next-prefix))) 
            (if (< char (trie-char (first lst))) 
              (begin (cons (trie char (create-children (rest char-list) empty next-prefix) #false next-prefix) lst)) (if (= char (trie-char (first lst))) (begin (cons (trie char (create-children (rest char-list) (trie-children (first lst)) next-prefix) (trie-end-word? (first lst)) (trie-word-up-to (first lst))) (rest lst))) (begin (cons (first lst) (create-children char-list (rest lst) prefix-chars)))))))) 
              
      (first char-list) (push-back prefix-chars char))) 0 0)))


(define handle-last-letter 
  (lambda (char-list lst prefix-chars) 
    ((lambda (char next-prefix) 
      ((lambda (#####char0 #####next-prefix1) 
        (begin 
          (set! char #####char0) 
          (set! next-prefix #####next-prefix1) 
          (if (empty? lst) 
            (begin (list (trie char empty #true next-prefix))) (if (< char (trie-char (first lst))) (begin (cons (trie char empty #true next-prefix) lst)) (if (= char (trie-char (first lst))) (begin (cons (trie char (trie-children (first lst)) #true next-prefix) (rest lst))) (begin (cons (first lst) (create-children char-list (rest lst) prefix-chars)))))))) 
      
      (first char-list) (push-back prefix-chars char))) 69 69)))

(define (test)
  (let ((x 10) (y 0) (foo void) (bar void))
        (let ((foo-prime (lambda (x) (if (= x 10000) x (bar (+ x 1)))))
              (bar-prime (lambda (x) (if (= x 10000) x (foo (+ x 1))))))
              (set! foo foo-prime)
              (set! bar bar-prime)
              (set! y x))
        (foo 0)))

(define (test-loop)
  (let ((loop void))
    (let ((loop-prime (lambda (x) 
                            (if (= x 10000)
                                x
                                (loop (+ x 1))))))
      (set! loop loop-prime))
    (loop 0)))


(define (test-loop-2)
  (define (loop x)
    (if (= x 10000)
        x
        (loop (+ x 1))))
  (loop 0))

(define test-loop-2 
  (lambda () 
    ((lambda (loop) 
      ((lambda (#####loop0) 
        (begin 
        (set! loop #####loop0) 
        (loop 0))) 
      (lambda (x) 
        (if (= x 10000) 
            x 
            (loop (+ x 1)))))) 0)))


(define (thing)
  (define blagh 10)
  (displayln 15)
  (define blegh 20)
  (displayln 25))



;; TODO this transformation is how I should implement defines
;; Do compile time transformation of AST -> this form

; (let ((is-even? void) (is-odd? void))
;   (let ((is-even?-prime 
;           (lambda (n) (or (zero? n) 
;                           (is-odd? (sub1 n)))))
        
;         (is-odd?-prime
;           (lambda (n) (and (not (zero? n))
;                            (is-even? (sub1 n))))))

;         (set! is-even? is-even?-prime)
;         (set! is-odd? is-odd?-prime))
;   (is-odd? 12))