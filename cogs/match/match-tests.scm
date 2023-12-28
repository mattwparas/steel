(provide __module__)
(define __module__ 'match-tests)

(define (test name input expected)
  (assert! (equal? input expected))
  (if (equal? input expected)
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display "OK")
        (newline))
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display "FAILED")
        (newline)
        (display "    Expected: ")
        (display expected)
        (display ", Found ")
        (displayln input))))

; ;; a => 1
; ;; x => '(2 3 4 5)
(test "Basic ellipses matching works"
      (match '(1 2 3 4 5)
        [(list a x ...) x])
      (list 2 3 4 5))

(test "Ellipses matches until next value"
      (match (list 1 2 3 4 5)
        [(list first rest ... last) rest])
      (list 2 3 4))

(test "match list of constants"
      (match (list 1 2 3 4 5)
        [(list 1 2 3 4 5) 'case1])
      'case1)

(test "match binds free vars"
      (match (list 1 2 3 4 5)
        [(list x 2 y 4 z) (+ x y z)])
      (+ 1 3 5))

(test "match binds to first matching case"
      (match (list 1 2 3 4 5)
        [?x 'case1]
        [(list ?a ?b ?c ?d ?e) 'case2])
      'case1)

(test "match constant"
      (match 10
        [10 'case1])
      'case1)

(test "takes else case when nothing matches"
      (match (list 1 (list 2 (list 3)))
        [(list x y z) 'case1]
        [24 'case2]
        [else 'case3])
      'case3)

(test "Empty list matches empty list"
      (match '()
        [() 'found-empty-list!]
        [(list x xs...) 'found-list!])
      'found-empty-list!)

(test "match nested list"
      (match (list (list 1 2) 3 (list 4 (list 5 6)))
        [(list (list a 2) b (list c (list 5 6))) (+ a b c)])
      (+ 1 3 4))

(test "wildcards work"
      (match (list 1 2 3 4 5)
        [(list 1 2 _ _ a) a])
      5)

;; Generic optimization passes?
(test "pattern matching against anonymous function application"
      (match '((lambda (x) 10) 20)

        [(list (list 'lambda (list var) body) arg) body])
      10)

(define (quoted? x)
  (and (list? x) (not (null? x)) (equal? (car x) 'quote)))

(define (constant? x)
  (or (number? x) (quoted? x)))

(define (identify-sequential-maps expr)
  (match expr
    ;; Matching against quasiquotes, should help with real pattern matching?
    [`(map ,func1 (map ,func2 ,lst))
     `(map (lambda (x)

             (,func2 (,func1 x)))
           ,lst)]
    [_ expr]))

(identify-sequential-maps '(map add1 (range 0 100)))
(identify-sequential-maps '(map add1 (map sub1 (range 0 100))))

(define my-expr
  '(define (foo-bar x)
     ((lambda (y) 100) x)))

(define (remove-constant-anonymous-functions expr)
  (match expr
    [`((lambda (,var) ,body) ,@args) (if (constant? body) body expr)]
    [(list args ...) (map remove-constant-anonymous-functions args)]
    [_ expr]))

; Nano pass framework for rewriting and incrementally lowering!
(remove-constant-anonymous-functions my-expr)
