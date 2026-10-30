;; A body that is #f is the clause's value
(assert! (equal? (match 1 [1 #f] [_ 'x]) #f))

(assert! (equal? (match '(1 . 2) [(cons a b) (list a b)]) '(1 2)))
(assert! (equal? (match '(1 2 3 4) [(list a b ... c) (list a b c)]) '(1 (2 3) 4)))
(assert! (equal? (match '((1 2) (3 4)) [(list (list a b) ...) (list a b)]) '((1 3) (2 4))))
(assert! (equal? (match '(1 2 3) [(list-rest a b rest) (list a b rest)]) '(1 2 (3))))
(assert! (equal? (match '(1 2 3) [(list* a rest) (list a rest)]) '(1 (2 3))))
(assert! (equal? (match '(1 2 3) [(list a ..2) a] [_ 'no]) '(1 2 3)))
(assert! (equal? (match '(1) [(list a ..2) a] [_ 'no]) 'no))

;; Ellipses take the longest run that lets the rest match
(assert! (equal? (match '(1 2 3 4 5) [(list a ... 4 b ...) (list a b)]) '((1 2 3) (5))))
(assert! (equal? (match '(1 2 3) [(list (? odd?) ... rest ...) rest]) '(2 3)))

(assert! (equal? (match 4 [(? number? n) (list 'num n)] [_ 'other]) '(num 4)))
(assert! (equal? (match "ab" [(? string? (app string-length 2)) 'len2]) 'len2))
(assert! (equal? (match '(1 2) [(and l (list a b)) (list l a b)]) '((1 2) 1 2)))
(assert! (equal? (match 2 [(or 1 2) 'one-or-two] [_ 'other]) 'one-or-two))
(assert! (equal? (match 1 [(or (list a) a) a]) 1))
(assert! (equal? (match '(1) [(or (list a) b) (list a b)]) '(1 #f)))
(assert! (equal? (match 3 [(not 4) 'not-four] [_ 'four]) 'not-four))
(assert! (equal? (match 4 [(not 4) 'not-four] [_ 'four]) 'four))
(assert! (equal? (match 4 [(app add1 n) n]) 5))
(assert! (equal? (match 2 [(== 2) 'two] [_ 'no]) 'two))
(assert! (equal? (match 2 [(== 3 <) 'lt] [_ 'no]) 'no))
(assert! (equal? (match '(1 1) [(list a a) 'same] [_ 'diff]) 'same))
(assert! (equal? (match '(1 2) [(list a a) 'same] [_ 'diff]) 'diff))

(assert! (equal? (match (vector 1 2 3) [(vector a ... b) (list a b)]) '((1 2) 3)))
(assert! (equal? (match #(1 2) [(vector a b) (+ a b)]) 3))
(assert! (equal? (match (box 3) [(box v) v]) 3))

(struct pt (x y))
(assert! (equal? (match (pt 1 2) [(pt a b) (+ a b)]) 3))
(assert! (equal? (match (pt 1 2) [(struct pt (a b)) (list a b)]) '(1 2)))
(assert! (equal? (match 5 [(pt a b) 'pt] [_ 'other]) 'other))
(struct mpt (x y) #:mutable)
(assert! (equal? (match (mpt 1 2) [(mpt a b) (list a b)]) '(1 2)))

(assert! (equal? (match (hash 'a 1 'b 2) [(hash-table ('a v)) v] [_ 'no]) 1))
(assert! (equal? (match (hash 'a 1) [(hash-table ('a v) ('b w)) v] [_ 'no]) 'no))
(assert! (equal? (match (hash 'a 1 'b 2) [(hash-table ('a v) rest ...) rest] [_ 'no]) '((b 2))))

(assert! (equal? (match 1 [1 (=> skip) (skip)] [_ 'fallthrough]) 'fallthrough))
(assert! (equal? (match 5 [x #:when (> x 3) 'big] [_ 'small]) 'big))
(assert! (equal? (match 2 [x #:when (> x 3) 'big] [_ 'small]) 'small))

(assert! (equal? (match* (1 2) [(a b) (+ a b)]) 3))
(assert! (equal? (match* (1 2) [(1 3) 'no] [(a 2) a]) 1))
(assert! (equal? ((match-lambda [(list a b) (+ a b)]) '(1 2)) 3))
(assert! (equal? ((match-lambda* [(list a b) (+ a b)]) 1 2) 3))
(assert! (equal? (match-let ([(list a b) '(1 2)] [c 3]) (+ a b c)) 6))
(assert! (equal? (match-let* ([(list a b) '(1 2)] [c (+ a b)]) c) 3))
(match-define (list q (? number? r)) '(1 2))
(assert! (equal? (+ q r) 3))

;; A later clause's body sees its own scope, not an earlier clause's pattern variables
(define x 10)
(assert! (equal? (match '(5) [(list x) #:when #f 1] [_ x]) 10))
(assert! (equal? (match '(5) [(list 6) 1] [_ x]) 10))

;; Clause bodies are in tail position
(define (count-down n)
  (match n
    [0 'done]
    [n (count-down (- n 1))]))
(assert! (equal? (count-down 1000000) 'done))

;; A predicate is an expression where the match is written
(define (big? n)
  (> n 100))
(assert! (equal? (let ([small? (lambda (n) (< n 5))]) (match 3 [(? small?) 'small] [_ 'other])) 'small))
(assert! (equal? (match 200 [(? big?) 'big] [_ 'other]) 'big))

;; `else` ends the clauses, unless it's a local variable, where it's a pattern variable
(assert! (equal? (match 5 [1 'one] [else 'other]) 'other))
(define (local-else)
  (define else #f)
  (match 5 [1 'one] [else (list 'else-var else)]))
(assert! (equal? (local-else) '(else-var 5)))

;; match-syntax looks inside syntax, and binds the syntax itself
(define (kind e)
  (match-syntax e
    [(list 'if c t f) (list 'if (syntax->datum c))]
    [(list 'lambda (list args ...) body) (list 'lambda (map syntax->datum args))]
    [(list a b ... c) (list 'many (syntax->datum a) (length b) (syntax->datum c))]
    [10 'ten]
    [(list) 'empty]
    [(? identifier? x) (list 'id (syntax->datum x))]
    [x (list 'other (syntax->datum x))]))
(assert! (equal? (kind (quasisyntax (if x 1 2))) '(if x)))
(assert! (equal? (kind (quasisyntax (lambda (a b) a))) '(lambda (a b))))
(assert! (equal? (kind (quasisyntax (f 1 2 3))) '(many f 2 3)))
(assert! (equal? (kind (quasisyntax 10)) 'ten))
(assert! (equal? (kind (quasisyntax ())) 'empty))
(assert! (equal? (kind (quasisyntax foo)) '(id foo)))
(assert! (equal? (kind (quasisyntax "s")) '(other "s")))
(assert! (equal? (match-syntax (quasisyntax (a b)) [(list x y) #f] [_ 'fell-through]) #f))
(assert! (equal? (match-syntax (quasisyntax (#%prim.null? (#%prim.cdr y)))
                   [`(#%prim.null? (#%prim.cdr ,e)) (syntax->datum e)]
                   [_ 'no])
                 'y))
(assert! (equal? (match-syntax (quasisyntax (a (b c))) [(list _ (or (list 'b x) x)) (syntax->datum x)])
                 'c))

;; And works in a transformer
(define-syntax (second-form stx)
  (match-syntax stx
    [(list _ a b) b]))
(assert! (equal? (second-form 1 2) 2))
(define-syntax (hash-rest stx)
  (datum->syntax stx
                 (list 'quote (match (hash 'a 1 'b 2) [(hash-table ('a v) rest ...) (list v rest)]))))
(assert! (equal? (hash-rest) '(1 ((b 2)))))
