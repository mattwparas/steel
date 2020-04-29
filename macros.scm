


(define-syntax map-test
    (syntax-rules ()
        [(map-test (x ...) lst)
            (map (lambda (v) (x ... v)) lst)]))

(displayln (map-test (* 2) (list 1 2 3 4)))

(define-syntax filter-test
    (syntax-rules ()
        [(filter-test (x ...) lst)
            (filter (lambda (v) (x ... v)) lst)]))

(displayln (filter-test (= 2) (list 1 2 3 4 5 2 4 2)))


; (define-syntax for
;   (syntax-rules (in as)
;     [(for element in lst body ...)
;      (begin
;           (map (lambda (element)
;             (begin
;                 body
;                 ...))
;           lst)
;           void)]))

; (for i in (range 0 10) (displayln i))


(define-syntax when
    (syntax-rules ()
        [(when a b ...)
            (if a (begin b ...) void)]))

(define-syntax cond
    (syntax-rules (else)
        [(cond [else e1 ...])
            (begin e1 ...)]
        [(cond [e1 e2 ...])
            (when e1 e2 ...)]
        [(cond [e1 e2 ...] c1 ...)
            (if e1
                (begin e2 ...)
                (cond c1 ...))]))

(cond [else (displayln "no cases -> single else prints!")])

(cond [#f (displayln "shouldn't print!")]
            [else (displayln "single false case goes to else -> should print!")])

(cond [#t (displayln "guarded evaluation, no else turns into when -> should print!")])

(cond 
    [#f (displayln "case 1 -> shouldn't print")]
    [#f (displayln "case 2 -> shouldn't print")]
    [#t (displayln "case 3 -> should print!")
        (displayln "case 3 -> extra thing should print!")]
    [else 
        (displayln "else case -> shouldn't print")
        (displayln "extra thing definitely shouldn't print")])


(define-syntax or
    (syntax-rules ()
        [(or) #f]
        [(or x) x]
        [(or x y) (let ([z x])
                    (if z z y))]
        [(or x y ...) (or x (or y ...))]))

(define-syntax and
    (syntax-rules ()
        [(and) #t]
        [(and x) x]
        [(and x y) (if x y #f)]
        [(and x y ...) (and x (and y ...))]))


(define-syntax unless
    (syntax-rules ()
        [(unless a b ...)
            (if a void (begin b ...))]))

(define-syntax swap
    (syntax-rules ()
        [(swap a b)
            (let ([tmp b])
                (begin
                    (set! b a)
                    (set! a tmp)))]))

(define a 10)
(define b 20)

(displayln a)
(displayln b)

(swap a b)

(displayln a)
(displayln b)

(when #t (displayln "hello world when!"))
(when #f (displayln "shouldn't print!"))

(unless #t (displayln "unless shouldn't print"))
(unless #f (displayln "hello world unless!"))

(define-syntax while
    (syntax-rules (do)
        [(while cond do body ...)
            (begin
                (define (loop) 
                    (when cond 
                        (begin
                            body ...
                            (loop))))
                (loop)))])

(define x 10)

(while (> x 0) do
    (displayln x)
    (set! x (- x 1)))

(displayln (or #f #f #f #f #f #t))
(displayln (and #t #t #t #t #t #t))
(displayln (and #t #t #t #t #f))

