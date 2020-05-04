; (displayln "hello world2!")
; (define big-list (range 0 10000))
; (displayln (take 100 big-list))
; (displayln (take 50 big-list))

; (define-syntax let
;     (syntax-rules ()
;         ))

; (define-syntax let
;   (syntax-rules ()
;     ((let ((var val) ...) body ...)
;       ((lambda (var ...) body ...) val ...))))

; (cond
;     ([test-expr then-expr] ...)
;     [else else-expr])

; (pattern ...) => Many(pattern) ?


; [(my-cond [test-expr then-expr] ...]

; Handle multiple ellipses
; (define-syntax test-cond
;     (syntax-rules (else)
;         [(test-cond [else e1 ...])
;             (begin e1 ...)]
;         [(test-cond [e1 e2 ...])
;             (when e1 e2 ...)]
;         [(my-cond [e1 e2 ...] c1 ...)
;             (if e1
;                 (begin e2 ...)
;                 (cond c1 ...))]))

;; maybe treat the `variable ...` as one thing?

; (define-syntax my-cond
;   (syntax-rules (else)
;     [(my-cond [else e1 ...])
;      (begin e1 ...)]
;     [(my-cond (e1 e2 ...))
;      (when e1 e2 ...)]
;     [(my-cond (e1 e2 ...) c1 ...)
;      (if e1 
; 	    (begin e2 ...)
; 	    (cond c1 ...))]))

(define-syntax map-test
    (syntax-rules ()
        [(map-test (x ...) lst)
            (map (lambda (v) (x ... v)) lst)]))

(displayln (map-test (* 2) (list 1 2 3 4)))

(define-syntax for
  (syntax-rules (in as)
    [(for element in lst body ...)
     (begin
          (map (lambda (element)
            (begin
                body
                ...))
          lst)
          void)]))

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

(define-syntax when
    (syntax-rules ()
        [(when a b ...)
            (if a (begin b ...) void)]))

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

; (define-syntax while
;     (syntax-rules (do)
;         [(while cond do body ...)
;             (begin
;                 (define (loop) 
;                     (when cond 
;                         (begin
;                             body ...
;                             (loop))))
;                 (loop)))])

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
; (set! x (- x 1))

(while (> x 0) do
    (displayln x)
    (set! x (- x 1)))

(displayln (or #f #f #f #f #f #t))
(displayln (and #t #t #t #t #t #t))
(displayln (and #t #t #t #t #f))


(for i in (range 0 10) (displayln i))

