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

; (define-syntax my-cond
;   (syntax-rules (else)
;     ((_ (else e1 ...))
;      (begin e1 ...))
;     ((_ (e1 e2 ...))
;      (when e1 e2 ...))
;     ((_ (e1 e2 ...) c1 ...)
;      (if e1 
; 	 (begin e2 ...)
; 	 (cond c1 ...)))))

(define-syntax for
  (syntax-rules (in as)
    [(for element in lst body ...)
     (map (lambda (element)
            (begin
                body
                ...))
          lst)]))

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
        [(when a b)
            (if a b void)]))

(define-syntax unless
    (syntax-rules ()
        [(unless a b)
            (if a void b)]))

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

