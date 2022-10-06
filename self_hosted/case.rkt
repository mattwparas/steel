(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda)
     (lambda args
       (error "CASE-LAMBDA without any clauses.")))
    ((case-lambda 
      (?a1 ?e1 ...) 
      ?clause1 ...)
     (lambda args
       (let ((l (length args)))
         (case-lambda "CLAUSE" args l 
           (?a1 ?e1 ...)
           ?clause1 ...))))
    ((case-lambda "CLAUSE" ?args ?l 
      ((?a1 ...) ?e1 ...))
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (error! "Arity mismatch")
         
         ))
    ((case-lambda "CLAUSE" ?args ?l 
      ((?a1 ...) ?e1 ...) 
      ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))
    ((case-lambda "CLAUSE" ?args ?l
      ((?a1 . ?ar) ?e1 ...) 
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
       ?clause1 ...))
    ((case-lambda "CLAUSE" ?args ?l 
      (?a1 ?e1 ...)
      )
     (let ((?a1 ?args))
       ?e1 ...))
    ((case-lambda "CLAUSE" ?args ?l)
     (error "Wrong number of arguments to CASE-LAMBDA."))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
      ?clause1 ...))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
      ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))))

; (define-syntax case-lambda
;   (syntax-rules ()
;     ((case-lambda)
;      (lambda args
;        (error "CASE-LAMBDA without any clauses.")))
;     ((case-lambda 
;       (?a1 ?e1 ...) 
;       ?clause1 ...)
;      (lambda args
;        (let ((l (length args)))
;          (case-lambda "CLAUSE" args l 
;            (?a1 ?e1 ...)
;            ?clause1 ...))))
;     ;; TODO: Fix ... conditions
;     ((case-lambda "CLAUSE" ?args ?l 
;       ((?a1 ...) ?e1 ...) 
;       ?clause1 ...)
;      (if (= ?l (length '(?a1 ...)))
;          (apply (lambda (?a1 ...) ?e1 ...) ?args)
;          (case-lambda "CLAUSE" ?args ?l 
;            ?clause1 ...)))
;     ((case-lambda "CLAUSE" ?args ?l
;       ((?a1 . ?ar) ?e1 ...) 
;       ?clause1 ...)
;      (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
;        ?clause1 ...))
;     ((case-lambda "CLAUSE" ?args ?l 
;       (?a1 ?e1 ...))
;      (let ((?a1 ?args))
;        ?e1 ...))
;     ((case-lambda "CLAUSE" ?args ?l 
;       (?a1 ?e1 ...)
;       ?clause1 ...)
;      (let ((?a1 ?args))
;        ?e1 ...))
;     ((case-lambda "CLAUSE" ?args ?l)
;      (error "Wrong number of arguments to CASE-LAMBDA."))
;     ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
;       ?clause1 ...)
;      (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
;       ?clause1 ...))
;     ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
;       ?clause1 ...)
;      (if (>= ?l ?k)
;          (apply (lambda ?al ?e1 ...) ?args)
;          (case-lambda "CLAUSE" ?args ?l 
;            ?clause1 ...)))))

(define greet
    (case-lambda
      [(name) (string-append "Hello, " name)]
      [(given surname) (string-append "Hello, " (string-append given (string-append " " surname)))]))

(greet "John")
(greet "John" "Smith")
; (greet "John" "Smith" "Extra")

(define plus
    (case-lambda 
        (() 0)
        ((x) x)
        ((x y) (+ x y))
        ((x y z) (+ (+ x y) z))
        (args (apply + args))))

(plus)
(plus 1)
(plus 1 2 3)