(struct node (datum children))

(define (parse expr)
(parse-helper expr '() '()))

(define (parse-helper expr operators operands)
    (cond ((null? expr)
            (if (null? operators)
                (car operands)
                (handle-op '() operators operands)))
            ((number? (car expr))
            (parse-helper (cdr expr)
                        operators
                        (cons (node (car expr) '()) operands)))
            ((list? (car expr))
            (parse-helper (cdr expr)
                        operators
                        (cons (parse (car expr)) operands)))
            (else (if (or (null? operators)
                        (> (precedence (car expr))
                            (precedence (car operators))))
                    (parse-helper (cdr expr)
                                    (cons (car expr) operators)
                                    operands)
                    (handle-op expr operators operands)))))

(define (handle-op expr operators operands)
    (parse-helper expr
                    (cdr operators)
                    (cons (node (car operators)
                                    (list (cadr operands) (car operands)))
                        (cddr operands))))

; (define (member? x los)
;     (cond
;         ((null? los) #f)
;         ((equal? x (car los)) #t)
;         (else (member? x (cdr los)))))


(define (precedence oper)
    (if (member oper '(+ -))
        1
        2))


(define (compute tree)
    (if (number? (node-datum tree))
        (node-datum tree)
        (+
         ;; (function-named-by (node-datum tree))
            (compute (car (node-children tree)))
            (compute (cadr (node-children tree))))))

(define (function-named-by oper)
    (cond ((equal? oper '+) +)
            ((equal? oper '-) -)
            ((equal? oper '*) *)
            ((equal? oper '/) /)
            (else (error! "no such operator as" oper))))

; (define expr (parse '(1 + 2)))

; (assert! (equal? 3 (compute (parse '(1 + 2))))) ;; => 10

; (assert! (equal? 10 (compute (parse '(1 + 2 + 3 + 4))))) ;; => 10

(parse '(1 + 2))
