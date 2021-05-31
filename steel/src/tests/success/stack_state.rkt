(define (push element)
    (lambda (stack)
        (list '() (cons element stack))))

(define (pop)
    (lambda (stack)
        (let ((element (car stack))
            (new-stack (cdr stack)))
        (list element new-stack))))

(define stack-of cadr)
(define value-of car)

(define (>>= stack-action continuation)
    (lambda (stack)
        (let ((result (stack-action stack)))
        ((continuation (value-of result)) (stack-of result)))))

(define (return value)
    (lambda (stack)
        (list value stack)))

(define (run-stack computation stack)
    (computation stack))

(define (eval-stack computation stack)
    (value-of (computation stack)))

(define (exec-stack computation stack)
    (stack-of (computation stack)))

(define computation-1 (>>= (push 4) (lambda (_)
                    (>>= (push 5) (lambda (_)
                    (>>= (pop)    (lambda (a)
                    (>>= (pop)    (lambda (b)
                    (return (list a b)))))))))))

(define computation-2 (>>= (push 2) (lambda (_)
                    (>>= (push 3) (lambda (_)
                    (>>= (pop)    (lambda (a)
                    (>>= (pop)    (lambda (b)
                    (return (list a b)))))))))))

(define (main)
    (let ((initial-stack '())
            (composed (>>= computation-1 (lambda (a)
                    (>>= computation-2 (lambda (b)
                    (return (list a b))))))))
        (begin
            (display "Result: ")
            (define result (eval-stack composed initial-stack))
            (display result)
            (newline)
            result)))

(assert! (equal? '((5 4) (3 2)) (main)))