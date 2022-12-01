(define (evaluate expr)
    (let ((func (car expr)))
        (cond [(equal? func 'if) 
                => 
                    (if (evaluate (second expr))
                        (evaluate (third expr))
                        (evaluate (fourth expr)))]
        
        
        )

)