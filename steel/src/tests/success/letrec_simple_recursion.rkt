(define (test)
    (let ((loop void))
        (let ((loop-prime (lambda (x) 
                            (if (= x 10000)
                                x
                                (loop (+ x 1))))))
            (set! loop loop-prime))
    (loop 0)))

; (assert! (= (test) 10000))