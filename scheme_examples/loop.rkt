; (define loop (lambda (x) (if (= x 10000) x (loop (+ x 1)))))

; (define (loop)
;     (define (foo x) (bar x))
;     (define (bar x) (foo x))
;     (foo 10))

; (define (test)
;     (let ((value 123456))
;         (let ((test (lambda (x) value)))
;             test)
;     value))

(define (test)
    (let ((loop void))
        (let ((loop-prime (lambda (x) 
                            (if (= x 10000)
                                x
                                (loop (+ x 1))))))
            (set! loop loop-prime))
    (loop 0)))


(define (blagh x)
    (if (= x 100)
        x
        (begin
            (test)
            (blagh (+ x 1)))))

(blagh 0)