; #lang racket

(define (flatten lst)
;   (displayln "inside flatten")
  (cond ((null? lst) '())
        ((list? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))


(define (_list-for-each func lsts)
    ; (displayln "calling list for each")
    ; (displayln func)
    ; (displayln lsts)
    ; (displayln lsts)
    (if (null? (flatten lsts))
        void
        (begin
            (apply func (map car lsts))
            (if (null? lsts)
                void
                (_list-for-each func (map cdr lsts))))))

(define (for-each func . lsts)
    ; (displayln "calling normal for each")
    ; (display "func: ") (displayln func)
    ; (display "args: ") (displayln lsts)
    (_list-for-each func lsts))

(define x  2.0)
(define xi 0.5)
(define y  4.0)
(define yi 0.25)
(define z  (+ x y))
(define zi (/ 1 (+ x y)))

(define number (list x y z))
(define inverse (list xi yi zi))

(define (multiplier n1 n2) (lambda (m) (* n1 n2 m)))

(define m 0.5)
(define (go n1 n2)
  (for-each (lambda (n1 n2)
              (displayln ((multiplier n1 n2) m)))
            n1 n2))


; (define (go n1 n2)
;   (for-each (lambda (x y)
;               (displayln (list x y)))
;             n1 n2))

; (go number inverse)