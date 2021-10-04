(struct Iterator (stack))

(struct Mapping (func))
(struct Filtering (predicate))
(struct Taking (num count))

(define (mapping-2 func)
    (Iterator (list (Mapping func))))

(define (filtering-2 pred)
    (Iterator (list (Filtering pred))))

(define (taking num)
    (Iterator (list (Taking num 0))))


(define (flatten lst)
  (cond ((null? lst) '())
        ((list? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))

;; Build up stack of functions to apply
(define (compose-2 a b)
    (Iterator (flatten (append (Iterator-stack a) (Iterator-stack b)))))


(define m (mapping-2 (fn (x) (+ x 1))))
(define f (filtering-2 (fn (x) (even? x))))

(define z (compose-2 m f))

(Iterator-stack z)


;; 
(define (apply-stack stack value)
    (if (null? stack)
        value
        (let ((transducer (car stack)))
            (cond 
                [(Mapping? transducer) 
                (apply-stack (cdr stack) ((Mapping-func transducer) value))]
                [(Filtering? transducer) 
                 (apply-stack 
                    (cdr stack) 
                    (if ((Filtering-predicate transducer) value)
                        value
                        void))]
                [else (error "Iterator with internal state not handled yet")]))))


; (apply-stack (Iterator-stack z) 2)

(define (transducing-foo iterator-stack lst res)
    (if (null? lst)
        res
        (let ((output (apply-stack iterator-stack (car lst))))
            (if output
                (transducing-foo iterator-stack (cdr lst) (cons output res))
                (transducing-foo iterator-stack (cdr lst) res)))))

(define (transducing iterator lst)
    (reverse (transducing-foo (Iterator-stack iterator) lst '())))


(define xf (compose (mapping (fn (x) (+ x 1)))
                    (filtering (fn (x) (even? x)))))

(define r (range 0 1000))

(transducing z r)
; (execute xf r)

;; Run through the stack and spit out the result
; (define (execute stack input result)

;     (if (null? stack)
;         result
;         (let* ((transducer (car stack))
;                (name (Transducer-name transducer))
;                (func (Transducer-func transducer)))
;             (cond [(equal? 'map name)])
;         )))

;     (cond [(null? stack) result]
;           [()]
;     )


