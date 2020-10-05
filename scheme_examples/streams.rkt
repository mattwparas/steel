;; (define (add-streams s1 s2)
;;  (let ((h1 (head s1))
;;        (h2 (head s2)))
;;    (cons-stream
;;      (+ h1 h2)
;;      (add-streams (tail s1) (tail s2)))))


(define (stream-cdr stream)
  ((stream-cdr' stream)))

(define (integers n)
  (stream-cons n (lambda () (integers (+ 1 n)))))

(define (in-range-stream n m)
  (if (= n m)
      empty-stream
      (stream-cons n (lambda () (in-range-stream (add1 n) m)))))

(define (append-streams s1 s2)
  (cond
    [(stream-empty? s1) s2]
    [(stream-empty? s2) s1]
    [else
     (stream-cons (stream-car s1)
                  (lambda () (append-streams (stream-cdr s1) s2)))]))

(define (add-streams s1 s2)
 (let ((h1 (stream-car s1))
       (h2 (stream-car s2)))
   (stream-cons
     (+ h1 h2)
     (lambda () (add-streams (stream-cdr s1) (stream-cdr s2))))))


(define (merge-streams s1 s2)
  (cond
    [(stream-empty? s1) s2] ; nothing to merge from s1
    [(stream-empty? s2) s1] ; nothing to merge from s2
    [else (let ([h1 (stream-car s1)]
                [h2 (stream-car s2)])
            (stream-cons h1
                         (lambda ()
                           (stream-cons h2
                                        (lambda ()
                                          (merge-streams (stream-cdr s1)
                                                         (stream-cdr s2)))))))]))

(define (map-stream func s)
  (cond
    [(stream-empty? s) s]
    [else
     (stream-cons (func (stream-car s))
                  (lambda ()
                    (map-stream func (stream-cdr s))))]))


(define (list->stream lst)
  (if (null? lst)
      empty-stream
      (stream-cons (car lst)
                   (lambda ()
                     (list->stream (cdr lst))))))


(define (stream->list s)
  (define (*stream->list s lst)
    (if (stream-empty? s)
        lst
        (*stream->list (stream-cdr s) (cons (stream-car s) lst))))
  (*stream->list s '()))


;; (define (stream-section n stream)
;;   (cond ((= n 0) '())
;;         (else
;;          (cons
;;           (head stream)
;;           (stream-section
;;            (- n 1)
;;            (tail stream))))))

;; execute / transducer work with transducers
;; these are like source agnostic

(execute (taking 15)
         (add-streams (integers 0) (integers 0)))

(execute (taking 15)
         (merge-streams (integers 0) (integers 0)))


(execute (taking 15)
         (merge-streams
          (merge-streams (integers 0) (integers 0))
          (add-streams (integers 0) (integers 0))))



(execute (taking 15)
         (map-stream (lambda (x) 10) (integers 0)))


(stream->list (in-range-stream 0 10))


;; make a stream out of a port
(define my-port (open-input-file "scheme_examples/dfs.rkt"))

(define (port-stream)
  (let ((head (read-line-from-port my-port)))
    (if (equal? 'eof head)
        empty-stream
        (stream-cons head (lambda () (port-stream))))))

;; Make a stream out of a port
;; Access the port using the given func
;; Stop reading w/ the given end-sym
(define (port->stream p func end-sym)
  (let ((head (func p)))
    (if (equal? end-sym head)
        empty-stream
        (stream-cons head (lambda () (port->stream p func end-sym))))))




(execute (taking 15) (port-stream my-port read-line-from-port 'eof))


;; (define-syntax unquote
;;   (syntax-rules ()
;;     ((unquote datum) datum)))

;; This is close to what I need, but not quite exactly what I need
;; Look into this more later tonight
(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((quasiquote (unquote datum))
     datum)
    ((quasiquote ((unquote-splicing datum) next))
     (append datum (quasiquote next)))
    ((quasiquote (datum next ...))
     (cons (quasiquote datum) (quasiquote next ...)))
    ((quasiquote datum)
     (quote datum))))



(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote xs ...)))
    ((quasiquote ((unquote-splicing x)))        (append (list x) (quote ())))
    ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
    ((quasiquote (unquote x))                 x)
    ((quasiquote (x))                          (quote (x)))
    ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
    ((quasiquote x)                           (quote x))))

;; (define-syntax quasiquote
;;   (syntax-rules (unquote unquote-splicing)
;;     ((quasiquote ((unquote x) . xs))          (cons x (quasiquote xs)))
;;     ((quasiquote ((unquote-splicing x) . xs)) (append x (quasiquote xs)))
;;     ((quasiquote (unquote x))                 x)
;;     ((quasiquote (x  . xs))                   (cons (quasiquote x) (quasiquote xs)))
;;     ((quasiquote x)                           (quote x))))

;; (define-syntax quasiquote
;;   (syntax-rules (unquote unquote-splicing)
;;     ((quasiquote (unquote datum))
;;      datum)
;;     ((quasiquote ((unquote-splicing datum) . next))
;;      (append datum (quasiquote next)))
;;     ((quasiquote (datum . next))
;;      (cons (quasiquote datum) (quasiquote next)))
;;     ((quasiquote datum)
;;      (quote datum))))


(quasiquote (0 1 2)) ;; => '(0 1 2)
(quasiquote (0 (unquote (+ 1 2)) 4)) ;; => '(0 3 4)
(quasiquote (0 (unquote-splicing (list 1 2)) 4)) ;; '(0 1 2 4)
(quasiquote (0 (unquote-splicing 1) 4)) ;; error
(quasiquote (0 (unquote-splicing 1))) ;; '(0 1)


;; (define-syntax infix
;;    (syntax-rules (plus times stack input eof)
;;      ((infix (stack 1) (input (stack <stack> ...) (input <input> ...)))
;;        'error) ;; rule 15, runaway stopper for rule 1
;;      ((infix (stack 1) (input <expr> <input> ...))
;;        (infix (stack 2 <expr> 1) (input <input> ...))) ;; rule 2
;;      ((infix (stack 2 <morestack> ...) (input plus <input> ...))
;;        (infix (stack 3 plus 2 <morestack> ...) (input <input> ...))) ;; rule 3
;;      ((infix (stack 2 <morestack> ...) (input times <input> ...))
;;        (infix (stack 4 times 2 <morestack> ...) (input <input> ...))) ;; rule 4
;;      ((infix (stack 3 <morestack> ...) (input <expr> <input> ...))
;;        (infix (stack 5 <expr> 3 <morestack> ...) (input <input> ...))) ;; rule 5
;;      ((infix (stack 4 <morestack> ...) (input <expr> <input> ...))
;;        (infix (stack 6 <expr> 4 <morestack> ...) (input <input> ...))) ;; rule 6
;;      ((infix (stack 5 <morestack> ...) (input times <input> ...))
;;        (infix (stack 4 times 5 <morestack> ...) (input <input> ...))) ;; rule 7
;;      ((infix (stack 5 <expr1> <s1> plus <s2> <expr2> 1) (input plus <input> ...))
;;        (infix (stack 2 (+ <expr1> <expr2>) 1) (input plus <input> ...))) ;; rule 8
;;      ((infix (stack 5 <expr1> <s1> plus <s2> <expr2> 1) (input eof))
;;        (infix (stack 2 (+ <expr1> <expr2>) 1) (input eof))) ;; rule 9
;;      ((infix (stack 6 <expr1> <s1> times <s2> <expr2> 1) (input times <input> ...))
;;        (infix (stack 2 (* <expr1> <expr2>) 1) (input times <input> ...))) ;; rule 10
;;      ((infix (stack 6 <expr1> <s1> times <s2> <expr2> 1) (input plus <input> ...))
;;        (infix (stack 2 (* <expr1> <expr2>) 1) (input plus <input> ...))) ;; rule 11
;;      ((infix (stack 6 <expr1> <s1> times <s2> <expr2> 1) (input eof))
;;        (infix (stack 2 (* <expr1> <expr2>) 1) (input eof))) ;; rule 12
;;      ((infix (stack 6 <expr1> <s1> times <s2> <expr2> 3 <morestack> ...) (input eof))
;;        (infix (stack 5 (* <expr1> <expr2>) 3 <morestack> ...) (input eof))) ;; rule 14
;;      ((infix (stack 6 <expr1> <s1> times <s2> <expr2> 3 <morestack> ...) (input times <input> ...))
;;        (infix (stack 5 (* <expr1> <expr2>) 3 <morestack> ...) (input times <input> ...))) ;; rule 16
;;      ((infix (stack 6 <expr1> <s1> times <s2> <expr2> 3 <morestack> ...) (input plus <input> ...))
;;        (infix (stack 5 (* <expr1> <expr2>) 3 <morestack> ...) (input plus <input> ...))) ;; rule 17
;;      ((infix (stack 2 <expr> 1) (input eof))
;;        <expr>) ;; rule 13
;;      ((infix <input> ...)
;;        (infix (stack 1) (input <input> ... eof))) ;; rule 1
;;      ))
