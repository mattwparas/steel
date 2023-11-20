(define (stream-cdr stream)
  ((#%stream-cdr stream)))

(define (integers n)
  (stream-cons n (lambda () (integers (+ 1 n)))))

(define (in-range-stream n m)
  (if (= n m) empty-stream (stream-cons n (lambda () (in-range-stream (add1 n) m)))))

(define (append-streams s1 s2)
  (cond
    [(stream-empty? s1) s2]
    [(stream-empty? s2) s1]
    [else (stream-cons (stream-car s1) (lambda () (append-streams (stream-cdr s1) s2)))]))

(define (add-streams s1 s2)
  (let ([h1 (stream-car s1)] [h2 (stream-car s2)])
    (stream-cons (+ h1 h2) (lambda () (add-streams (stream-cdr s1) (stream-cdr s2))))))

(define (merge-streams s1 s2)
  (cond
    [(stream-empty? s1) s2] ; nothing to merge from s1
    [(stream-empty? s2) s1] ; nothing to merge from s2
    [else
     (let ([h1 (stream-car s1)] [h2 (stream-car s2)])
       (stream-cons
        h1
        (lambda () (stream-cons h2 (lambda () (merge-streams (stream-cdr s1) (stream-cdr s2)))))))]))

(define (map-stream func s)
  (cond
    [(stream-empty? s) s]
    [else (stream-cons (func (stream-car s)) (lambda () (map-stream func (stream-cdr s))))]))

(define (list->stream lst)
  (if (null? lst) empty-stream (stream-cons (car lst) (lambda () (list->stream (cdr lst))))))

(define (stream->list s)
  (define (*stream->list s lst)
    (if (stream-empty? s) lst (*stream->list (stream-cdr s) (cons (stream-car s) lst))))
  (*stream->list s '()))

(define (stream-section n stream)
  (cond
    [(= n 0) '()]
    [else (cons (stream-car stream) (stream-section (- n 1) (stream-cdr stream)))]))

;; execute / transducer work with transducers
;; these are like source agnostic

(transduce (add-streams (integers 0) (integers 0)) (taking 15) (into-list))

(transduce (merge-streams (integers 0) (integers 0)) (taking 15) (into-list))

(transduce (merge-streams (merge-streams (integers 0) (integers 0))
                          (add-streams (integers 0) (integers 0)))
           (taking 15)
           (into-list))

(transduce (map-stream (lambda (x) 10) (integers 0)) (taking 15) (into-list))

(stream->list (in-range-stream 0 10))

;; make a stream out of a port
(define my-port (open-input-file "scheme_examples/dfs.rkt"))

(define (port-stream)
  (let ([head (read-line-from-port my-port)])
    (if (equal? 'eof head) empty-stream (stream-cons head (lambda () (port-stream))))))

;; Make a stream out of a port
;; Access the port using the given func
;; Stop reading w/ the given end-sym
(define (port->stream p func end-sym)
  (let ([head (func p)])
    (if (equal? end-sym head)
        empty-stream
        (stream-cons head (lambda () (port->stream p func end-sym))))))

(transduce (port-stream my-port read-line-from-port 'eof) (taking 15) (into-list))
