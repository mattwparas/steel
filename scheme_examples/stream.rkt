


(define (force promise) (promise))

;; syntax
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda ()
        expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)
(define (tail stream) (force (car (cdr stream))))
(define empty-stream? null?)
(define the-empty-stream '())

(define (integers n)
  (cons-stream n (integers (+ 1 n))))

(define (stream-section n stream)
  (cond ((= n 0) '())
        (else
          (cons
            (head stream)
            (stream-section
             (- n 1)
             (tail stream))))))

(define (add-streams s1 s2)
 (let ((h1 (head s1))
       (h2 (head s2)))
   (cons-stream
     (+ h1 h2)
     (add-streams (tail s1) (tail s2)))))

(define fibs
  (cons-stream 0
    (cons-stream 1
      (add-streams (tail fibs) fibs))))

(define (integers n)
  (stream-cons n (lambda () (integers (+ 1 n)))))

(define (in-range-stream n m)
  (if (= n m)
      empty-stream
      (stream-cons n (lambda () (in-range-stream (add1 n) m)))))

(define (append-streams s1 s2)
  (cond
    [(empty-stream? s1) s2]
    [(empty-stream? s2) s1]
    [else
     (stream-cons (stream-car s1)
                  (lambda () (append-streams (stream-cdr s1) s2)))]))


; (stream-section 15 fibs)

;; TODO add macro to assist in the wrapping of stream-cons similar to how cons-stream functions
