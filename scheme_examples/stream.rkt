


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

;; (define ones (cons-stream 1 ones))

;; (stream-section 7 ones)

;; (define (integers-starting-from n)
;;  (cons-stream n
;;   (integers-starting-from (+ n 1))))

;; (define nat-nums
;;   (integers-starting-from 1))

;; (stream-section 10 nat-nums)

;; (define nat-nums
;;  (cons-stream 1
;;   (add-streams ones nat-nums)))

;; (stream-section 10 nat-nums)

(define fibs
  (cons-stream 0
    (cons-stream 1
      (add-streams (tail fibs) fibs))))

(stream-section 10 fibs)
