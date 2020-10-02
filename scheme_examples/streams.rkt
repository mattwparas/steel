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



(execute (taking 15)
         (add-streams (integers 0) (integers 0)))

(execute (taking 15)
         (merge-streams (integers 0) (integers 0)))
