; #lang racket
; Tail-recursive solution :
(define (sieve n)
  (define (aux u v)
    (let ((p (car v)))
      (if (> (* p p) n)
        (rev-append u v)
        (aux (cons p u)
          (wheel '() (cdr v) (* p p) p)))))
  (aux '(2)
    (range-s '() (if (odd? n) n (- n 1)))))


(define (wheel u v a p)
    (cond ((null? v) (reverse u))
                    ((= (car v) a) (wheel u (cdr v) (+ a p) p))
                    ((> (car v) a) (wheel u v (+ a p) p))
                    (else (wheel (cons (car v) u) (cdr v) a p))))

(define (rev-append u v)
    (if (null? u) v (rev-append (cdr u) (cons (car u) v))))

(define (range-s v k)
    (if (< k 3) v (range-s (cons k v) (- k 2))))

; (displayln (length (sieve 1000000)))

; (let fac ([n 10])
;     (if (zero? n)
;         1
;         (* n (fac (sub1 n)))))
; 3628800