; #lang racket
; Tail-recursive solution :
(define (sieve n)
  ;; Transform this function since it does not escape - should be possible to pass the argument in
  (define (aux u v)
    ; (displayln v)
    (let ((p (car v)))
      (if (> (* p p) n)
        (rev-append u v)
        (aux (cons p u)
          (wheel '() (cdr v) (* p p) p)))))
  (aux '(2)
    (range-s '() (if (odd? n) n (- n 1)))))


;; TODO: Looks like the lambda lifter doesn't work on its own
;; passing in aux explicitly causes issues because aux 
; (define sieve 
;   (lambda (n) 
;     ((lambda (aux) 
;       ((lambda (#####aux0) 
;         (begin (set! aux #####aux0) 
;                (aux (quote (2)) 
;                     (range-s (quote ()) (if (odd? n) n (- n 1))))))
      
;         (lambda (u v) (##-##lambda-lifter-sieve1 (car v) n aux u v))))
;     123)))

; (define (sieve n)
;   ;; Transform this function since it does not escape - should be possible to pass the argument in
;   (define (aux u v n)
;     ; (displayln v)
;     (let ((p (car v)))
;       (if (> (* p p) n)
;         (rev-append u v)
;         (aux (cons p u)
;           (wheel '() (cdr v) (* p p) p)
;           n
;           ))))
;   (aux '(2)
;     (range-s '() (if (odd? n) n (- n 1)))
;     n
;     ))


(define (wheel u v a p)
    (cond ((null? v) (reverse u))
          ((= (car v) a) (wheel u (cdr v) (+ a p) p))
          ((> (car v) a) (wheel u v (+ a p) p))
          (else (wheel (cons (car v) u) (cdr v) a p))))

(define (rev-append u v)
    (if (null? u) v (rev-append (cdr u) (cons (car u) v))))

(define (range-s v k)
    (if (< k 3) v (range-s (cons k v) (- k 2))))

(sieve 10)