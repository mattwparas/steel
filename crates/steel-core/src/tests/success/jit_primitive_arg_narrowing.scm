;; The jit narrows a local passed to a primitive to the argument type the
;; primitive declares (`args(..)`), once the call has returned normally.
;; Loops so the functions get compiled, then uses the narrowed locals.

;; After (length lst), lst is a list, so use car / cdr / null? on it.
(define (sum-after-length lst)
  (let ([n (length lst)])
    (let loop ([l lst] [acc n])
      (if (null? l) acc (loop (cdr l) (+ acc (car l)))))))

;; After (string-length s), s is a string.
(define (last-char s)
  (let ([n (string-length s)])
    (if (= n 0) #\space (string-ref s (- n 1)))))

;; After (abs x), x is a number.
(define (twice-abs x)
  (let ([a (abs x)])
    (+ a (* x x))))

;; A local reassigned after the call, so the narrowing must not outlive it.
(define (reassigned lst)
  (let ([x lst])
    (length x)
    (set! x 42)
    (+ x 1)))

;; A different type through the same code on another call.
(define (maybe-length v)
  (if (list? v) (length v) (string-length v)))

(define (run n acc)
  (if (= n 0)
      acc
      (run (- n 1)
           (list (sum-after-length (list 1 2 3))
                 (last-char "hello")
                 (twice-abs -3)
                 (reassigned (list 1 2))
                 (maybe-length (list 1 2 3))
                 (maybe-length "abcd")))))

(define result (run 2000 '()))
(assert! (equal? result (list 9 #\o 12 43 3 4)))
