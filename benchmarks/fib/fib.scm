(provide fib)

;; This should return an int, and should always
;; return an int - we should be able to do
;; ADDINT... Or also could do loop unrolling?
(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

; (fib 41)

(define (fib2 n)
  (if (<= n 2)
      1
      (+ (if (<= n 3)
             1
             (+ (fib2 (- n 2)) (fib2 (- n 3))))
         (if (<= n 4)
             1
             (+ (fib2 (- n 3)) (fib2 (- n 4)))))))

;; Take a callsite, and unroll it multiple times?
;; How that would be done; given a function definition,
;; go through and rewrite the recursive calls
;; to include the value?

;; Loop unrolling
;; Constant propagation, with some rules.
;;
;; What that looks like -> Inlining a function call?
