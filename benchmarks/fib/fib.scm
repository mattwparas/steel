(provide fib)

;; This should return an int, and should always
;; return an int - we should be able to do
;; ADDINT... Or also could do loop unrolling?
(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (jit-fib n)
  ;; Loop unrolling would do so much, assuming we can do that easily
  (if (<= n 2)
      1
      (+ (jit-fib (- n 1)) (jit-fib (- n 2)))))

(set! jit-fib (#%jit-compile jit-fib "jit-fib"))

;; Take a callsite, and unroll it multiple times?
;; How that would be done; given a function definition,
;; go through and rewrite the recursive calls
;; to include the value?

;; Loop unrolling
;; Constant propagation, with some rules.
;;
;; What that looks like -> Inlining a function call?

; (fib 35)
