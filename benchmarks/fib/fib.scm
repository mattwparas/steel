(provide fib
         ; jitfib
         jit-fib
         add-two
         loop)

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

; (define jitfib (#%jit-compile jit-fib))
; (define jitfib jit-fib)

; (set! jit-fib (#%jit-compile jit-fib "jit-fib"))

;; Replace it on the stack
; (#%jit-compile-2 jit-fib)

(define (add-two x y)
  (split-many (string-append x y) " "))

; (define (add-two x)
;   (cons (car x) (cdr x)))

; (define (add-two x y)
;   (string-append x y))

(define (loop x)
  (if (= x 100)
      x
      (loop (+ x 1))))

; (#%jit-compile-2 loop)

; (add-two 10)

; (define jitfib jit-fib)

;; Take a callsite, and unroll it multiple times?
;; How that would be done; given a function definition,
;; go through and rewrite the recursive calls
;; to include the value?

;; Loop unrolling
;; Constant propagation, with some rules.
;;
;; What that looks like -> Inlining a function call?

; (fib 35)
