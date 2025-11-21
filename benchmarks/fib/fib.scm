(provide fib
         ; jitfib
         jit-fib
         add-two
         loop
         jit-loop
         assoc2
         fake-car
         test-alloc-stuff
         map1
         big-list)

;; This should return an int, and should always
;; return an int - we should be able to do
;; ADDINT... Or also could do loop unrolling?
(define (fib n)
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(define (jit-fib n)
  ;; Loop unrolling would do so much, assuming we can do that easily
  (if (<= n 2) 1 (+ (jit-fib (- n 1)) (jit-fib (- n 2)))))

; (define jitfib (#%jit-compile jit-fib))
; (define jitfib jit-fib)

; (set! jit-fib (#%jit-compile jit-fib "jit-fib"))

;; Replace it on the stack
; (#%jit-compile-2 jit-fib)

(define (add-two x y)
  (split-many (string-append x y) " "))

(define (fake-car x)
  (car x))

; (#%jit-compile-2 fake-car)

; (define (add-two x)
;   (cons (car x) (cdr x)))

; (define (add-two x y)
;   (string-append x y))

(define (loop x y)
  (if (= x y) x (loop (+ x 1) y)))

(define (jit-loop x y)
  (if (= x y) x (jit-loop (+ x 1) y)))

(define (assoc2 obj lst)
  (cond
    [(null? lst) #f]
    [(equal? (car (car lst)) obj) (car lst)]
    [else (assoc2 obj (cdr lst))]))

(define (test-alloc-stuff obj)
  (stdout-simple-displayln obj)
  (unbox (box obj)))

(define big-list (map (lambda (x) (cons x x)) (range 0 1000)))

(define (map1 func accum lst)
  ; (stdout-simple-displayln accum)
  (if (null? lst) (reverse accum) (map1 func (cons (func (car lst)) accum) (cdr lst))))

; (set! map1 map1)

; (#%jit-compile-2 test-alloc-stuff)

;; TODO: Fix the inlining issue with free identifiers
; (define (loop x y)
;   (if (= x y)
;       x
;       (loop (+ x 1))))

; (#%jit-compile-2 map1)
(#%jit-compile-2 assoc2)
; (#%jit-compile-2 jit-loop)
(#%jit-compile-2 jit-fib)

; (displayln (assoc2 999 big-list))
; (displayln (assoc2 999 big-list))
; (displayln (assoc2 999 big-list))
; (displayln (assoc2 999 big-list))
; (displayln (assoc2 999 big-list))
; (displayln (assoc2 999 big-list))
; (displayln (assoc2 999 big-list))
; (displayln (assoc2 999 big-list))

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
