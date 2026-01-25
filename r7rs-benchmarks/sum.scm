(require "common.scm")

;; TODO: Look into loop unrolling? Inlining?
;; Otherwise, we'll want to look into
;; better type inference / understanding
;; how to avoid the dispatching cost.
(define (run n)
  ;; If the value is not a reference type, can we avoid
  ;; spilling it to the stack? i.e. if its a known
  ;; boolean / or number type, with only one usage, should
  ;; we just avoid spilling to the stack and instead
  ;; keep everything as numbers?
  (let loop ([i n]
             [sum 0])
    (if (< i 0)
        sum
        (loop (- i 1) (+ i sum)))))

;; If _everything_ just became args for the duration
;; of this function, meaning:
;;
;; Block args are the inputs:
;; and then we just run a hot loop doing addition with
;; the values returning, then we could avoid the stack
;; refs? How is it possible _that_ much faster?
;
; Possible optimizations to make:
; Inline addition and subtraction. Let vars that are guaranteed to be
; numbers perhaps don't need to result in calls?
;
; Inline as many calls as possible.
; less than should probably be inlined as well. Less than 0 is a quick
; operation.
;
; Basically, all of these things can involve no calls since its just
; numeric operations that can be pushed down to hand written generated
; code, especially for add 1 or things like that.

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "sum"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (run (hide count input1)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/sum.input" run-benchmark)
