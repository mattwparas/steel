; #lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Matthew Flatt
;;; *reset*

; (require racket/cmdline)

(provide main
         make
         check
         iterate)

(struct node (left val right) #:transparent)

;; Instead of (define-struct leaf (val)):
(define (leaf val)
  (node #f val #f))
(define (leaf? l)
  (not (node-left l)))

(define (make item d)
  ; (stdout-simple-displayln item " " d)
  (if (= d 0)
      (leaf item)
      (let ([item2 (* item 2)]
            [d2 (- d 1)])
        ; (stdout-simple-displayln item)
        (node (make (- item2 1) d2) item (make item2 d2)))))

(define (check t)
  (if (leaf? t)
      1
      (+ 1 (+ (check (node-left t)) (check (node-right t))))))

; (set! check check)
; (set! make make)
; (set! leaf leaf)
; (set! leaf? leaf?)

; (inspect check)
(#%jit-compile-2 make)
(#%jit-compile-2 check)
(#%jit-compile-2 leaf)
(#%jit-compile-2 leaf?)

; (set! make make)
; (set! check check)

(define (iterate n m d sum)
  ; (stdout-simple-displayln n " " m " " d " " sum)
  (if (equal? n m)
      sum
      (iterate (+ n 1) m d (+ sum (check (make n d))))))

; (inspect iterate)

(define (max x y)
  (if (> x y) x y))

(define (loop d end max-depth min-depth)
  (if (>= d end)
      void
      (begin
        (let ([iterations (arithmetic-shift 1 (+ (- max-depth d) min-depth))])
          (displayln iterations " trees of depth " d " check: " (iterate 0 iterations d 0)))
        (loop (+ 2 d) end max-depth min-depth))))

(define (main n)
  (let* ([min-depth 4]
         [max-depth (max (+ min-depth 2) n)])
    (let ([stretch-depth (+ max-depth 1)])
      (displayln "stretch tree of depth " stretch-depth " check: " (check (make 0 stretch-depth))))
    (let ([long-lived-tree (make 0 max-depth)])
      ; (begin
      ; (define end )

      (loop 4 (add1 max-depth) max-depth min-depth)

      ; )

      (displayln "long lived tree of depth " max-depth " check: " (check long-lived-tree)))))

(inspect iterate)

; (set! iterate iterate)

(#%jit-compile-2 iterate)
(#%jit-compile-2 max)
(#%jit-compile-2 loop)
(#%jit-compile-2 main)

; (inspect make)
; (define foo (make 0 2))
; (displayln foo)
; (displayln node)
; (displayln (check foo))

(main 12)
; (inspect make)

; (displayln (make 0 2))

; (main 3)

; (main 21)
; (main 21)

; (command-line #:args (n)
;               (main (string->number n)))
