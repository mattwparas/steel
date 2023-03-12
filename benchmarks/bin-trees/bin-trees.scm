; #lang racket/base

;;; The Computer Language Benchmarks Game
;;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Matthew Flatt
;;; *reset*

; (require racket/cmdline)

(struct node (left val right))

;; Instead of (define-struct leaf (val)):
(define (leaf val) (node #f val #f))
(define (leaf? l) (not (node-left l)))
(define (leaf-val l) node-val)

(define (make item d)
  (if (= d 0)
      (leaf item)
      (%plain-let ((item2 (* item 2))
            (d2 (- d 1)))
        (node (make (- item2 1) d2) 
              item 
              (make item2 d2)))))

(define (check t)
  (if (leaf? t)
      1
      (+ 1 (+ (check (node-left t)) 
                         (check (node-right t))))))

(define (iterate n m d sum)
  (if (equal? n m)
      sum
      (iterate (+ n 1) m d (+ sum (check (make n d))))))


(define (loop d end max-depth min-depth)
    (if (>= d end)
        void
        (begin
          (let ((iterations (arithmetic-shift 1 (+ (- max-depth d) min-depth))))
              (displayln iterations " trees of depth " d " check: " (iterate 0 iterations d 0))
              
              
              )
        (loop (+ 2 d) end max-depth min-depth))))


(define (main n)
  (let* ((min-depth 4)
         (max-depth (max (+ min-depth 2) n)))
    (let ((stretch-depth (+ max-depth 1)))
        (displayln "stretch tree of depth " stretch-depth " check: " (check (make 0 stretch-depth))))
    (%plain-let ((long-lived-tree (make 0 max-depth)))
      ; (begin
        ; (define end )

        (loop 4 (add1 max-depth) 
                max-depth min-depth)
                
                ; )

      
      (displayln "long lived tree of depth " max-depth " check: " (check long-lived-tree))
             
             )))

(main 12)
; (main 21)
; (main 21)


; (command-line #:args (n) 
;               (main (string->number n)))
