;;; TRIANGL -- Board game benchmark.

; (import (scheme base) (scheme read) (scheme write) (scheme time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (require "steel/mutable-vectors")

(define-syntax do
  (syntax-rules ()
    [(do ((var init step ...) ...) (test expr ...) command ...)
     (letrec* ([loop
                (lambda (var ...)
                  (if test
                      (begin
                        expr ...)
                      (begin
                        command ...
                        (loop (do "step" var step ...) ...))))])
              (loop init ...))]
    [(do "step" x) x]
    [(do "step" x y) y]))

(define (list->vector lst)
  (apply mutable-vector lst))

; (define list->vector list->mutable-vector)
(define vector->list mutable-vector->list)
(define vector-ref mut-vector-ref)
; (define vector-set! mutable-vector-set!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *board* (list->vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence* (list->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

; (displayln *sequence*)

(define *a*
  (list->vector
   '(1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6 6)))

(define *b*
  (list->vector '(2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 5)))

(define *c*
  (list->vector
   '(4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6 1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())

(define (attempt i depth)
  (cond
    [(= depth 14)
     (set! *answer* (cons (cdr (vector->list *sequence*)) *answer*))
     #t]
    [(and (= 1 (vector-ref *board* (vector-ref *a* i)))
          (= 1 (vector-ref *board* (vector-ref *b* i)))
          (= 0 (vector-ref *board* (vector-ref *c* i))))
     (vector-set! *board* (vector-ref *a* i) 0)
     (vector-set! *board* (vector-ref *b* i) 0)
     (vector-set! *board* (vector-ref *c* i) 1)
     (vector-set! *sequence* depth i)
     (do ((j 0 (+ j 1)) (depth (+ depth 1))) ((or (= j 36) (attempt j depth)) #f))
     (vector-set! *board* (vector-ref *a* i) 1)
     (vector-set! *board* (vector-ref *b* i) 1)
     (vector-set! *board* (vector-ref *c* i) 0)
     #f]
    [else #f]))

(define (test i depth)
  (set! *answer* '())
  (attempt i depth)
  (car *answer*))

; 50
; 22
; 1
; (22 34 31 15 7 1 20 17 25 6 5 13 32)

; (displayln (test 22 1))

(let loop ([i 0])
  ; (when (< i 1000000)
  (when (< i 50)
    (begin
      ; (displayln i)
      ; (assert! (equal? (list (vector 4 1 3 2) (vector 0 5 7 6)) (test 740.0)))
      (displayln (test 22 1))

      (loop (+ i 1)))))

; (define (run-benchmark)
;   (let* ((count (read))
;          (input1 (read))
;          (input2 (read))
;          (output (read))
;          (s3 (number->string count))
;          (s2 (number->string input2))
;          (s1 (number->string input1))
;          (name "triangl"))
;     (run-r7rs-benchmark
;      (string-append name ":" s1 ":" s2 ":" s3)
;      count
;      (lambda () (test (hide count input1) (hide count input2)))
;      (lambda (result) (equal? result output)))))
