(require "common.scm")

; (define-syntax do
;   (syntax-rules ()
;     [(do ((var init step ...) ...) (test expr ...) command ...)
;      (letrec* ([loop
;                 (lambda (var ...)
;                   (if test
;                       (begin
;                         expr ...)
;                       (begin
;                         command ...
;                         (loop (do "step" var step ...) ...))))])
;               (loop init ...))]
;     [(do "step" x) x]
;     [(do "step" x y) y]))

; (define vector->list mutable-vector->list)
; (define vector-ref mut-vector-ref)
; (define vector-length mut-vec-len)


; (define (make-vector n)
;   (apply mutable-vector (map (lambda (x) 0) (range 0 n))))

; (define values list)
; (define (call-with-values producer consumer)
;   (define result (apply consumer (producer)))
;   (if (= (length result) 1) (car result) result))

; (define (hide r x)
;   (call-with-values (lambda () (values (vector values (lambda (x) x)) (if (< r 100) 0 1)))
;                     (lambda (v i) ((vector-ref v i) x))))

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1))) ((>= i n) result) (vector-set! result i i)))

(define (create-y x)
  (let* ([n (vector-length x)]
         [result (make-vector n)])
    (do ((i (- n 1) (- i 1))) ((< i 0) result) (vector-set! result i (vector-ref x i)))))

(define (my-try n)
  (vector-length (create-y (create-x n))))

(define (go m n)
  (let loop ([repeat m]
             [result '()])
    (if (> repeat 0) (loop (- repeat 1) (my-try n)) result)))

; (displayln (go 50 1000000))

; (displayln (go 1 500))

; 500
; 1000000
; 1000000

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "array1"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (go (hide count count) (hide count input1)))
                        (lambda (result) (equal? result output)))))


(with-input-from-file "r7rs-benchmarks/small-inputs/array1.input" run-benchmark)
