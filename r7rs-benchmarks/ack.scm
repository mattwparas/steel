(define values list)
(define (call-with-values producer consumer)
  (define result (apply consumer (producer)))
  (if (= (length result) 1) (car result) result))

(define (hide r x)
  (call-with-values (lambda () (values (vector values (lambda (x) x)) (if (< r 100) 0 1)))
                    (lambda (v i) ((vector-ref v i) x))))

(define (ack m n)
  (cond
    [(= m 0) (+ n 1)]
    [(= n 0) (ack (- m 1) 1)]
    [else (ack (- m 1) (ack m (- n 1)))]))

; 2
; 3
; 12
; 32765

(define count 2)

(let loop ([i 0])
  ; (when (< i 1000000)
  (when (< i count)
    (begin
      (equal? (ack (hide count 3) (hide count 12)) 32765)

      (loop (+ i 1)))))

; (define (run-benchmark)
;   (let* ((count (read))
;          (input1 (read))
;          (input2 (read))
;          (output (read))
;          (s3 (number->string count))
;          (s2 (number->string input2))
;          (s1 (number->string input1))
;          (name "ack"))
;     (run-r7rs-benchmark
;      (string-append name ":" s1 ":" s2 ":" s3)
;      count
;      (lambda () (ack (hide count input1) (hide count input2)))
;      (lambda (result) (= result output)))))
