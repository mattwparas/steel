;;; WC -- One of the Kernighan and Van Wyk benchmarks.
;;; Rewritten by Will Clinger into more idiomatic (and correct!) Scheme.

(require "common.scm")

(define (wcport port)
  (define (loop nl nw nc inword?)
    (let ([x (read-char port)])
      (cond
        [(eof-object? x) (list nl nw nc)]
        [(char=? x #\space) (loop nl nw (+ nc 1) #f)]
        [(char=? x #\newline) (loop (+ nl 1) nw (+ nc 1) #f)]
        [else
         (loop nl
               (if inword?
                   nw
                   (+ nw 1))
               (+ nc 1)
               #t)])))
  (loop 0 0 0 #f))

; (define (loop nl nw nc inword? port)
;   (let ([x (read-char port)])
;     (cond
;       [(eof-object? x) (list nl nw nc)]
;       [(char=? x #\space) (loop nl nw (+ nc 1) #f port)]
;       [(char=? x #\newline) (loop (+ nl 1) nw (+ nc 1) #f port)]
;       [else
;        (loop nl
;              (if inword?
;                  nw
;                  (+ nw 1))
;              (+ nc 1)
;              #t
;              port)])))

; (define (wcport port)
;   (loop 0 0 0 #f port))

(define (go x)
  (call-with-input-file x wcport))

(define (run-benchmark)
  (let* ([count (read)]
         [input (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 input]
         [name "wc"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (go (hide count input)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/small-inputs/wc.input" run-benchmark)
