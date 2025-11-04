;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test of R6RS get-datum, comparable to the parsing benchmark.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "common.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; get-datum benchmark comparable to the parsing benchmark.
;;
;; Reads nboyer.sch into a string before timing begins.
;;
;; The timed portion of the benchmark parses the string
;; representation of nboyer.sch 1000 times.
;;
;; The output of that parse is checked by comparing it
;; the the value returned by the read procedure.
;;
;; Usage:
;;     (read-benchmark n input)
;;     (read-from-string-port-benchmark n input)
;;
;;
;; n defaults to 1000, and input defaults to "nboyer.sch".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-from-file-benchmark input)
  (call-with-input-file input
                        (lambda (in)
                          (do ((x (read in) (read in)) (y #f x) (i 0 (+ i 1))) ((eof-object? x) y)))))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 input1]
         [name "read1"])
    (run-r7rs-benchmark (string-append name ":" s2)
                        count
                        (lambda () (read-from-file-benchmark (hide count input1)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/read1.input" run-benchmark)
