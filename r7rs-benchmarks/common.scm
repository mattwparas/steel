;;; The following code is appended to all benchmarks.

; (define current-second current-seconds)
; (define (jiffies-per-second)
;   1000)
; (define (current-jiffy)
;   (llong->flonum (current-nanoseconds)))

(require-builtin steel/time)

(provide run-r7rs-benchmark
         hide
         bench-input)

;;; Selects the input file for a benchmark based on R7RS_BENCH_SIZE.
;;;
;;;   R7RS_BENCH_SIZE=small (the default) uses small-inputs/<name>.input,
;;;   falling back to inputs/<name>.input when no reduced input exists.
;;;   R7RS_BENCH_SIZE=full always uses inputs/<name>.input.

(define (bench-input name)
  (define size (with-handler (lambda (_) "small") (env-var "R7RS_BENCH_SIZE")))
  (define small (string-append "r7rs-benchmarks/small-inputs/" name ".input"))
  (define full (string-append "r7rs-benchmarks/inputs/" name ".input"))
  (if (and (equal? size "small") (path-exists? small)) small full))

; (define values list)
; (define (call-with-values producer consumer)
;   (define result (apply consumer (producer)))
;   (if (= (length result) 1) (car result) result))

(define (this-scheme-implementation-name)
  "steel")

(define (current-jiffy)
  (current-milliseconds))

(define (jiffies-per-second)
  1000)

(define inexact exact->inexact)

;;; Given an integer and an object, returns the object
;;; without making it too easy for compilers to tell
;;; the object will be returned.

(define (hide r x)
  (call-with-values (lambda () (values (vector values (lambda (x) x)) (if (< r 100) 0 1)))
                    (lambda (v i) ((vector-ref v i) x))))

;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; and a unary predicate that is true of the
;;; correct results the thunk may return,
;;; runs the benchmark for the number of specified iterations.

(define (run-r7rs-benchmark name count thunk ok?)

  ;; Rounds to thousandths.
  (define (rounded x)
    (/ (round (* 1000 x)) 1000))

  (display "Running ")
  (display name)
  (newline)
  (flush-output-port (current-output-port))
  (let* ([j/s (jiffies-per-second)]
         [t0 (current-second)]
         [j0 (current-jiffy)])
    (let loop ([i 0]
               [result #f])
      (cond
        [(< i count) (loop (+ i 1) (thunk))]
        [(ok? result)
         (let* ([j1 (current-jiffy)]
                [t1 (current-second)]
                [jifs (- j1 j0)]
                [secs (inexact (/ jifs j/s))]
                [secs2 (rounded (- t1 t0))])
           (display "Elapsed time: ")
           (write secs)
           (display " seconds (")
           (write secs2)
           (display ") for ")
           (display name)
           (newline)
           (display "+!CSVLINE!+")
           (display (this-scheme-implementation-name))
           (display ",")
           (display name)
           (display ",")
           (display secs)
           (newline)
           (flush-output-port (current-output-port)))
         0]
        [else
         (display "ERROR: returned incorrect result: ")
         (write result)
         (newline)
         (display "+!CSVLINE!+")
         (display (this-scheme-implementation-name))
         (display ",")
         (display name)
         (display ",")
         (display "INCORRECT")
         (newline)
         (flush-output-port (current-output-port))
         0]))))
