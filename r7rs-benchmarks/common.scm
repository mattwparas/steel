;;; The following code is appended to all benchmarks.

; (define current-second current-seconds)
; (define (jiffies-per-second)
;   1000)
; (define (current-jiffy)
;   (llong->flonum (current-nanoseconds)))

(require-builtin steel/time)

(define values list)
(define (call-with-values producer consumer)
  (define result (apply consumer (producer)))
  (if (= (length result) 1) (car result) result))

(define (this-scheme-implementation-name)
  "steel")

(define (current-jiffy)
  (current-milliseconds))

(define (jiffies-per-second)
  1000)

(define (current-second)
  (* 0.001 (current-inexact-milliseconds)))

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
  (let* ([j/s (jiffies-per-second)] [t0 (current-second)] [j0 (current-jiffy)])
    (let loop ([i 0] [result #f])
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
         (flush-output-port (current-output-port))
         0]))))
