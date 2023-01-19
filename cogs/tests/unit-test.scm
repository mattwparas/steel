(provide
    test
    (for-syntax check-equal?)
    (for-syntax check-err?)
    (for-syntax test-module)
    get-test-stats)

(define *SUCCESS-COUNT* 0)
(define *FAILURE-COUNT* 0)

;; Failed tests
(define *failures* '())

(define (mark-success)
    (set! *SUCCESS-COUNT* (+ *SUCCESS-COUNT* 1)))

(define (mark-failed name)
    (set! *FAILURE-COUNT* (+ *FAILURE-COUNT* 1))
    (set! *failures* (cons name *failures*)))

(define (print-success name) 
    (display "test > ")
    (display name)
    (display " ... ")
    (display-color "Ok" 'green)
    (newline))

(define (print-failure name)
    (display "test > ")
    (display name)
    (display " ... ")
    (display-color "FAILED" 'red)
    (newline))

(define-syntax check-equal?
    (syntax-rules ()
        [(check-equal? name input expected)
            (with-handler (lambda (err) (mark-failed name)
                                        (print-failure name) 
                                        (displayln err))
                (test name input expected))]))

(define-syntax check-err?
    (syntax-rules ()
        [(check-err? name input expected)
            (with-handler (lambda (err) (mark-success)
                                        (print-success name))
                (test name input expected))]))

;; Check the equality, and otherwise do some nice printing of the result
(define (test name input expected)
  (if (equal? input expected)
      (begin
        (mark-success)
        (print-success name))
      (begin
        (mark-failed name)
        (print-failure name)
        (display "    Expected: ")
        (display expected)
        (display ", Found ")
        (displayln input))))

(define-syntax test-module
    (syntax-rules ()
        [(test-module name expr ...)
            (when (get-test-mode)
                (begin 
                    (display "###### Running tests for module ") (display name) (displayln " ######")
                    (begin expr ...)
                    (display "Test result: ") (display *SUCCESS-COUNT*) (display " passed; ") (display *FAILURE-COUNT*) (displayln " failed;")
                    (display "Failures: ") (displayln *failures*)
                    ))]
        [(test-module expr ...)
            (begin expr ...)]))

(define (get-test-stats) (hash
    'success-count *SUCCESS-COUNT*
    'failure-count *FAILURE-COUNT*
    'failures *failures*
))

; (test-module
;     (check-equal? "Checks that the expressions make sense" 
;                   (+ 10 20 30) 
;                   (+ 10 20 30))

;     (check-err? "This should fail (in a good way)!" 
;                 (error! "Throwing an error") 
;                 (+ 10 20 30))

;     (check-equal? "This should fail" 
;                   (+ 10 20) 
;                   (+ 30 40)))

