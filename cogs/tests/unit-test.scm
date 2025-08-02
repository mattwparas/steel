(require "steel/colors/colors.scm")

; (provide test
;          (for-syntax check-equal?)
;          (for-syntax check-err?)
;          (for-syntax test-module)
;          get-test-stats
;          (for-syntax check)
;          (for-syntax skip-compile))

(provide test
         check-equal?
         check-err?
         test-module
         get-test-stats
         ;; TODO: We should do some validation to say this is a free identifier?
         ; check
         skip-compile)

(define *SUCCESS-COUNT* 0)
(define *FAILURE-COUNT* 0)

(define *FAILED-TO-COMPILE* 0)

;; Failed tests
(define *failures* '())

(define (mark-success)
  (set! *SUCCESS-COUNT* (+ *SUCCESS-COUNT* 1)))

(define (mark-failed name)
  (set! *FAILURE-COUNT* (+ *FAILURE-COUNT* 1))
  (set! *failures* (cons name *failures*)))

(define (mark-skipped)
  (set! *FAILED-TO-COMPILE* (+ *FAILED-TO-COMPILE* 1)))

(define (print-success name)
  (display "test > ")
  (display name)
  (display " ... ")
  (displayln-color "Ok" #:fg 'green))

(define (print-failure name)
  (display "test > ")
  (display name)
  (display " ... ")
  (displayln-color "FAILED" #:fg 'red))

(define-syntax check-equal?
  (syntax-rules ()
    [(check-equal? 'skip rest ...) (mark-skipped)]
    [(check-equal? name input expected)
     (with-handler (lambda (err)
                     (mark-failed name)
                     (print-failure name)
                     (displayln err))
                   (test name input expected))]))

(define-syntax check-err?
  (syntax-rules ()
    [(check-err? name input expected)
     (with-handler (lambda (err)
                     (mark-success)
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
        (displayln "    Expected: " expected ", Found " input))))

(define-syntax test-module
  (syntax-rules ()
    [(test-module name expr ...)
     (when (get-test-mode)
       (begin
         (displayln "###### Running tests for module " name " ######")
         (begin
           expr ...)
         (displayln "Test result: " *SUCCESS-COUNT* " passed; " *FAILURE-COUNT* " failed;")
         (display "Failures: ")
         (displayln *failures*)))]
    [(test-module expr ...)
     (begin
       expr ...)]))

(define-syntax skip-compile
  (syntax-rules ()
    [(skip-compile) (begin)]
    [(skip-compile expr) (mark-skipped)]
    [(skip-compile expr exprs ...)
     (begin
       (mark-skipped)
       (skip-compile exprs ...))]))

(define (get-test-stats)
  (hash 'success-count
        *SUCCESS-COUNT*
        'failure-count
        *FAILURE-COUNT*
        'failures
        *failures*
        'failed-to-compile
        *FAILED-TO-COMPILE*))

; (test-module
; (check-equal? "Checks that the expressions make sense"
;               (+ 10 20 30)
;               (+ 10 20 30))

; (check-err? "This should fail (in a good way)!"
;             (error! "Throwing an error")
;             (+ 10 20 30))

; (check-equal? "This should fail"
;               (+ 10 20)
;               (+ 30 40))

;   )

; (check-equal? "test" (begin (error! "hello world") 10) 10)
