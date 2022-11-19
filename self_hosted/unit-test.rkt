(provide
    test
    (for-syntax check-equal?)
    (for-syntax check-err)
    (for-syntax test-module))

(define-syntax check-equal?
    (syntax-rules ()
        [(check-equal? name input expected)
            (with-handler (lambda (err) (display "> ")
                                        (display name)
                                        (display " ... ")
                                        (display-color "FAILED" 'red)
                                        (newline) (displayln err))
                (test name input expected))]))

(define-syntax check-err?
    (syntax-rules ()
        [(check-equal? name input expected)
            (with-handler (lambda (err) (display "> ")
                                        (display name)
                                        (display " ... ")
                                        (display-color "Ok" 'green)
                                        (newline))
                (test name input expected))]))

;; Check the equality, and otherwise do some nice printing of the result
(define (test name input expected)
  (if (equal? input expected)
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display-color "OK" 'green)
        (newline))
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display-color "FAILED" 'red)
        (newline)
        (display "    Expected: ")
        (display expected)
        (display ", Found ")
        (displayln input))))

(define-syntax test-module
    (syntax-rules ()
        [(test-module expr ...)
            (begin expr ...)]))

(test-module
    (check-equal? "Checks that the expressions make sense" 
                  (+ 10 20 30) 
                  (+ 10 20 30))

    (check-err? "This should fail (in a good way)!" 
                (error! "Throwing an error") 
                (+ 10 20 30))

    (check-equal? "This should fail" 
                  (+ 10 20) 
                  (+ 30 40)))

