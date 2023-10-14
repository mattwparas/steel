(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm")
         "contract.scm"
         (for-syntax "contract.scm"))

(provide foo)

(define/c (foo x y)
  (->c even? odd? odd?)
  (+ x y))

(define/c (simple-higher-order x func)
  (->c odd? (->c odd? even?) even?)
  (func x))

(define (any? x)
  (displayln "***** CHECKING ANY? *****")
  #true)

(define (int-checker? x)
  (displayln "***** CHECKING INT? ******")
  (int? x))
(define (number-checker? x)
  (displayln "***** CHECKING NUMBER? ******")
  (number? x))

(define level1
  (bind-contract-to-function
   (make-function-contract (make-function-contract (FlatContract number-checker? 'number-checker?)))
   (lambda ()
     (lambda ()
       (displayln "@@@@@@@@@@ CALLING FUNCTION @@@@@@@@@@@")
       10))
   'level1))

(define level2
  (bind-contract-to-function
   (make-function-contract (make-function-contract (FlatContract int-checker? 'int-checker)))
   (lambda () (level1))
   'level2))

(define level3
  (bind-contract-to-function (make-function-contract (make-function-contract (FlatContract any?
                                                                                           'any?)))
                             (lambda () (level2))
                             'level3))

(test-module
 "check-basic-contract-checking"
 (check-equal? "basic contract" (foo 10 21) 31)
 (check-err? "Should raise a contract violation" (foo 11 10) 21)
 (check-equal? "higher order contract works" (simple-higher-order 11 (lambda (x) (+ x 1))) 12)
 (check-equal? "Multiple levels of contracts apply" ((level3)) 10)
 ; (check-err? "Should raise a contract violation" (foo 11 10) 21)
 ; (check-err? "Should raise a contract violation" (foo 11 10) 21)
 )
