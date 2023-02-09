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

(test-module "check-basic-contract-checking"
    (check-equal? "basic contract" (foo 10 21) 31)
    (check-err? "Should raise a contract violation" (foo 11 10) 21)

    (check-equal? "higher order contract works"
        (simple-higher-order 11 (lambda (x) (+ x 1)))
        12)

    ; (check-err? "Should raise a contract violation" (foo 11 10) 21)
    
    ; (check-err? "Should raise a contract violation" (foo 11 10) 21)
)