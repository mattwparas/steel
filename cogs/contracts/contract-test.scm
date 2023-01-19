(require "steel/tests/unit-test.scm" 
         (for-syntax "steel/tests/unit-test.scm") 
         "contract.scm"
         (for-syntax "contract.scm"))

(provide foo)

(define/c (foo x y)
    (->c even? odd? odd?)
    (+ x y))


(test-module "check-basic-contract-checking"
    (check-equal? "basic contract" (foo 10 21) 31)
    (check-err? "Should raise a contract violation" (foo 11 10) 21)

    (check-equal? "applesauce" 10 20)

    ; (check-err? "Should raise a contract violation" (foo 11 10) 21)
    
    ; (check-err? "Should raise a contract violation" (foo 11 10) 21)
    )