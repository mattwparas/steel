(require "tests/unit-test.scm"
         (for-syntax "tests/unit-test.scm"))

(set-test-mode!)

(check-equal? "addition" 8 ((lambda (x) (+ x x)) 4))

(check-equal? "Variable arity function call" '(3 4 5 6) ((lambda x x) 3 4 5 6))

(check-equal? "Rest arguments" '(5 6) ((lambda (x y . z) z) 3 4 5 6))

(check-equal? "Branching with >" 'yes (if (> 3 2) 'yes 'no))

(check-equal? "Branch with <" 'no (if (> 2 3) 'yes 'no))

(check-equal? "Numeric operations with if" 1 (if (> 3 2) (- 3 2) (+ 3 2)))

(check-equal? "Cond with >"
              'greater
              (cond
                [(> 3 2) 'greater]
                [(< 3 2) 'less]))

(check-equal? "Cond with equal"
              'equal
              (cond
                [(> 3 3) 'greater]
                [(< 3 3) 'less]
                [else 'equal]))

(check-equal? "Case macro"
              'composite
              (case (* 2 3)
                [(2 3 5 7) 'prime]
                [(1 4 6 8 9) 'composite]))
