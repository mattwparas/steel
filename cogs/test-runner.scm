(define shared-engine (Engine::new))

(run! shared-engine "(set-test-mode!)")
(run! shared-engine '((require "transducers/transducers.scm" "contracts/contract-test.scm")))
; (run! shared-engine '((require "contracts/contract-test.scm")))

(define test-stats 
    (~> (run! shared-engine '((require "steel/tests/unit-test.scm") (get-test-stats)))
        (Ok->value)
        (last)))

(when (not (empty? (hash-get test-stats 'failures)))
    (error! "There were test failures!"))