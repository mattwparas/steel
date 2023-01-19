(define shared-engine (Engine::new))

(run! shared-engine "(set-test-mode!)")
(run! shared-engine '((require "transducers/transducers.scm" "contracts/contract-test.scm")))
; (run! shared-engine '((require "contracts/contract-test.scm")))

(run! shared-engine '((require "steel/tests/unit-test.scm") (displayln (get-test-stats))))