(require "fs/fs.scm")

(define shared-engine (Engine::new))

(run! shared-engine "(set-test-mode!)")

(define (require-file path)
    (when (ends-with? path ".scm")
        (run! shared-engine (list (list 'require path)))))

(walk-files "." require-file)


(define test-stats 
    (~> (run! shared-engine '((require "steel/tests/unit-test.scm") (get-test-stats)))
        (Ok->value)
        (last)))

(when (not (empty? (hash-get test-stats 'failures)))
    (error! "There were test failures!"))