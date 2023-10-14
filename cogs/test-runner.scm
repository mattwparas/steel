(require "fs/fs.scm")

(define shared-engine (Engine::new))

(run! shared-engine "(set-test-mode!)")

;; TODO: Run each test in its own engine, extract the results
(define (require-file path)
  (when (ends-with? path ".scm")
    (displayln "LOADING --> " path)
    (displayln (run! shared-engine (list (list 'require path))))))

(define (get-directory-from-args)
  (if (empty? std::env::args) "." (car std::env::args)))

(walk-files (get-directory-from-args) require-file)

(define test-stats
  (~> (run! shared-engine '((require "steel/tests/unit-test.scm") (get-test-stats)))
      (Ok->value)
      (last)))

(when (not (empty? (hash-get test-stats 'failures)))
  (error! "There were test failures!"))
