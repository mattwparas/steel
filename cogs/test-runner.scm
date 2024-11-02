(require "steel/fs/fs.scm")

(define shared-engine (Engine::new))

(run! shared-engine "(set-test-mode!)")

;; If the path contains a cog file, respect it
; (define (parse-cog-file path)
;   (define contents (let ([file (open-input-file path)]) (read-port-to-string file)))
;   (transduce (read! contents) (mapping cdr) (into-hashmap)))

(define (read-file-to-string path)
  (let ([file (open-input-file path)]) (read-port-to-string file)))

(define (expression-contains-provide expr-list)
  (contains? (λ (expr)
               (cond
                 [(and (list? expr) (not (empty? expr)))
                  (cond
                    [(equal? 'provide (car expr)) #t]
                    [(equal? 'begin (car expr)) (expression-contains-provide (cdr expr))]
                    [else #f])]
                 [else #f]))
             expr-list))

;; Open file, read it
(define (path-contains-provide path)
  (~> (read-file-to-string path) read! expression-contains-provide))

(define (require-file path)

  (displayln "Parsing: " path)

  (when (and (ends-with? path ".scm") (path-contains-provide path))

    (displayln "Loading: " path)

    ;; First parse the file and check that it provides something
    (let ([result (run! shared-engine (list (list 'require path)))])

      (when (Err? result)
        (error "Error when trying to require:" path result)))))

(define (get-directory-from-args)
  (if (empty? std::env::args) "." (car std::env::args)))

(walk-files (get-directory-from-args) require-file)

(define test-stats
  (~> (run! shared-engine '((require "steel/tests/unit-test.scm") (get-test-stats)))
      (Ok->value)
      (last)))

(when (not (empty? (hash-get test-stats 'failures)))
  (error! "There were test failures!"))
