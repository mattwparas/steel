(require (for-syntax "steel/provide-transform"))

(provide filtered-out
         matching-identifiers-out)

(define-syntax matching-identifiers-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ regexp spec)
        (filter (lambda (export)
                  (regexp-match? (syntax-e #'regexp) (symbol->string (export-out-sym export))))
                (expand-export #'spec modes))]))))

(define-syntax filtered-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ proc spec)
        (let ([rename (#%spec-answer 'eval #'proc)])
          (filter (lambda (x) x)
                  (map (lambda (e)
                         (let ([name (rename (symbol->string (export-out-sym e)))])
                           (unless (or (not name) (string? name))
                             (error-with-span (syntax-span #'proc)
                                              "filtered-out: expected the procedure to return a string or #f, found:"
                                              name))
                           (and name
                                (export (export-local-id e)
                                        (string->symbol name)
                                        (export-mode e)
                                        (export-protect? e)
                                        (export-orig-stx e)))))
                       (expand-export #'spec modes))))]))))
