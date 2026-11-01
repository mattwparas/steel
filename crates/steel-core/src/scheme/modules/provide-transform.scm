(provide make-provide-transformer
         expand-export
         (struct-out export)
         make-provide-pre-transformer
         pre-expand-export)

(struct export (local-id out-sym mode protect? orig-stx) #:transparent)

(define (expand-export stx modes)
  (map (lambda (pair) (export (first pair) (second pair) 0 #f stx)) (#%spec-answer 'export stx)))

(define (make-provide-transformer proc)
  (lambda (stx)
    (syntax/loc (cons '#%provide-specs
                      (map (lambda (export)
                             (if (equal? (syntax-e (export-local-id export)) (export-out-sym export))
                                 (export-local-id export)
                                 (list '#%provide-as (export-local-id export) (export-out-sym export))))
                           (proc stx '(0))))
      (syntax-span stx))))

(define (make-provide-pre-transformer proc)
  (lambda (stx) (proc stx '(0))))

(define (pre-expand-export stx modes)
  stx)
