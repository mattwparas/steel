(provide make-require-transformer
         expand-import
         (struct-out import)
         (struct-out import-source)
         module->exports
         current-require-module-path)

(struct import (local-id src-sym src-mod-path mode req-mode orig-mode orig-stx) #:transparent)
(struct import-source (mod-path-stx mode) #:transparent)

(define (record->import record stx)
  (let ([phase (list-ref record 3)])
    (import (first record) (second record) (third record) phase phase 0 stx)))

(define (import->record i)
  (list (import-local-id i) (import-src-sym i) (import-src-mod-path i) (import-mode i)))

(define (expand-import stx)
  (let ([answer (#%spec-answer 'import stx)])
    (values (map (lambda (record) (record->import record stx)) (first answer))
            (map (lambda (source) (import-source (first source) (second source))) (second answer)))))

(define (all? pred xs)
  (null? (filter (lambda (x) (not (pred x))) xs)))

(define (make-require-transformer proc)
  (lambda (stx)
    (let* ([result (proc stx)]
           [both? (and (list? result) (= (length result) 2) (list? (car result)) (list? (cadr result)))]
           [imports (if both? (car result) result)]
           [sources (if both? (cadr result) '())])
      (unless (and (list? imports) (all? import? imports) (list? sources) (all? import-source? sources))
        (error-with-span (syntax-span stx)
                         "make-require-transformer: expected the procedure to return imports and import sources, found:"
                         result))
      (#%imports->require-spec (map import->record imports)
                               (map (lambda (source)
                                      (list (import-source-mod-path-stx source) (import-source-mode source)))
                                    sources)
                               stx))))

(define (module->exports mod)
  (let* ([exports (#%spec-answer 'exports mod)]
         [at-phase-0 (lambda (names)
                       (if (null? names) '() (list (cons 0 (map (lambda (name) (list name '())) names)))))])
    (values (at-phase-0 (first exports)) (at-phase-0 (second exports)))))

(define (current-require-module-path)
  (#%current-module-path))
