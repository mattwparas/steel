(provide make-require-transformer
         expand-import
         (struct-out import)
         (struct-out import-source)
         module->exports
         current-require-module-path)

(struct import (local-id src-sym src-mod-path mode req-mode orig-mode orig-stx) #:transparent)
(struct import-source (mod-path-stx mode) #:transparent)

(define (record->import record)
  (import (first record) (second record) (third record) (list-ref record 3) 0 (list-ref record 3) (first record)))

(define (import->record import)
  (list (import-local-id import) (import-src-sym import) (import-src-mod-path import) (import-mode import)))

(define (sources-of imports)
  (let loop ([imports imports]
             [sources '()])
    (if (null? imports)
        (reverse sources)
        (let* ([path (import-src-mod-path (car imports))]
               [mode (import-mode (car imports))]
               [seen? (lambda (source)
                        (and (equal? (syntax-e (import-source-mod-path-stx source)) (syntax-e path))
                             (equal? (import-source-mode source) mode)))])
          (loop (cdr imports)
                (if (null? (filter seen? sources)) (cons (import-source path mode) sources) sources))))))

(define (expand-import stx)
  (let ([imports (map record->import (#%spec-answer 'import stx))])
    (values imports (sources-of imports))))

(define (make-require-transformer proc)
  (lambda (stx)
    (let* ([result (proc stx)]
           [both? (and (list? result) (= (length result) 2) (list? (car result)) (list? (cadr result)))]
           [imports (if both? (car result) result)]
           [sources (if both? (cadr result) '())])
      (#%imports->require-spec (map import->record imports)
                               (map (lambda (source)
                                      (list (import-source-mod-path-stx source) (import-source-mode source)))
                                    sources)
                               stx))))

(define (module->exports mod)
  (let* ([exports (#%spec-answer 'exports mod)]
         [at-phase-0 (lambda (names) (list (cons 0 (map (lambda (name) (list name '())) names))))])
    (values (at-phase-0 (first exports)) (at-phase-0 (second exports)))))

(define (current-require-module-path)
  (#%current-module-path))
