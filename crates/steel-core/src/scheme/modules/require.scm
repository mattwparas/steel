(require (for-syntax "steel/require-transform"))

(provide matching-identifiers-in
         subtract-in
         filtered-in
         path-up
         multi-in)

(define-syntax matching-identifiers-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ regexp spec)
        (let-values ([(imports sources) (expand-import #'spec)])
          (values (filter (lambda (import)
                            (regexp-match? (syntax-e #'regexp)
                                           (symbol->string (syntax-e (import-local-id import)))))
                          imports)
                  sources))]))))

(define-syntax subtract-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ spec subtracted ...)
        (let-values ([(imports sources) (expand-import #'spec)])
          (let ([removed (apply append
                                (map (lambda (spec)
                                       (let-values ([(imports sources) (expand-import spec)])
                                         (map (lambda (import) (syntax-e (import-local-id import)))
                                              imports)))
                                     (syntax->list #'(subtracted ...))))])
            (values (filter (lambda (import)
                              (not (member (syntax-e (import-local-id import)) removed)))
                            imports)
                    sources)))]))))

(define-syntax filtered-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ proc spec)
        (let ([rename (eval (syntax->datum #'proc))])
          (let-values ([(imports sources) (expand-import #'spec)])
            (values (filter (lambda (x) x)
                            (map (lambda (i)
                                   (let* ([local (import-local-id i)]
                                          [name (rename (symbol->string (syntax-e local)))])
                                     (and name
                                          (import (datum->syntax local (string->symbol name))
                                                  (import-src-sym i)
                                                  (import-src-mod-path i)
                                                  (import-mode i)
                                                  (import-req-mode i)
                                                  (import-orig-mode i)
                                                  (import-orig-stx i)))))
                                 imports))
                    sources)))]))))

(define-syntax (path-up stx)
  (syntax-case stx ()
    [(_ relative ...)
     (cons '#%require-specs
           (map (lambda (relative)
                  (let ([found (#%path-up (syntax-e relative))])
                    (unless found
                      (error-with-span (syntax-span relative) "path-up: can't find" (syntax-e relative)))
                    (datum->syntax relative found)))
                (syntax->list #'(relative ...))))]))

(define-syntax (multi-in stx)
  (define (alternatives sub)
    (let ([e (syntax-e sub)])
      (map (lambda (x)
             (let ([x (if (syntax? x) (syntax-e x) x)])
               (if (symbol? x) (symbol->string x) x)))
           (if (list? e) e (list sub)))))
  (define (paths subs)
    (if (null? subs)
        (list "")
        (apply append
               (map (lambda (first)
                      (map (lambda (rest) (if (equal? rest "") first (string-append first "/" rest)))
                           (paths (cdr subs))))
                    (alternatives (car subs))))))
  (syntax-case stx ()
    [(_ sub ...)
     (cons '#%require-specs
           (map (lambda (path) (datum->syntax stx path)) (paths (syntax->list #'(sub ...)))))]))
