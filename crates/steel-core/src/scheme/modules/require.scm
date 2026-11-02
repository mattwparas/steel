(require (for-syntax "steel/require-transform"))

(provide matching-identifiers-in
         subtract-in
         filtered-in
         path-up
         multi-in
         values-in
         macros-in
         glob-in
         optional-require)

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
                                       (let-values ([(imports _) (expand-import spec)])
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
        (let ([rename (#%spec-answer 'eval #'proc)])
          (let-values ([(imports sources) (expand-import #'spec)])
            (values (filter (lambda (x) x)
                            (map (lambda (i)
                                   (let* ([local (import-local-id i)]
                                          [name (rename (symbol->string (syntax-e local)))])
                                     (unless (or (not name) (string? name))
                                       (error-with-span (syntax-span #'proc)
                                                        "filtered-in: expected the procedure to return a string or #f, found:"
                                                        name))
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
                  (unless (string? (syntax-e relative))
                    (error-with-span (syntax-span relative) "path-up expects a string path, found:" (syntax->datum relative)))
                  (let ([found (#%path-up (syntax-e relative))])
                    (unless found
                      (error-with-span (syntax-span relative) "path-up: can't find" (syntax-e relative)))
                    (datum->syntax relative found)))
                (syntax->list #'(relative ...))))]))

(define-syntax (multi-in stx)
  (define (all? pred xs)
    (null? (filter (lambda (x) (not (pred x))) xs)))
  (define (parts sub)
    (let ([e (syntax-e sub)])
      (when (and (list? e) (null? e))
        (error-with-span (syntax-span sub) "multi-in expects a path or a list of paths, found:" e))
      (if (list? e) e (list sub))))
  (define (alternatives sub)
    (map (lambda (x)
           (let ([x (syntax-e x)])
             (if (symbol? x) (symbol->string x) x)))
         (parts sub)))
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
     (let* ([subs (syntax->list #'(sub ...))]
            [all (apply append (map parts subs))])
       (when (null? subs)
         (error-with-span (syntax-span stx) "multi-in expects at least one path:" (syntax->datum stx)))
       (unless (or (all? (lambda (x) (string? (syntax-e x))) all)
                   (all? (lambda (x) (symbol? (syntax-e x))) all))
         (error-with-span (syntax-span stx)
                          "multi-in expects all strings or all identifiers:"
                          (syntax->datum stx)))
       (cons '#%require-specs (map (lambda (path) (datum->syntax stx path)) (paths subs))))]))

(begin-for-syntax
  (define (exported-names exports)
    (if (null? exports) '() (map car (cdr (car exports)))))
  (define (imports-of-kind stx kind)
    (syntax-case stx ()
      [(_ spec)
       (let-values ([(imports sources) (expand-import #'spec)])
         (let ([names (map (lambda (source)
                             (let-values ([(variables syntax)
                                           (module->exports (import-source-mod-path-stx source))])
                               (cons (syntax-e (import-source-mod-path-stx source))
                                     (exported-names (if (equal? kind 'variable) variables syntax)))))
                           sources)])
           (values (filter (lambda (i)
                             (member (import-src-sym i)
                                     (cdr (assoc (syntax-e (import-src-mod-path i)) names))))
                           imports)
                   sources)))])))

(define-syntax values-in
  (make-require-transformer (lambda (stx) (imports-of-kind stx 'variable))))

(define-syntax macros-in
  (make-require-transformer (lambda (stx) (imports-of-kind stx 'syntax))))

(define-syntax (glob-in stx)
  (syntax-case stx ()
    [(_ pattern ...)
     (cons '#%require-specs
           (apply append
                  (map (lambda (pattern)
                         (map (lambda (path) (datum->syntax pattern path))
                              (#%glob (syntax-e pattern))))
                       (syntax->list #'(pattern ...)))))]))

(define-syntax (optional-require stx)
  (syntax-case stx ()
    [(_ path entry ...)
     (let ([entries (map (lambda (entry)
                           (if (list? (syntax-e entry))
                               (syntax->list entry)
                               (list entry #f)))
                         (syntax->list #'(entry ...)))])
       (cond
         [(not (#%module-exists? (syntax-e #'path)))
          (if (null? entries)
              'void
              (cons 'begin (map (lambda (entry) (list 'define (first entry) (second entry))) entries)))]
         [else
          (let ([head (syntax/loc 'require (syntax-span stx))])
            (if (null? entries)
                (list head #'path)
                (list head (cons (syntax/loc 'only-in (syntax-span stx)) (cons #'path (map first entries))))))]))]))
