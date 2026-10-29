;; The top level's own macros see its redefinition of a builtin, as a module's do
(define (list . xs) 'mine)
(define-syntax uses-list (syntax-rules () [(_) (list 1 2)]))
(define-syntax (uses-list2 stx) (syntax-case stx () [(_) #'(list 1 2)]))
(assert! (equal? (uses-list) 'mine))
(assert! (equal? (uses-list2) 'mine))
;; The prelude's don't
(assert! (equal? (when #t 'ok) 'ok))
