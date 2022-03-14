(define program '(

    (define-syntax when
        (syntax-rules ()
            [(when a b ...)
                (if a (begin b ...) void)]))

    (define-syntax cond
        (syntax-rules (else =>)
            [(cond [else => e1 ...])
                (begin e1 ...)]
            [(cond [else e1 ...])
                (begin e1 ...)]
            [(cond [e1 e2 ...])
                (when e1 e2 ...)]
            [(cond [e1 => e2 ...] c1 ...)
                (if e1
                    (begin e2 ...)
                    (cond c1 ...))]
            [(cond [e1 e2 ...] c1 ...)
                (if e1
                    (begin e2 ...)
                    (cond c1 ...))]))
    
    (define (foo x) 
        (cond [(equal? x 10) => 27]
              [else => 100]))
))

;; Yoink out the macros, do any reader expansions
;; Then move on with the program, returning whats left
(expand! program) ;; => '((define foo (lambda (x) ...)))


;; TODO: add reader macros passes to this as well
;; Marks primitives with contracts as well...
;; Multi arity functions with contracts probably doesn't work very well
;; Core macros can be pre processed and cached in the binary

;; Add handle to the current context?
;; Eval should also be possible... It would need to be compiled down to
;; Bytecode directly inside the interpreter
;; Do this by exposing the symbol map and make a new instance of the compiler
;; This requires re architecting the current implementation
