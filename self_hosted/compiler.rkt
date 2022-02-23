; #lang racket

;; Add support for mutable structs
(struct Instruction (op-code payload-size contents))

;; Add support for mutable vectors
(define *op-codes*
    '(
        VOID
        PUSH
        LOOKUP
        IF
        JMP
        FUNC
        SCLOSURE
        ECLOSURE
        STRUCT
        POP
        BIND
        SDEF
        EDEF
        PASS
        PUSHCONST
        NDEFS
        EVAL
        PANIC
        CLEAR
        TAILCALL
        SET
        READ
        METALOOKUP
        CALLCC
        READLOCAL
        SETLOCAL
        READUPVALUE
        SETUPVALUE
        FILLUPVALUE
        FILLLOCALUPVALUE
        CLOSEUPVALUE ;; Should be 1 for close, 0 for not
        TCOJMP
        CALLGLOBAL
        CALLGLOBALTAIL
        LOADINT0 ;; Load const 0
        LOADINT1
        LOADINT2
        CGLOCALCONST
        INNERSTRUCT
        MOVEREADLOCAL
        MOVEREADUPVALUE
        MOVECGLOCALCONST
        BEGINSCOPE
        ENDSCOPE
))

(define *op-code-set* (apply hashset *op-codes*))

;; Write compiler that can compile this to bytecode
(define test-program '(define foo 10))

;; Our resulting instruction set
;; For now, just refer to the global
(define *instructions* (mutable-vector))

(define (push! x) (vector-push! *instructions* x))

; (vector-push! *instructions* 1)
; (vector-push! *instructions* 2)
; (vector-push! *instructions* 3)

; (displayln *instructions*)

(define (black-box x) x)

(define (compile expr)
    (cond 
        [(symbol? expr) 
         (display "Visiting symbol: ")
         (displayln expr)]
        [(list? expr)
         (let ((sym (car expr)))
            (cond [(equal? sym 'define)
                    (begin
                        (push! (Instruction 'SDEF 0 ""))
                        (compile (cadr expr))
                        (push! (Instruction 'EDEF 0 ""))
                        (push! (Instruction 'BIND 0 (cadr expr)))
                        (push! (Instruction 'VOID 0 "")))]
                  [else (displayln "Unknown instruction")]))]
        [else (displayln "Unknown case")]))

(compile test-program)

; (displayln *instructions*)

(transduce *instructions* (mapping struct->list) (into-for-each displayln))
