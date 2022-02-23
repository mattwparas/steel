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

(define *op-code-set* (apply hashset op-codes))