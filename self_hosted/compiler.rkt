; #lang racket

; (require "match.rkt" (for-syntax "match.rkt"))

(require "std::option")

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

;; --------- Local Variable -----------

(define (LocalVariable name captured? struct-offset syntax-object)
    (mutable-vector 'LocalVariable name captured? struct-offset syntax-object))

(define (LocalVariable-name self) (mut-vector-ref self 1))
(define (LocalVariable-captured? self) (mut-vector-ref self 2))
(define (LocalVariable-struct-offset self) (mut-vector-ref self 3))
(define (LocalVariable-syntax-object self) (mut-vector-ref self 4))

(define (set-captured! self)
    (vector-set! 2 #t))

;; ---------- Upvalue -------------

(define (UpValue index local? ident)
    (mutable-vector 'UpValue index local? ident))

(define (UpValue-index self) (mut-vector-ref 1))
(define (UpValue-local? self) (mut-vector-ref 2))
(define (Upvalue-ident self) (mut-vector-ref 3))

;; --------- Variable Data ------------

;; Mutable struct
(define (VariableData locals enclosing)
    (mutable-vector
        ;; Name
        'VariableData
        ;; locals (mutable-vector)
        locals
        ;; upvalues
        (mutable-vector)
        ;; enclosing => default void
        enclosing))

;; Getter for locals
(define (VariableData-locals self)
    (mut-vector-ref self 1))

;; Getter for upvalues
(define (VariableData-upvalues self)
    (mut-vector-ref self 2))

;; Pointer to the enclosing variable
(define (VariableData-enclosing self)
    (mut-vector-ref self 3))

;; push a local to the VariableData struct
(define (push-local! self local)
    (vector-push! (VariableData-locals self) local))

;; Mark a local at the index as captured
(define (mark-captured! self index)
    (set-captured!    
        (mut-vector-ref
            (VariableData-locals self)
            index)))

;; --------- Code Generator ------------

(struct CodeGenerator (
    instructions
    constant-map
    defining-context
    depth
    variable-data
    let-context
    stack-offset
    top-level-define
    rooted
))

;; Push directly on to the instruction set
(define (push! self x)
    (vector-push! (CodeGenerator-instructions self) x))

;; Get the length of the underlying instruction set
(define (instructions-len self)
    (mut-vec-len (CodeGenerator-instructions self)))

;; Generate a new code generator
(define (CodeGenerator::new constant-map)
    (CodeGenerator 
        (mutable-vector)
        constant-map
        void
        0
        void
        #false
        0
        #false
        #false))

;; Generate a new code generator from body instructions
;; Explicitly for recursive calls
(define (CodeGenerator::new-from-body-instructions 
            constant-map
            instructions
            depth
            variable-data
            defining-context)
    (CodeGenerator 
        instructions 
        constant-map 
        defining-context 
        depth 
        variable-data
        #false
        0
        #false
        #false))

;; (-> CodeGenerator? int? int?)
;; Updates the payload of the instruction at index
(define (update-payload! self index new-payload)
    (let ((instructions (CodeGenerator-instructions self))
          (old-instruction (mut-vector-ref (CodeGenerator-instructions self) index)))
            (vector-set! instructions index (set-Instruction-payload-size! old-instruction new-payload))))

;; (-> CodeGenerator? (listof? expr))
(define (compile self expr)
    (cond 
        [(symbol? expr) 
         =>
            (display "Visiting symbol: ")
            (displayln expr)]
        [(integer? expr)
         => 
            (display "Visiting integer: ")
            (displayln expr)]
        [(list? expr)
         (let ((sym (car expr)))
            (cond [(equal? sym 'define)
                    =>
                        (define sidx (instructions-len self))
                        (push! self (Instruction 'SDEF 0 ""))
                        (compile self (caddr expr))
                        (define defn-body-size (- (instructions-len self) sidx))
                        (push! self (Instruction 'EDEF 0 ""))
                        (update-payload! self sidx defn-body-size)
                        (push! self (Instruction 'BIND 0 (cadr expr)))
                        (push! self (Instruction 'VOID 0 ""))
                  ]
                  ;; ('if test then else)
                  [(equal? sym 'if)
                    => 
                        (displayln "Visiting if")
                        (let ((test-expr (cadr expr))
                              (then-expr (caddr expr))
                              (else-expr (cadddr expr)))
                                ;; Load in the test condition
                                (compile self test-expr)

                                ;; Push in the if instructions
                                (push! self (Instruction 'IF (+ 2 (instructions-len self)) ""))

                                ;; save spot of the jump instruction, fill in after
                                (define idx (instructions-len self))

                                ;; Dummy value
                                (push! self (Instruction 'JMP 0 ""))

                                ;; Emit instructions for the then
                                (compile self then-expr)

                                (push! self (Instruction 'JMP 0 ""))

                                (define false-start (instructions-len self))

                                ;; emit instructions for else expression
                                (compile self else-expr)

                                (define j3 (instructions-len self))

                                (update-payload! self idx false-start)
                                (update-payload! self (- false-start 1) j3))
                  ]

                  [else (displayln "Unhandled case")]))]
        [else (displayln "Unknown case")]))


;; Write compiler that can compile this to bytecode
(define test-program '(define foo (if #true 10 20)))

(define *Compiler* (CodeGenerator::new '()))

(compile *Compiler* test-program)

(define *instructions* 
    (CodeGenerator-instructions *Compiler*))

; (displayln *instructions*)

;; (-> void)
(define (pretty-print-instructions)
    (transduce 
        *instructions* 
        (mapping struct->list) 
        (into-for-each displayln)))

; (transduce *instructions* (mapping struct->list) (into-for-each displayln))

(pretty-print-instructions)
