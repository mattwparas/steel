; #lang racket

; (require "match.rkt" (for-syntax "match.rkt"))

; (require "std::option")

;; ---------------- Vendored std::option ----------------
;; -- TODO -> fix bug with importing option
;; Looks like theres an issue where None gets bound to void for some reason
;; Most likely having to do with declaring nested structs

(struct Some (value))
(struct None ())

;; Contracts for option
(define (Option/c pred)
    (make/c (fn (x) 
                (cond [(Some? x) (pred (Some-value x))]
                      [(None? x) #t]
                      [else #f])) 
            'Option/c))

(define (Option? value)
    (or (Some? value) (None? value)))

;; Map - explore dynamic dispatch with contracts?
(define (map-option option func)
    (cond [(Some? option) (Some (func (Some-value option)))]
          [(None? option) (None)]))

(define (flatten-option option)
    (if (Some? option)
        (if (Some? (Some-value option))
            (Some-value option)
            (None))
        None))

;; Get the inner value of the option - contract checking along the way
;; Figure out how to turn off contracts on OptLevel3 regardless - 
;; this would speed up performance a lot - also figure out how to map this to compile time options in Rust
(define (unwrap-some option)
    (Some-value option))

;; Unwraps the given option or returns the given other value
(define (unwrap-or option other)
    (if (Some? option) (Some-value option) other))


;; -----------------------------------------------------------


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
        ;; enclosing => default (None)
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

;; Get the position (from the back) of the value that satisfies `func` 
(define (position-from-back vec func)
    ;; Get the last index of the vector
    (define last-index (- (mut-vec-len vec) 1))
    (when (< last-index 0) 
          (return! (None)))
    (define (looper vec idx)
        (cond 
            ;; If we're outside of the range of the vector, return None
            [(< idx 0) => (None)]
            [(func (mut-vector-ref vec idx)) => (Some idx)]
            [else => (looper vec (- idx 1))]))
    (looper vec last-index))

;; Get the position (from the front) of the value that satisfies `func`
(define (position-from-front vec func)
    (define vec-length (mut-vec-len vec))
    (define (loop vec idx)
        (cond
            [(= idx vec-length) => (None)]
            [(func (mut-vector-ref vec idx)) => (Some (mut-vector-ref vec idx))]
            [else => (loop vec (+ idx 1))]))
    (loop vec 0))

;; Get the first value from the back that satisfies the position
(define (find-from-back vec func)
    ;; Get the last index of the vector
    (define last-index (- (mut-vec-len vec) 1))
    (when (< last-index 0) 
          (return! (None)))
    (define (looper vec idx)
        (cond 
            ;; If we're outside of the range of the vector, return None
            [(< idx 0) => (None)]
            [(func (mut-vector-ref vec idx)) => (Some (mut-vector-ref vec idx))]
            [else => (looper vec (- idx 1))]))
    (looper vec last-index))



; (-> VariableData? string? Option?)
(define (resolve-local self ident)
    (let ((idx (map-option
                    (position-from-back 
                        (VariableData-locals self) 
                        (lambda (x) (equal? ident (LocalVariable-name x))))
                    (lambda (x) (- (mut-vec-len (VariableData-locals self)) 1 x))))
          (var (find-from-back (VariableData-locals self))))

          ;; Just bail out if these are Nones
          (when (None? idx) (return! idx))
          (when (None? var) (return! var))

          ;; return the sum of the index and the offset
          (Some (+ (unwrap-some idx) (LocalVariable-struct-offset (unwrap-some var))))))

(define (add-upvalue self index local? ident)
    (define i (position-from-front 
                    (VariableData-upvalues self)
                    (lambda (x) (and
                                    (equal? (UpValue-index x) index)
                                    (equal? (UpValue-local? x) local?)))))
    (cond 
        [(Some? i) => (unwrap-some i)]
        [else => 
            (-> self
                (VariableData-upvalues)
                (vector-push! (UpValue index local? ident)))
            
            (mut-vec-len (VariableData-upvalues self))]))

;; TODO
;; this behavior doesn't match yet
(define (resolve-upvalue self ident)
    (when (None? (VariableData-enclosing self))
          (return! (None)))

    (define local 
        (-> self
            (VariableData-enclosing)
            (map-option (lambda (x) (resolve-local x ident)))
            (flatten-option)))

    (cond [(Some? local)
          => (let ((var-offset (-> self
                                    (VariableData-enclosing) ;; Option<VariableData>
                                    (unwrap-some) ;; VariableData
                                    (VariableData-locals) ;; Vec<LocalVariable>
                                    (find-from-back (lambda (x) (equal? (LocalVariable-name x) ident))) ;; Option<LocalVariable>
                                    (map-option LocalVariable-struct-offset) ;; Option<Int>
                                    (unwrap-or 0))))

                ;; Mark this captured in the parent
                (-> self
                    (VariableData-enclosing)
                    (unwrap-some)
                    (mark-captured! (- (unwrap-some local) var-offset)))

                (Some (add-upvalue (unwrap-some local) #true ident)))]
          [else 
            => (let ((upvalue (-> self 
                                (VariableData-enclosing) 
                                (map-option (lambda (x) (resolve-upvalue x ident)))
                                (flatten-option))))
                    (if (Some? upvalue)
                        (Some (add-upvalue (unwrap-some upvalue) #false ident))
                        (None)))]))

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

;; TODO -> have this share the same body as the above
(define (update-contents! self index new-contents)
    (let ((instructions (CodeGenerator-instructions self))
          (old-instruction (mut-vector-ref (CodeGenerator-instructions self) index)))
            (vector-set! instructions index (set-Instruction-contents! old-instruction new-contents))))


;; TODO - implement collecting locals from the lambda function
;; should also verify that there are no duplicate function arguments
(define (collect-locals-from-function lambda-function)
    (let ((locals (cadr lambda-function)))
        (when (not (equal? (length locals) (hashset-length (list->hashset locals))))
            (error! "Bad Syntax: lambda function cannot have duplicate argument names: " lambda-function))

        (apply 
            mutable-vector
            (map 
                (lambda (x) (LocalVariable (symbol->string x) #false x 0))
                (reverse locals)))))

(define (add-or-get constant-map value)
    (let ((idx (position-from-front constant-map (lambda (x) (equal? x value)))))
        (cond [(Some? idx) => (unwrap-some idx)]
              [else 
                => 
                    (let ((idx (mut-vec-len constant-map)))
                        (vector-push! constant-map value)
                        idx)])))




(define (visit-lambda-function self expr)
    (displayln "Visiting lambda")

    (let ((idx (instructions-len self)))
        (push! self (Instruction 'SCLOSURE 0 ""))

        ;; Detect if this is a multi arity function somehow 
        (push! self (Instruction 'PASS 0 ""))

        ;; If we're a let context and not rooted, insert this instruction
        ;; not sure exactly why its necessary, but we're just matching the existing
        ;; behavior
        (push! self (Instruction 'PASS 
                            (if (and 
                                    (CodeGenerator-let-context self)
                                    (not (CodeGenerator-rooted self))) 
                                1 
                                0)
                                ""))

        (let* ((variable-data (VariableData 
                                (collect-locals-from-function expr) 
                                (CodeGenerator-variable-data self)))
              (child-generator (CodeGenerator::new-from-body-instructions
                                    (CodeGenerator-constant-map self)
                                    (mutable-vector)
                                    (+ 1 (CodeGenerator-depth self))
                                    (Some variable-data)
                                    (if (or (CodeGenerator-top-level-define self) 
                                            (CodeGenerator-let-context self))
                                        (CodeGenerator-defining-context self)
                                        (None)))))
            
            ;; Compile the body of the lambda function
            (compile child-generator (caddr expr))

            (push! self 
                (Instruction 'NDEFS 
                            (mut-vec-len (VariableData-upvalues variable-data)) ""))
            
            ;; Fill out the upvalue information
            (transduce (VariableData-upvalues variable-data)
                        (into-for-each 
                            (lambda (upvalue)
                                (if (UpValue-local? upvalue)
                                    (push! self 
                                        (Instruction 
                                            'FILLLOCALUPVALUE 
                                            (UpValue-index upvalue) ""))

                                    (push! self
                                        (Instruction
                                            'FILLUPVALUE
                                            (UpValue-index upvalue) ""))))))

            (push! self (Instruction 'POP (mut-vec-len (VariableData-locals variable-data)) ""))

            ;; TODO -> defining-context tail call stuff

            (transduce (VariableData-locals variable-data)
                        (into-for-each
                            (lambda (local)
                                (push! self 
                                    (Instruction
                                        'CLOSEUPVALUE
                                        (if (LocalVariable-captured? local) 1 0)
                                        (LocalVariable-syntax-object local))))))

            ;; TODO -> convert last usages here

            (vector-append! 
                (CodeGenerator-instructions self)
                    (CodeGenerator-instructions child-generator))

            (let ((closure-body-size (- (instructions-len self) idx))
                  (arity (length (cadr expr))))

                (push! self (Instruction 'ECLOSURE arity ""))

                ;; Store the body size
                (update-payload! self idx closure-body-size)))))

(define (constant? expr)
    (or
        (boolean? expr)
        (number? expr)
        (string? expr)
        (char? expr)))

(define (specialize-constant self expr)
    (push! self 
            (Instruction 
                'PUSHCONST
                (add-or-get (CodeGenerator-constant-map self) expr)
                expr)))

(define (visit-atom self expr)
    (let ((idx (-> self
                   (CodeGenerator-variable-data)
                   (resolve-local expr)
                   (flatten-option))))
        (cond [(Some? idx) 
                => 
                    (push! self (Instruction 'READLOCAL (unwrap-some idx) expr))]
              [else =>
                (let ((idx (-> self
                               (CodeGenerator-variable-data)
                               (resolve-upvalue)
                               (flatten-option))))
                    (cond [(Some? idx)
                            => 
                                (push! self (Instruction 'READUPVALUE (unwrap-some idx) expr))]
                          [else 
                            => 
                                (push! self (Instruction 'PUSH 0 expr))]))])))


(define (visit-define self expr)
    (let* ((sidx (instructions-len self))
           (defining-context (cadr expr))
           (adjusted-self (-> self
                            (set-CodeGenerator-defining-context! defining-context)
                            (set-CodeGenerator-top-level-define! #true))))
        
        ;; Start the definition
        (push! self (Instruction 'SDEF 0 ""))

        ;; Update the contents to now point to this new context
        (update-contents! self sidx defining-context)

        ;; Compile the body of this define
        (compile self (caddr expr))

        (let ((defn-body-size (- (instructions-len self) sidx)))
            (push! self (Instruction 'EDEF 0 ""))
            (update-payload! self sidx defn-body-size)
            (push! self (Instruction 'BIND 0 (cadr expr)))
            (push! self (Instruction 'VOID 0 "")))))

(define (visit-if self expr)
    (displayln "Visiting if")
    (let ((test-expr (cadr expr))
          (then-expr (caddr expr))
          (else-expr (cadddr expr)))
        ;; Load in the test condition
        (compile self test-expr)
        ;; Push in the if instructions
        (push! self (Instruction 'IF (+ 2 (instructions-len self)) ""))
        ;; save spot of the jump instruction, fill in after
        (let ((idx (instructions-len self)))
            ;; Dummy value
            (push! self (Instruction 'JMP 0 ""))
            ;; Emit instructions for the then
            (compile self then-expr)
            (push! self (Instruction 'JMP 0 ""))
            (let ((false-start (instructions-len self)))
                ;; emit instructions for else expression
                (compile self else-expr)
                (let ((j3 (instructions-len self)))
                    (update-payload! self idx false-start)
                    (update-payload! self (- false-start 1) j3))))))

(define (visit-list self expr)
    (displayln "Visiting function application!"))

;; (-> CodeGenerator? (listof? expr))
(define (compile self expr)
    (cond 
        [(symbol? expr) => (visit-atom self expr)]
        [(constant? expr) => (specialize-constant self expr)]
        [(list? expr)
         (let ((sym (car expr)))
            (cond [(equal? sym 'define) => (visit-define self expr)]
                  ;; ('if test then else)
                  [(equal? sym 'if) => (visit-if self expr)]
                  [(equal? sym 'lambda) => (visit-lambda-function self expr)]
                  [else => (visit-list self expr)]))]
        [else (displayln "Unknown case")]))


;; Write compiler that can compile this to bytecode
; (define test-program '(define foo (if #true 10 20)))

(define test-program 
    '(define foo (lambda (x) (+ x x 10))))

(define *Constant-map* (mutable-vector))
(define *Compiler* (CodeGenerator::new *Constant-map*))

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


; (displayln (collect-locals-from-function '(lambda (x y z) 10)))
; (displayln (collect-locals-from-function '(lambda (x x x) 10)))
