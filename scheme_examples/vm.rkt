
(define-syntax fun
  (syntax-rules (:: ->)
    [(fun name :: (arg c -> return) body ...)
     (define/contract (name arg) (->/c c return) body ...)]
    [(fun name :: (arg c -> arg1 c1 -> return) body ...)
     (define/contract (name arg arg1) (->/c c c1 return) body ...)]
    [(fun name :: (arg c -> arg1 c1 -> arg2 c2 -> return) body ...)
     (define/contract (name arg arg1 arg2) (->/c c c1 c2 return) body ...)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> return) body ...)
     (define/contract
       (name arg arg1 arg2 arg3)
       (->/c c c1 c2 c3 return) body ...)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> return) body ...)
     (define/contract
       (name arg arg1 arg2 arg3 arg4)
       (->/c c c1 c2 c3 c4 return) body ...)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> return) body ...)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5)
       (->/c c c1 c2 c3 c4 c5 return) body ...)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> arg6 c6
                       -> return) body ...)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5 arg6)
       (->/c c c1 c2 c3 c4 c5 c6 return) body ...)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> arg6 c6
                       -> arg7 c7
                       -> return) body ...)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5 arg6 arg7)
       (->/c c c1 c2 c3 c4 c5 c6 c7 return) body ...)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> arg6 c6
                       -> arg7 c7
                       -> arg8 c8
                       -> return) body ...)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
       (->/c c c1 c2 c3 c4 c5 c6 c7 c8 return) body ...)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> arg6 c6
                       -> arg7 c7
                       -> arg8 c8
                       -> arg9 c9
                       -> return) body ...)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)
       (->/c c c1 c2 c3 c4 c5 c6 c7 c8 c9 return) body ...)]))


;; New more palatable function declarations
;; similar to haskell / agda kinda stuff
; (fun add-two :: (left int? -> right int? -> int?)
;      (+ left right))

; (displayln (add-two 10 20))


; (fun boop-lemma-1 :: (l any/c -> any/c)
;      (displayln l))

; (boop-lemma-1 "alex")



(struct VmState (ip stack))


(define PUSH 0)
(define POP 1)
(define ADD 2)
(define SUB 3)

;; Just use 
(define (make-inst op-code payload)
    (list op-code payload))

(define (handle-push instructions payload state)
    (VmState (+ (VmState-ip state) 1) (cons payload (VmState-stack state))))

(fun handle-add :: (instructions vector? -> payload any/c -> state VmState? -> (or/c VmState? integer?))
    (define stack (VmState-stack state))
    ; (displayln stack) ;; this is a bug - it should print the stack and not "123"
    (define left (car stack))
    (define right (car (cdr stack)))
    (VmState (+ (VmState-ip state) 1) (cons (+ left right) (cdr (cdr stack)))))

(fun handle-sub :: (instructions vector? -> payload any/c -> state VmState? -> (or/c VmState? integer?))
    (define stack (VmState-stack state))
    (define left (car stack))
    (define right (car (cdr stack)))
    (define result (- left right))
    (VmState (+ (VmState-ip state) 1) (cons result (cdr (cdr stack)))))

(fun handle-pop :: (instructions vector? -> payload any/c -> state VmState? -> (or/c VmState? integer?))
    (car (VmState-stack state)))

(define event-loop
    ;; Index corresponds to the actual op code itself
    ;; This then dispatches to the correct case
    ;; Could also do this with a big cond but this might be better performance on every loop to just do this
    (vector 
        handle-push
        handle-pop
        handle-add
        handle-sub))

(fun dispatch :: (opcode integer? -> (->/c vector? any/c VmState? (or/c VmState? integer?)))
    (vector-ref event-loop opcode))


;; Core event loop 
(define (vm instructions state)
    (let* ((ip (VmState-ip state))
           (stack (VmState-stack state))
           (cur-inst (vector-ref instructions ip))
           (cur-op (car cur-inst)))

           (displayln cur-op)
           ;; Hand off the VM here
           (let ((output ((dispatch cur-op) instructions (car (cdr cur-inst)) state)))
                (if (equal? cur-op POP)
                    output
                    (vm instructions output)))))


(define test-instructions
    (vector
        (list PUSH 10) ;; (10)
        (list PUSH 20) ;; (20 10)
        (list ADD 0) ;; (30)
        (list PUSH 30) ;; (30 30)
        (list ADD 0) ;; (60)
        (list POP 0)))

(displayln (vm test-instructions (VmState 0 '())))


