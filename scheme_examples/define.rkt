;; Fancy alternative syntax to generating define/contracts
;; Might be easier to read

(define-syntax fun
  (syntax-rules (:: ->)
    [(fun name :: (arg c -> return) body)
     (define/contract (name arg) (->/c c return) body)]
    [(fun name :: (arg c -> arg1 c1 -> return) body)
     (define/contract (name arg arg1) (->/c c c1 return) body)]
    [(fun name :: (arg c -> arg1 c1 -> arg2 c2 -> return) body)
     (define/contract (name arg arg1 arg2) (->/c c c1 c2 return) body)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> return) body)
     (define/contract
       (name arg arg1 arg2 arg3)
       (->/c c c1 c2 c3 return) body)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> return) body)
     (define/contract
       (name arg arg1 arg2 arg3 arg4)
       (->/c c c1 c2 c3 c4 return) body)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> return) body)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5)
       (->/c c c1 c2 c3 c4 c5 return) body)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> arg6 c6
                       -> return) body)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5 arg6)
       (->/c c c1 c2 c3 c4 c5 c6 return) body)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> arg6 c6
                       -> arg7 c7
                       -> return) body)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5 arg6 arg7)
       (->/c c c1 c2 c3 c4 c5 c6 c7 return) body)]
    [(fun name :: (arg c
                       -> arg1 c1
                       -> arg2 c2
                       -> arg3 c3
                       -> arg4 c4
                       -> arg5 c5
                       -> arg6 c6
                       -> arg7 c7
                       -> return) body)
     (define/contract
       (name arg arg1 arg2 arg3 arg4 arg5 arg6 arg7)
       (->/c c c1 c2 c3 c4 c5 c6 c7 return) body)]))


(fun add-two :: (left int? -> right int? -> int?)
     (+ left right))

(displayln (add-two 10 20))
