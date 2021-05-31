;; Fancy alternative syntax to generating define/contracts
;; Might be easier to read


(define-syntax fun
  (syntax-rules (:: where)
    [(fun name :: (args ...) where contract body ...)
     (define/contract (name args ...) contract body ...)]))


(fun applesauce :: (arg arg2) where (->/c int? int? any/c)
     (+ arg arg2))
