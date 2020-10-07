
(define-syntax define/contract-helper
  (syntax-rules (->)
    [(define/contract-helper name ()
       (-> argc)
       body ...)
     (begin
       (define res ((lambda () body ...)))
       (unless (argc res)
         (error! (symbol->string 'name)
                 ":"
                 "contract violation on result:"
                 res
                 "violated the contract:"
                 (symbol->string 'argc)))
       res)]
    [(define/contract-helper name
       (arg args ...)
       (-> argc argcs ...)
       body ...)
     (begin
       (unless (argc arg)
         (error! (symbol->string 'name)
                 ":"
                 "contract violation:"
                 arg
                 "violated the contract:"
                 (symbol->string 'argc)))
       (define/contract-helper name (args ...) (-> argcs ...) body ...))]))

(define-syntax define/contract
  (syntax-rules (->)
    [(define/contract (name arg args ...)
       (-> argc argcs ...)
       body ...)
     (define (name arg args ...)
       (define/contract-helper name (arg args ...) (-> argc argcs ...) body ...))]))




(define/contract (test arg1 arg2 arg3 arg4)
  (-> even? odd? even? odd? number?)
  (+ arg1 arg2 arg3 arg4))



(test 1 2 3 4)