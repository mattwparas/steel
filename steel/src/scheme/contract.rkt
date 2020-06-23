(export define/contract define/contract-helper)

(define-syntax define/contract-helper
  (syntax-rules (->)
    [(define/contract-helper name 
       ()
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
