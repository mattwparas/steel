
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

;; TODO name should not escape unless you add the datum->syntax wrapper
;; jk this is fine, because its an argument to the macr
(define-syntax define/contract
  (syntax-rules (->)
    [(define/contract (name arg args ...)
       (-> argc argcs ...)
       body ...)
     (define (name arg args ...)
       (define/contract-helper name (arg args ...) (-> argc argcs ...) body ...))]))


;; do something like
;; (contracts)

;; contract combinators are difficult
;; attach contracts to values
;; functions can become contracts
;; returned values also become contracts


;; Attach contracts to values
;; how to test higher order contracts
(define/contract (test arg1 arg2 arg3 arg4)
  (-> even? odd? even? odd? string?)
  "hello world")


(define-syntax error-out
  (syntax-rules ()
    [(error-out message) (error! message)]))

(define (blagh) (error! "testing")) (blagh)


(define (blargh) (error-out "erroring out with a message")) (blargh)


;; generates a value plus the thunk required to do contract checking on it
;; how else would it work?
;; pre - (listof contracts)
;; post - contract
;; (struct Function (pre post))


;; Want to apply the function w/ the arguments
;; Also need to do contract checking on each of the

;; (define (apply-contract function args)

;;   (define (loop contracts arg-list)
;;     (cond [(empty? arg-list) #t]
;;           [()]

;;         #f

;;         )

;;     )

;;   (define result (apply function args))
;;   (if (Function-post result)
;;       result))



;; make a new type, a contracted-function
;; contains both the function itself, and the argument checks for before and after




(test 2 3 4 5)
(test 2 3 4 5)
(test 2 3 4 5)
(test 2 3 4 5)
(test 2 3 4 5)
(test 2 3 4 5)
(test 2 3 4 5)
(test 2 3 4 5)
(test 1 2 3 4)

; (define make-account (lambda (balance) (lambda (amt) (begin (set! balance (+ balance amt)) balance))))
; (define account1 (make-account 100.00))
; (account1 -20.00)
; (account1 -20.00)

; #<void>
; #<void>
; 80.0
; 60.0
