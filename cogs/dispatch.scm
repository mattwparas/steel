; (require "contracts/contract.scm"
;          (for-syntax "contracts/contract.scm"))

(require "logging/log.scm")

; (provide apples)

;;@doc
;; Apples are a tasty fruit
; (define/c (apples x)
;   (->c even? odd?)
;   (+ x 1))

; (define/c (bananas y)
;   (->c even? odd?)
;   (+ y 1))

; (dbg! (apples 10))
; (dbg! (bananas 10))

; (dbg! (equal? (get-contract-struct apples)
;               (get-contract-struct bananas)))

; (dbg! (get-contract-struct apples))
; (dbg! (get-contract-struct bananas))


; (define (function-contract-equal? l r)
;   (and (equal? (FunctionContract-pre-conditions l)
;                (FunctionContract-pre-conditions r))
;        (equal? (FunctionContract-post-condition l)
;                (FunctionContract-post-condition r))))

; (dbg! (function-contract-equal?
;        (get-contract-struct apples)
;        (get-contract-struct bananas)))

; (dbg! (->c even? odd?))



; (define (transform-doc expr comment)
;   (if (equal? (car expr) 'define)
;       `(begin 
;           (define ,(concat-symbols '__doc- (second expr)) comment)
;           ,expr
;        )
;        (displayln "didn't work!")))


; (dbg! (transform-doc '(define x 100) "this is the value that is associated with 100"))


;;@doc
;;
;; Seeing if a comment will show up in the repl.
;; 
;; perhaps introduce some markdown:
;; 
;; ```scheme
;; (list 10 20 30 40)
;; ```
(define x 100)

;;@doc
;; Hello world!
(define y 20)

; (displayln __doc-apples)
; (displayln __doc-x)
; (displayln __doc-y)