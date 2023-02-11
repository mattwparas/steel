(require "contracts/contract.scm"
         (for-syntax "contracts/contract.scm"))

(define/c (apples x)
  (->c even? odd?)
  (+ x 1))

(define/c (bananas y)
  (->c even? odd?)
  (+ y 1))

(dbg! (apples 10))
(dbg! (bananas 10))

(dbg! (equal? (get-contract-struct apples)
              (get-contract-struct bananas)))

(dbg! (get-contract-struct apples))
(dbg! (get-contract-struct bananas))


(define (function-contract-equal? l r)
  (and (equal? (FunctionContract-pre-conditions l)
               (FunctionContract-pre-conditions r))
       (equal? (FunctionContract-post-condition l)
               (FunctionContract-post-condition r))))

(dbg! (function-contract-equal?
       (get-contract-struct apples)
       (get-contract-struct bananas)))

(dbg! (->c even? odd?))
