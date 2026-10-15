(require "steel/signatures")

(define (signature-of contract)
  (contract->signature contract))

;; Contracts that only say what types things are have signatures
(assert! (equal? (signature-of (->/c string? fixnum?)) '((args String) (returns Int) (raises Maybe))))
(assert! (equal? (signature-of (->/c (listof fixnum?) (hashof symbol? (or/c string? char?)) list?))
                 '((args (listof Int) (hashof Symbol (or String Char))) (returns List) (raises Maybe))))
(assert! (equal? (signature-of (->*/c (string?) #:rest (listof fixnum?) string?))
                 '((args String (rest Int)) (returns String) (raises Maybe))))
(assert! (equal? (signature-of (->*/c () #:rest list? any/c)) '((args (rest Any)) (returns Any) (raises Maybe))))
(assert! (equal? (signature-of (->/c (custom/c "app::Counter") void?))
                 '((args (custom "app::Counter")) (returns Void) (raises Maybe))))

;; Contracts that say more than that do not
(assert! (not (signature-of (->/c (>/c 0) fixnum?))))
(assert! (not (signature-of (->/c int? fixnum?))))
(assert! (not (signature-of (->/c (non-empty-listof fixnum?) fixnum?))))
(assert! (not (signature-of (->/c (and/c fixnum? (>/c 0)) fixnum?))))
(assert! (not (signature-of (->/c (->/c fixnum? fixnum?) fixnum?))))
(assert! (not (signature-of (->/c fixnum? (or/c string? symbol?)))))
(assert! (not (signature-of (->*/c () #:rest (non-empty-listof fixnum?) any/c))))
(assert! (not (signature-of (lambda (x) x))))

;; A user's own predicate is not the builtin one of the same name
(let ([string? (lambda (x) #t)])
  (assert! (not (signature-of (->/c string? any/c)))))

;; Native functions round trip through their contracts
(for-each (lambda (f)
            (let* ([signature (function-signature f)]
                   [back (contract->signature (signature->contract signature))])
              (assert! (equal? (assoc 'args back) (assoc 'args signature)))
              (assert! (equal? (assoc 'returns back) (assoc 'returns signature)))))
          (list string-length string-append car hash-insert vector-ref + substring list->string
                string->list split-many string-join bytes->list))

;; Natives can say what their lists hold
(assert! (equal? (assoc 'returns (function-signature split-many)) '(returns (listof String))))
(assert! (equal? (assoc 'args (function-signature list->string)) '(args (listof Char))))

;; A signature without argument types has no contract
(assert! (not (signature->contract '((returns Int)))))
(assert! (not (signature->contract #f)))

;; The contract made from a signature checks the function
(define checked-length
  (bind/c (signature->contract (function-signature string-length)) string-length 'checked-length))
(assert! (= (checked-length "abc") 3))
(assert! (equal? (with-handler (lambda (e) 'raised) (checked-length 'abc)) 'raised))

;; Functions with rest arguments
(define/contract (sum-all start . rest)
  (->*/c (fixnum?) #:rest (listof fixnum?) fixnum?)
  (apply + start rest))
(assert! (= (sum-all 1) 1))
(assert! (= (sum-all 1 2 3) 6))
(assert! (equal? (with-handler (lambda (e) 'raised) (sum-all 1 "x")) 'raised))
(assert! (equal? (with-handler (lambda (e) 'raised) (sum-all)) 'raised))

(define/contract (joined . parts)
  (->*/c () #:rest (listof string?) string?)
  (apply string-append parts))
(assert! (equal? (joined "a" "b") "ab"))
(assert! (equal? (joined) ""))

;; any/c is a contract
(assert! (contract? any/c))
(assert! (any/c 5))
(assert! (equal? (signature-of (->/c any/c any/c)) '((args Any) (returns Any) (raises Maybe))))
