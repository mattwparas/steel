;; A global that is set! is kept in a box. One that holds what can't be sent
;; is found by name on the other side, like one that isn't set!.
(define (round-trip value)
  (deserialize-value (bytes->serialized (serialized->bytes (serialize-value value)))))

;; read keeps its reader in a global it sets.
(define (read-one text)
  (read (open-input-string text)))
(assert! (equal? ((round-trip (lambda () (read-one "(1 2)")))) '(1 2)))

(define port (open-input-string "first"))
(set! port (open-input-string "second"))
(define (port-text)
  (read-port-to-string port))
(assert! (equal? ((round-trip (lambda () (port-text)))) "second"))

;; A set! global that can be sent is still sent.
(define count 1)
(set! count 2)
(assert! (equal? ((round-trip (lambda () count))) 2))
