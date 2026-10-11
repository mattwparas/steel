;; Struct types that share a name are still different types, and a struct
;; that goes through serialization in one process comes back as its own.

(define (round-trip value)
  (deserialize-value (bytes->serialized (serialized->bytes (serialize-value value)))))

;; Each call defines a new type called q
(define (local n)
  (struct q (a))
  (list (q n) q? q-a))

(define first-type (local 1))
(define second-type (local 2))

(define (instance of)
  (car of))
(define (is? of value)
  ((cadr of) value))
(define (field of value)
  ((caddr of) value))

(assert! (not (is? first-type (instance second-type))))

(let ([back (round-trip (instance first-type))])
  (assert! (is? first-type back))
  (assert! (not (is? second-type back)))
  (assert! (= 1 (field first-type back))))

(let ([back (round-trip (instance second-type))])
  (assert! (is? second-type back))
  (assert! (not (is? first-type back)))
  (assert! (= 2 (field second-type back))))

;; A type the expander's own engine defines too
(define setting (make-parameter 5))
(assert! (= 5 ((round-trip setting))))
(assert! (= 5 (thread-join! (spawn-native-thread (lambda () (setting))))))

;; Many values that each bring a symbol this engine has not seen cost the
;; same each, not more as they add up.
(define (send-symbols from to)
  (when (< from to)
    (round-trip (string->symbol (string-append "fresh-symbol-" (number->string from))))
    (send-symbols (+ from 1) to)))
(send-symbols 0 20000)
