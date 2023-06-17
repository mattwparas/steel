; (require "dispatch.scm")

; (apples 10)

;; Lookup multiple keys at a time
(define (href table keys .)
    (foldl (lambda (key t) (hash-get t key)) table keys))