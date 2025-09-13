(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "expected" expected "but got" actual)))

(assert-equal! (string-append) "")
(assert-equal! (string-append "foo") "foo")
(assert-equal! (string-append "foo" "bar") "foobar")
